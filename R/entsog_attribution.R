process_iterative <- function(flows_for_whole_period) {
  process_with_date <- function(df) {
    process_iterative_for_day(df) %>%
      mutate(date = unique(df$date))
  }

  # Distribute to source country using our iterative method
  flows_sourced <- pbapply::pblapply(
    split(flows_for_whole_period, flows_for_whole_period$date), process_with_date
  ) %>%
    do.call(bind_rows, .)

  return(flows_sourced)
}

# Approach: iterative + net flows
process_iterative_for_day <- function(flows_for_day) {
  options(dplyr.summarise.inform = FALSE)

  # Assert expected columns
  expected_columns <- c("from_country", "to_country", "value", "date", "from_method", "to_method")
  if (!all(expected_columns %in% names(flows_for_day))) {
    stop(glue("Expected columns {columns} not found"))
  }

  if (("date" %in% names(flows_for_day)) && (length(unique(flows_for_day$date)) > 1)) {
    stop("This function should only work on one date at a time.
       Please aggregate if need be.")
  }

  date <- unique(flows_for_day$date)

  log_info("Processing flows for date {date}")

  flows_for_day <- flows_for_day %>%
    mutate(
      from = to_matrix_key(from_country, from_method),
      to = to_matrix_key(to_country, to_method)
    ) %>%
    select(from, to, value)

  # The flow_matrix contains the net imports for each country to each
  # other country. The row of a matrix is the importer, the column of the
  # matrix is the exporter. For example, flow_matrix["BG", "RU"] would give
  # you the value of gas imported from Russia into Bulgaria).
  flow_matrix <- build_flow_matrix(flows_for_day)

  distributed_flows_for_day <- flow_matrix %>%
    distribute_until_stable() %>%
    verify_matrix(original = flow_matrix) %>%
    remove_production() %>%
    convert_to_data_frame() %>%
    mutate(
      date = date,
      from_values = purrr::map(from, from_matrix_key),
      to_values = purrr::map(to, from_matrix_key),
      from_country = purrr::map_chr(from_values, "country"),
      from_method = purrr::map_chr(from_values, "method"),
      to_country = purrr::map_chr(to_values, "country"),
      to_method = purrr::map_chr(to_values, "method")
    ) %>%
    select(from_country, to_country, from_method, to_method, value)
  return(distributed_flows_for_day)
}

convert_to_data_frame <- function(flow_matrix) {
  all_countries <- dimnames(flow_matrix)[[1]]

  return(
    bind_cols(country = sort(all_countries), as.data.frame(flow_matrix)) %>%
      gather(key = "from", value = "value", -country) %>%
      rename(to = country)
  )
}

remove_production <- function(flow_matrix) {
  diag(flow_matrix) <- 0
  return(flow_matrix)
}

build_flow_matrix <- function(flows) {
  net_flows_matrix <- flows %>%
    add_heuristics() %>%
    consolidate() %>%
    netize() %>%
    to_flow_mat()

  only_imports <- net_flows_matrix %>%
    pmax(0) %>%
    verify_matrix(original = net_flows_matrix)

  return(
    only_imports
  )
}

add_heuristics <- function(flows) {
  return(
    flows %>%
      # Ukraine considered as a pure transit country
      set_transit_country(
        origin_key = to_matrix_key("RU", "pipeline"),
        transit_key = to_matrix_key("UA", "pipeline")
      ) %>%
      # Bulgaria as transit from TR (i.e RU) to RO,RS,MK
      bypass_country(
        origin_key = to_matrix_key("RU", "pipeline"),
        transit_key = to_matrix_key("BG", "pipeline"),
        destination_keys = c(
          to_matrix_key("RO", "pipeline"),
          to_matrix_key("RS", "pipeline"),
          to_matrix_key("MK", "pipeline")
        )
      )
  )
}

distribute_until_stable <- function(initial_matrix) {
  n_countries <- dim(initial_matrix)[1]
  current_matrix <- initial_matrix

  for (ignored in seq(n_countries)) { # We don't need to use the iteration number, just iterate for n_countries.
    current_matrix <- distribute(current_matrix)
    if (!is_matrix_square(current_matrix)) {
      stop("Matrix not square")
    }
  }
  return(current_matrix)
}

set_transit_country <- function(flows, origin_key, transit_key) {
  flows %>%
    mutate(
      value = case_when(
        (from == origin_key) & (to == transit_key) ~ 0,
        .default = value
      ),
      from = case_when(
        from == transit_key ~ origin_key,
        .default = from
      ),
      to_country = case_when(
        to == transit_key ~ origin_key,
        .default = to
      )
    )
}

bypass_country <- function(flows, origin_key, transit_key, destination_keys) {
  if (length(origin_key) != 1 | length(transit_key) != 1) {
    stop("Wrong arguments in bypass")
  }

  flows_in_filter <- flows$from %in% origin_key & flows$to %in% transit_key
  flows_out_filter <- flows$from %in% transit_key & flows$to %in% destination_keys
  flows_in <- flows[flows_in_filter, ]
  flows_out <- flows[flows_out_filter, ]

  if (nrow(flows_in) > 1) stop("more than one input flow")
  if (nrow(flows_in) == 0 | nrow(flows_out) == 0) {
    return(flows)
  }

  total_flow_in <- sum(flows_in$value)
  total_flow_out <- sum(flows_out$value)
  if (total_flow_in < 0 | any(flows_out$value < 0)) {
    stop("negative flow, not managed")
  }

  offset <- min(total_flow_in, total_flow_out)
  ratio <- offset / total_flow_out

  flows[flows_in_filter, "value"] <- flows[flows_in_filter, "value"] - offset

  # Create new flows
  new_flows <- flows[flows_out_filter, ]
  new_flows$from <- origin_key
  new_flows$value <- new_flows$value * ratio

  # Update old ones
  flows[flows_out_filter, "value"] <- flows[flows_out_filter, "value"] * (1 - ratio)

  # Concatenate
  flows <- rbind(flows, new_flows)

  # Group just in case other flows existed
  flows <- flows %>%
    group_by(from, to) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()

  # Remove unneeded rows
  flows <- flows %>%
    filter(value != 0)

  return(flows)
}

# Ensure all countries are both in from and to
# so as to have a square matrix
consolidate <- function(flow_matrix) {
  all_countries <- unique(c(flow_matrix$from, flow_matrix$to))
  if (is.null(flow_matrix)) {
    stop("d is null")
  }
  flow_matrix %>%
    full_join(
      tidyr::crossing(
        from = all_countries,
        to = all_countries
      ),
      by = c("to", "from")
    ) %>%
    mutate(value = replace_na(value, 0))
}

to_flow_mat <- function(d) {
  flows <- d %>%
    group_by(country = to, partner = from) %>%
    summarise(import = sum(value, na.rm = T)) %>%
    left_join(
      d %>%
        group_by(country = from, partner = to) %>%
        summarise(export = sum(value, na.rm = T)),
      by = c("country", "partner")
    ) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    mutate(
      net_import = import - export,
      net_export = export - import
    ) %>%
    arrange(country, partner) %>%
    ungroup() %>%
    select(country, partner, net_import) %>%
    spread(partner, net_import)

  flow_mat <- as.matrix(flows %>% select(-c(country))) %>% `dimnames<-`(list(flows$country, flows$country))

  # Add production consumption on diagonal
  diag(flow_mat) <- -rowSums(flow_mat)
  all(rowSums(flow_mat) < 1e-4) # should be equal to 0
  return(flow_mat)
}

# "Netize" so that there is no A <-> B
netize <- function(flow_matrix) {
  if (is.null(flow_matrix)) {
    stop("d is null")
  }

  flow_matrix %>%
    ungroup() %>%
    left_join(
      flow_matrix %>%
        ungroup() %>%
        select(
          from = to,
          to = from,
          value_opposite = value
        ) %>%
        filter(from != to), # We keep production
      by = c("to", "from")
    ) %>%
    mutate(value_opposite = tidyr::replace_na(value_opposite, 0)) %>%
    mutate(value = pmax(0, value - value_opposite)) %>%
    select(-c(value_opposite))
}


distribute <- function(flow_matrix) {
  # Considering each country as an importer, we proportionally distribute
  # the indirect imports of gas to each provider ("neighbours" of importers)
  # from sub-providers ("neighbours" of providers) directly to the importer.
  #
  # When this function is called iteratively, on the first iteration, the
  # "neighbours" will be the actual neighbours in the gas system but on
  # future iterations, these will also include the sub-providers and then
  # the sub-provider's sub-providers (and so on).

  production_for_countries <- diag(flow_matrix)

  countries <- rownames(flow_matrix)

  for (importer in countries) {
    importer_production <- get_production_for_country(flow_matrix, importer)

    # We will iteratively modify this in the for loop.
    aggregate_imports <- importer_production

    for (provider in countries) {
      if (importer == provider) {
        next
      }

      provider_to_importer <- flow_matrix[importer, provider]

      providers_imports <- flow_matrix[provider, ]
      # Ukraine special case: Ukraine cannot reimport gas from Russia
      if ((importer == ua_pipeline)) {
        providers_imports[ru_pipeline] <- 0
      }


      subprovider_attribution <- if (sum(providers_imports) > 0) {
        provider_to_importer * providers_imports / sum(providers_imports)
      } else {
        0
      }

      aggregate_imports <- aggregate_imports + subprovider_attribution
      # What was added needs to be removed, sort of
      # by passing the original
      flow_matrix[provider, ] <- flow_matrix[provider, ] - subprovider_attribution
      flow_matrix[provider, provider] <- production_for_countries[provider]
    }

    flow_matrix[importer, ] <- aggregate_imports
  }

  return(flow_matrix)
}

verify_matrix <- function(current, original) {
  # We use flow_mat to have consumption
  without_production <- current - diag(diag(current))

  matrix_names_same <- all(rownames(current) == colnames(current))
  is_square <- is_matrix_square(current)

  ok <- matrix_names_same && is_square

  if (!ok) {
    stop_reason <- paste(
      "ERROR, matrix did not pass checklist:",
      ifelse(!matrix_names_same, "matrix names not same", ""),
      ifelse(!is_square, "matrix not square", "")
    )
    stop(stop_reason)
  }
  return(current)
}

get_production_for_country <- function(flow_mat, country) {
  countries <- rownames(flow_mat)

  country_index <- which(dimnames(flow_mat)[[1]] == country)
  country_production_only <- diag(diag(flow_mat))[country_index, ]
  names(country_production_only) <- countries

  return(country_production_only)
}

is_matrix_square <- function(mat) {
  dim(mat)[1] == dim(mat)[2]
}

to_matrix_key <- function(country, method) {
  glue("{country}_{method}")
}

from_matrix_key <- function(key) {
  split <- strsplit(key, "_")[[1]]
  names(split) <- c("country", "method")
  return(split)
}


ua_pipeline <- to_matrix_key("UA", "pipeline")
ru_pipeline <- to_matrix_key("RU", "pipeline")
