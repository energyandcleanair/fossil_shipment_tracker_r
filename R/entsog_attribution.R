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

  if (("date" %in% names(flows_for_day)) && (length(unique(flows_for_day$date)) > 1)) {
    stop("This function should only work on one date at a time.
       Please aggregate if need be.")
  }

  # The flow_matrix contains the net imports for each country to each
  # other country. The row of a matrix is the importer, the column of the
  # matrix is the exporter. For example, flow_matrix["BG", "RU"] would give
  # you the value of gas imported from Russia into Bulgaria).
  flow_matrix <- build_flow_matrix(flows_for_day)

  distributed_flows_for_day <- flow_matrix %>%
    distribute_until_stable() %>%
    verify_matrix(original = flow_matrix) %>%
    remove_production() %>%
    convert_to_data_frame()

  return(distributed_flows_for_day)
}

convert_to_data_frame <- function(flow_matrix) {
  all_countries <- dimnames(flow_matrix)[[1]]

  return(
    bind_cols(country = sort(all_countries), as.data.frame(flow_matrix)) %>%
      gather(key = "from_country", value = "value", -country) %>%
      rename(to_country = country)
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
      set_transit_country(origin_country = "RU", transit_country = "UA") %>%
      # Bulgaria as transit from TR (i.e RU) to RO,RS,MK
      bypass_country(
        origin_iso2 = "RU",
        transit_iso2 = "BG",
        destination_iso2s = c("RO", "RS", "MK")
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

set_transit_country <- function(flows, origin_country, transit_country) {
  flows %>%
    mutate(
      value = case_when(
        (from_country == origin_country) & (to_country == transit_country) ~ 0,
        .default = value
      ),
      from_country = case_when(
        from_country == transit_country ~ origin_country,
        .default = from_country
      ),
      to_country = case_when(
        to_country == transit_country ~ origin_country,
        .default = to_country
      )
    )
}

bypass_country <- function(flows, origin_iso2, transit_iso2, destination_iso2s) {
  if (length(unique(flows$date)) > 1) {
    stop("Only works for a single date")
  }
  if (length(origin_iso2) != 1 | length(transit_iso2) != 1) {
    stop("Wrong arguments in bypass")
  }

  flows_in_filter <- flows$from_country %in% origin_iso2 & flows$to_country %in% transit_iso2
  flows_out_filter <- flows$from_country %in% transit_iso2 & flows$to_country %in% destination_iso2s
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
  new_flows$from_country <- origin_iso2
  new_flows$value <- new_flows$value * ratio

  # Update old ones
  flows[flows_out_filter, "value"] <- flows[flows_out_filter, "value"] * (1 - ratio)

  # Concatenate
  flows <- rbind(flows, new_flows)

  # Group just in case other flows existed
  flows <- flows %>%
    group_by(from_country, to_country, date) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()

  # Remove unneeded rows
  flows <- flows %>%
    filter(value != 0)

  return(flows)
}

# Ensure all countries are both in from_country and to_country
# so as to have a square matrix
consolidate <- function(flow_matrix) {
  all_countries <- unique(c(flow_matrix$from_country, flow_matrix$to_country))
  if (is.null(flow_matrix)) {
    stop("d is null")
  }
  flow_matrix %>%
    full_join(
      tidyr::crossing(
        from_country = all_countries,
        to_country = all_countries
      ),
      by = c("to_country", "from_country")
    ) %>%
    mutate(value = replace_na(value, 0))
}

to_flow_mat <- function(d) {
  flows <- d %>%
    group_by(country = to_country, partner = from_country) %>%
    summarise(import = sum(value, na.rm = T)) %>%
    left_join(
      d %>%
        group_by(country = from_country, partner = to_country) %>%
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
          from_country = to_country,
          to_country = from_country,
          value_opposite = value
        ) %>%
        filter(from_country != to_country), # We keep production
      by = c("to_country", "from_country")
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
      if ((importer == "UA")) {
        providers_imports["RU"] <- 0
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
