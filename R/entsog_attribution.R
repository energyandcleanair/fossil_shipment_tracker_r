#' @title Redistribute flows for a period for each day
#' @description Redistributes flows for a period for each day.
#' @param flows_for_whole_period the flows for the whole period
#' @return the flows for the period with the indirect imports distributed
#' @rdname process_iterative
#' @export
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

#' @title Process iterative for day
#' @description Processes flows for a single day using an iterative method.
#' @param flows_for_day the flows for a single day
#' @return the flows for the day with the indirect imports distributed
#' @rdname process_iterative_for_day
#' @export
process_iterative_for_day <- function(flows_for_day) {
  options(dplyr.summarise.inform = FALSE)

  if (("date" %in% names(flows_for_day)) && (length(unique(flows_for_day$date)) > 1)) {
    stop("This function should only work on one date at a time.
       Please aggregate if need be.")
  }

  distributed_flows_for_day <- flows_for_day %>%
    add_heuristics() %>%
    build_flow_matrix() %>%
    distribute_until_stable() %>%
    verify_matrix() %>%
    remove_production() %>%
    convert_to_data_frame()

  return(distributed_flows_for_day)
}


#' @title Build flow matrix
#' @description Builds a flow matrix from a data frame of flows.
#' @param flows The flows to build the matrix from in a data frame with columns
#' from_country, to_country, value.
#' @return
#' A matrix which contains the net imports for each country to each other
#' country. The row of a matrix is the importer, the column of the matrix is
#' the exporter. For example, flow_matrix["BG", "RU"] would give you the value
#' of gas imported from Russia into Bulgaria).
#' @export
#' @rdname build_flow_matrix
build_flow_matrix <- function(flows) {
  return(
    flows %>%
      consolidate() %>%
      netize() %>%
      to_flow_mat() %>%
      pmax(0) %>%
      verify_matrix()
  )
}

#' @title Add heuristics
#' @description Adds heuristics to flows.
#' @param flows The flows to add heuristics to.
#' @return The flows with heuristics added.
#' @export
#' @rdname add_heuristics
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

#' @title Distribute until stable
#' @description Distributes flows until they are stable.
#' @param initial_matrix the matrix to distribute
#' @return the distributed matrix
#' @rdname distribute_until_stable
distribute_until_stable <- function(initial_matrix) {
  current_matrix <- initial_matrix
  prev_matrix <- NULL

  iteration_limit <- 1000
  iteration_number <- 0

  repeat {
    prev_matrix <- current_matrix
    current_matrix <- distribute(prev_matrix)

    if (all(prev_matrix == current_matrix)) {
      break
    }
    if ((iteration_number <- iteration_number + 1) >= iteration_limit) {
      print(glue("WARNING: Iteration limit ({iteration_limit}) reached"))
      break
    }
  }
  return(current_matrix)
}

#' @title Distribute flows
#' @description
#' Considering each country as an importer, we proportionally distribute
#' the indirect imports of gas to each provider ("neighbours" of importers)
#' from sub-providers ("neighbours" of providers) directly to the importer.
#' @details
#' When this function is called iteratively, on the first iteration, the
#' "neighbours" will be the actual neighbours in the gas system but on
#' future iterations, these will also include the sub-providers and then
#' the sub-provider's sub-providers (and so on).
#' @param flow_matrix the flow matrix to distribute
#' @return the flow matrix with the indirect imports distributed
#' @rdname distribute
distribute <- function(flow_matrix) {
  production_for_countries <- diag(flow_matrix)

  countries <- rownames(flow_matrix)

  for (importer in countries) {
    importer_production <- get_production_matrix_for_country(flow_matrix, importer)

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

#' @title Set transit country
#' @description Makes all flows to or from a transit country to or from the
#' origin country.
#' @param flows the flows to modify
#' @param origin_country the country to set as the origin
#' @param transit_country the transit country to override
#' @return the flows with the transit country overridden
#' @rdname set_transit_country
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

#' @title Bypass country
#' @description Bypasses a country by taking the flows from origin_iso2 to
#' transit_iso2 and then from transit_iso2 to destination_iso2s and
#' redistributing them so that the flows go from origin_iso2 to
#' destination_iso2s, proportionally.
bypass_country <- function(flows, origin_iso2, transit_iso2, destination_iso2s) {
  if (length(unique(flows$date)) > 1) {
    stop("Only works for a single date")
  }
  if (length(origin_iso2) != 1 | length(transit_iso2) != 1) {
    stop("Wrong arguments in bypass")
  }

  origin_to_transit <- flows %>%
    filter(from_country == origin_iso2, to_country == transit_iso2)

  transit_to_destination <- flows %>%
    filter(from_country == transit_iso2, to_country %in% destination_iso2s)

  assert_that(
    nrow(origin_to_transit) <= 1
  )

  total_to_destinations_ma <- sum(transit_to_destination$value_ma)
  total_from_origin_ma <- sum(origin_to_transit$value_ma)

  transit_through_ma <- min(total_to_destinations_ma, total_from_origin_ma)
  leave_in_transit_ma <- max(total_from_origin_ma - total_to_destinations_ma, 0)

  provided_by_transit_ma <- max(total_to_destinations_ma - total_from_origin_ma, 0)

  proportion_transit_through <- transit_through_ma / total_from_origin_ma
  proportion_leave_in_transit <- leave_in_transit_ma / total_from_origin_ma

  proportion_from_transit <- provided_by_transit_ma / total_from_origin_ma

  total_from_origin <- sum(origin_to_transit$value)

  destination_overwrites <- transit_to_destination %>%
    rowwise() %>%
    mutate(
      proportion = value_ma / total_to_destinations_ma,
      overwrite_from_origin = proportion * proportion_transit_through * total_from_origin,
      overwrite_from_transit = proportion * proportion_from_transit * total_from_origin
    ) %>%
    ungroup()

  new_origin_to_transit <- origin_to_transit %>%
    mutate(value = total_from_origin * proportion_leave_in_transit)

  new_transit_to_destinations <- destination_overwrites %>%
    mutate(value = overwrite_from_transit)

  new_origin_to_destinations <- destination_overwrites %>%
    mutate(
      from_country = origin_iso2,
      value = overwrite_from_origin
    )

  flows_without_transit <- flows %>%
    filter(
      !(
        (from_country == transit_iso2 & to_country %in% destination_iso2s) |
          (from_country == origin_iso2 & to_country == transit_iso2)
      )
    )

  return(
    bind_rows(
      flows_without_transit,
      new_origin_to_transit,
      new_transit_to_destinations,
      new_origin_to_destinations
    )
  )
}

#' @title Consolidate
#' @description Consolidates a flow matrix so that all countries are both in
#' from_country and to_country.
#' @param flow_matrix the flow matrix to consolidate
#' @return the flow matrix with all countries in from_country and to_country
#' @rdname consolidate
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

#' @title To flow matrix
#' @description Converts a data frame of flows to a flow matrix.
#' @param flows the flows to convert
#' @return a flow matrix
#' @rdname to_flow_mat
to_flow_mat <- function(flows) {
  flows <- flows %>%
    group_by(country = to_country, partner = from_country) %>%
    summarise(import = sum(value, na.rm = T)) %>%
    left_join(
      flows %>%
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

#' @title Netize
#' @description "Netize" so that there is no A <-> B
#' @param flow_matrix the flow matrix to netize
#' @return the flow matrix with no A <-> B
#' @rdname netize
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

#' @title Convert to data frame
#' @description Converts a flow matrix to a data frame.
#' @param flow_matrix the flow matrix to convert
#' @return a data frame of flows
#' @rdname convert_to_data_frame
convert_to_data_frame <- function(flow_matrix) {
  all_countries <- dimnames(flow_matrix)[[1]]

  return(
    bind_cols(country = sort(all_countries), as.data.frame(flow_matrix)) %>%
      gather(key = "from_country", value = "value", -country) %>%
      rename(to_country = country)
  )
}

#' @title Verify matrix
#' @description Verifies that a flow matrix is square and that the rownames and
#' colnames are the same.
#' @param current the matrix to verify
#' @return the matrix which has been verified
#' @details will stop if the matrix is not square or if the rownames and
#' colnames are not the same
#' @rdname verify_matrix
verify_matrix <- function(current) {
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

#' @title Get production for country
#' @description Gets the production for a country from a flow matrix.
#' @param flow_mat the flow matrix to get the production from
#' @param country the country to get the production for
#' @return a matrix containing only the production for the country
#' @rdname get_production_matrix_for_country
get_production_matrix_for_country <- function(flow_mat, country) {
  countries <- rownames(flow_mat)

  country_index <- which(dimnames(flow_mat)[[1]] == country)
  country_production_only <- diag(diag(flow_mat))[country_index, ]
  names(country_production_only) <- countries

  return(country_production_only)
}

#' @title Is matrix square
#' @description Checks if a matrix is square.
#' @param mat the matrix to check
#' @return TRUE if the matrix is square, FALSE otherwise
#' @rdname is_matrix_square
is_matrix_square <- function(mat) {
  dim(mat)[1] == dim(mat)[2]
}

#' @title Remove production
#' @description Removes production from a flow matrix.
#' @param flow_matrix the flow matrix to remove production from
#' @return the flow matrix with production removed
#' @rdname remove_production
remove_production <- function(flow_matrix) {
  diag(flow_matrix) <- 0
  return(flow_matrix)
}
