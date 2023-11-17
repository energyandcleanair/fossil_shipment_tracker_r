price_cap.apply_price_cap <- function(p, version){
  price_caps <- price_cap.get_price_caps(p, version)

  p %>%
    left_join(price_caps) %>%
    mutate(eur_per_tonne=pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T))
}

price_cap.get_price_caps <- function(p, version) {
  get_price_cap_fns <- list(
    "^default$" = price_cap.get_default,
    "^2021H1$" = price_cap.get_2021H1,
    "^usd[0-9]+$" = price_cap.get_usd,
    "cap_is_working" = price_cap.get_cap_is_working
  )

  price_cap_fn <- get_price_cap_fns[grepl(names(get_price_cap_fns), version)]
  price_cap_fn(p, version)
}


price_cap.get_default <- function(p, version) {

  precaps <- list(
    crude_oil = 60 * barrel_per_tonne,
    crude_oil_espo = 60 * barrel_per_tonne,
    crude_oil_urals = 60 * barrel_per_tonne
  )

  eur_per_usd <- price.eur_per_usd(
    date_from = min(p$date),
    date_to = min(date(max(p$date)), lubridate::today())
  ) %>%
    fill_gaps_and_future()

  tibble(
    commodity = names(precaps),
    usd_per_tonne = unlist(precaps)
  ) %>%
    tidyr::crossing(eur_per_usd) %>%
    arrange(date) %>%
    tidyr::fill(eur_per_usd) %>%
    mutate(max_eur_per_tonne = case_when(
      date >= '2022-12-05' ~ usd_per_tonne * eur_per_usd,
      T ~ Inf) %>%
    select(-c(usd_per_tonne, eur_per_usd))
}

price_cap.get_2021H1 <- function(p, version) {
  # Cap using 2021H1 prices, weighted average, one globally
  precaps <- readRDS(system.file("extdata", "comtrade_eurostat.RDS", package = "russiacounter")) %>%
    filter(lubridate::floor_date(date, "halfyear") == "2021-01-01") %>%
    group_by(commodity, unit) %>%
    summarise_at("value", sum, na.rm = T) %>%
    tidyr::spread(unit, value) %>%
    mutate(eur_per_tonne = eur / tonne) %>%
    select(-c(eur, tonne)) %>%
    mutate(commodity = recode(commodity,
      oil = "crude_oil"
    ))

  precaps <- bind_rows(precaps, precaps %>%
    filter(commodity == "crude_oil") %>%
    mutate(commodity = "pipeline_oil"))

  eur_per_usd <- price.eur_per_usd(
    date_from = min(p$date),
    date_to = min(date(max(p$date)), lubridate::today())
  )

  eur_per_usd_2021H1 <- eur_per_usd %>%
    filter(lubridate::floor_date(date, "halfyear") == "2021-01-01") %>%
    summarise(eur_per_usd_base = mean(eur_per_usd))

  eur_per_usd %>%
    tidyr::crossing(eur_per_usd_2021H1) %>%
    mutate(price_adjustment = eur_per_usd / eur_per_usd_base) %>%
    select(date, price_adjustment) %>%
    tidyr::crossing(precaps) %>%
    mutate(eur_per_tonne = case_when(
      commodity %in% c("natural_gas", "lng") ~ eur_per_tonne,
      T ~ eur_per_tonne * price_adjustment
    )) %>%
    select(-c(price_adjustment)) %>%
    rename(max_eur_per_tonne = eur_per_tonne)
}

price_cap.get_usd <- function(p, version) {
  cap_usd <- as.numeric(sub("usd", "", version))

  precaps <- list(
    crude_oil = cap_usd * barrel_per_tonne,
    crude_oil_espo = cap_usd * barrel_per_tonne,
    crude_oil_urals = cap_usd * barrel_per_tonne,
  )

  eur_per_usd <- price.eur_per_usd(
    date_from = min(p$date),
    date_to = min(max(p$date), lubridate::today())
  )

  tibble(
    commodity = names(precaps),
    usd_per_tonne = unlist(precaps)
  ) %>%
    tidyr::crossing(eur_per_usd) %>%
    arrange(date) %>%
    tidyr::fill(eur_per_usd) %>%
    mutate(max_eur_per_tonne = usd_per_tonne * eur_per_usd) %>%
    select(-c(usd_per_tonne, eur_per_usd))
}

price_cap.get_cap_is_working <- function(p, version) {

  eur_per_usd <- price.eur_per_usd(
    date_from = min(p$date),
    date_to = min(date(max(p$date)), lubridate::today())
  )

  p %>%
    price_cap.add_subcommodity_group() %>%
    left_join(eur_per_usd) %>%
    mutate(
      cap_usd_per_barrel = case_when(
        grepl("crude_oil", commodity) & date >= "2022-12-05" ~ 60,
        oil_products_pricing_subgroup == "low value" & date >= "2023-02-05" ~ 45,
        oil_products_pricing_subgroup == "premium" & date >= "2023-02-05" ~ 100,
        T ~ Inf
      ),
      cap_eur_per_tonne = cap_usd_per_barrel * eur_per_usd * barrel_per_tonne,
      max_eur_per_tonne = pmin(eur_per_tonne, cap_eur_per_tonne)
    ) %>%
    select(-c(oil_products_pricing_subgroup, cap_eur_per_tonne, cap_usd_per_barrel))
}

price_cap.add_subcommodity_group <- function(p) {
  p %>%
    mutate(
      oil_products_pricing_subgroup = case_when(
        grepl("fuel_oils$|_fo$|_slurry$|vgo$", commodity) ~ "low value",
        grepl("gasoline|naphtha|diesel|gasoil|jet$|kero|blending_comps", commodity) ~ "premium",
        T ~ NA_character_
      )
    )
}
