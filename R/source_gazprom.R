gazprom.get_flows <- function(){




  flows_2020 <- list("DE"=41.6,
                     "NL" = 48.1,
                     "BE" = 1.3,
                    "FR" = 14,
                    "ES" = 0.8,
                    "IT" = 20.9,
                    "CH" = 0.4,
                    "UK" = 8.9,
                    "DK" = 1.8,
                    "FI" = 1.6,
                    "PL" = 9.7,
                    "SK" = 7.5,
                    "CZ" = 4,
                    "AT" = 10.6,
                    "SI" = 0.4,
                    "HR" = 1.8,
                    "RO" = 1,
                    "BG" = 2.3,
                    "GR" = 3.1,
                    "HU" = 8.6,
                    "EE" = .3,
                    "LV" = 1.6,
                    "LT" = .9)


  tibble(to_country=countrycode::countrycode(names(flows_2020),"iso2c","country.name", custom_match = c(UK="United Kingdom")),
         value=unlist(flows_2020)*1e9 / 12,
         unit="m3") %>%
    mutate(from_country='Russia') %>%
    tidyr::crossing(month=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by='month'))

}
