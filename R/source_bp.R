bp.get_flows <- function(use_cache=T){

  f <- system.file("extdata", "bp-stats-review-2021-all-data.xlsx",
                   package="russiacounter")
  # f <- "data/bp-stats-review-2021-all-data.xlsx"

  sheet <- "Gas - Trade movts - pipeline"



  # natural_gas
  readxl::read_xlsx(f,
                    sheet="Gas - Trade movts - pipeline",
                    range="A3:W41") %>%
    tidyr::pivot_longer(cols = -To,
                        names_to="partner") %>%
    rename(country=To) %>%
    mutate(commodity="natural_gas",
           unit="m3",
           value=value*1e9) %>%
    filter(!country %in% c("North America", "S. & Cent. America", "Europe",
                           "CIS","Middle East","Middle East", "Asia Pacific")) %>%
    mutate(destination_iso2=countrycode::countrycode(country, "country.name", "iso2c")) %>%
    mutate(destination_region=ifelse(destination_iso2 %in% countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")] | country=='Other EU', "EU28", NA)) %>%
    mutate(year=2020)



#
#   #lng
#   readxl::read_xlsx(f,
#                     sheet="Gas - Trade movts LNG",
#                     range="A4:W38") %>%
#     tidyr::pivot_longer(cols = -To,
#                         names_to="partner") %>%
#     rename(country=To) %>%
#     mutate(commodity=commodity,
#            unit="m3",
#            value=value*1e9) %>%
#     filter(!country %in% c("North America", "S. & Cent. America", "Europe",
#                            " Middle East & Africa"," Asia Pacific ")) %>%
#     mutate(commodity="lng")
#
#
#
#
#
#
#     filter(!is.na(value))
#
#
#
#
#   lapply(names(commodity_sheet), function(commodity){
#     sheet <- commodity_sheet[[commodity]]
#     readxl::read_xlsx(f,
#                       sheet=sheet,
#                       skip=as.integer(sheet_skip[[sheet]])) %>%
#       tidyr::pivot_longer(cols = -To,
#                           names_to="partner") %>%
#       rename(country=To) %>%
#       mutate(commodity=commodity,
#              unit=sheet_unit[[sheet]]) %>%
#       filter(!country %in% c("North America", "S. & Cent. America", "Europe",
#                              "CIS","Middle East","Middle East", "Asia Pacific"),
#              !grepl("Total",country)) %>%
#       filter(!is.na(value))
#   })
#
#
#
#   commodity_codes <- list(
#     "G3000"="natural_gas",
#     "G3200"="lng",
#     "O4100_TOT"="crude_oil",
#     "2210"="lignite",
#     "2111"="coal")
#
#   units <- list(
#     "MIO_M3"="m3",
#     "TJ_GCV"="TJ",
#     "THS_T"="tonne"
#   )
#
#   unit_factors <- list(
#     "MIO_M3"=1e6,
#     "TJ_GCV"=1,
#     "THS_T"=1
#   )

}


bp.get_gas_consumption <- function(){
  f <- system.file("extdata", "bp-stats-review-2021-all-data.xlsx",
                   package="russiacounter")
  # f <- "data/bp-stats-review-2021-all-data.xlsx"

  sheet <- "Gas Consumption - Bcm"



  # natural_gas
  d <- readxl::read_xlsx(f,
                    sheet=sheet,
                    range="A3:BE109")

  d['country'] <- d[,1]
  d %>%
    select_at(c('country'='Billion cubic metres', as.character(seq(2015, 2020)))) %>%
    tidyr::pivot_longer(cols=-country, names_to='year') %>%
    mutate(unit='m3',
           value=as.numeric(value) * 1e9) %>%
    mutate(iso2=countrycode::countrycode(country, "country.name", "iso2c"),
           year=as.numeric(year)) %>%
    filter(!is.na(iso2))
}
