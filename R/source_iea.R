iea.get_flows <- function(years, remove_kipi=TRUE){
  f <- system.file("extdata", "iea/Export_GTF_IEA_202209.xls",
                   package="russiacounter")

  custom_match = c("Republic of Türkiye"="TR",
                   "Liquefied Natural Gas"="lng")

  readxl::read_xls(f) %>%
    tidyr::pivot_longer(cols=-c(Borderpoint, Exit, `...3`, Entry,	`MAXFLOW (Mm3/h)`),
                        values_transform=list(value=as.numeric),
                        names_to="month") %>%
    mutate(value_m3=value*1E6,
           month=lubridate::floor_date(as.Date(as.numeric(month), origin = "1900-01-01"), 'month')) %>%
    filter(!remove_kipi | (Borderpoint != 'Kipi')) %>%
    mutate(value_mwh=value_m3 * gcv_MWh_per_m3) %>%
    mutate(departure_iso2=countrycode::countrycode(Exit, 'country.name', 'iso2c',
                                                    custom_match = custom_match),
           destination_iso2=countrycode::countrycode(Entry, 'country.name', 'iso2c',
                                                     custom_match = custom_match),
           ) %>%
    select(country=Entry,
           partner=Exit,
           departure_iso2,
           destination_iso2,
           month,
           value_m3,
           value_mwh) %>%
    mutate(source="IEA",
           commodity=ifelse(partner=='Liquefied Natural Gas', 'lng', 'natural_gas'))
}


iea.get_detailed_flows <- function(years, remove_kipi=TRUE){
  f <- system.file("extdata", "iea/Export_GTF_IEA_202209.xls",
                   package="russiacounter")

  custom_match = c("Republic of Türkiye"="TR",
                   "Liquefied Natural Gas"="lng")

  readxl::read_xls(f) %>%
    tidyr::pivot_longer(cols=-c(Borderpoint, Exit, `...3`, Entry,	`MAXFLOW (Mm3/h)`),
                        values_transform=list(value=as.numeric),
                        names_to="month") %>%
    mutate(value_m3=value*1E6,
           month=lubridate::floor_date(as.Date(as.numeric(month), origin = "1900-01-01"), 'month')) %>%
    filter(!remove_kipi | (Borderpoint != 'Kipi')) %>%
    mutate(value_mwh=value_m3 * gcv_MWh_per_m3) %>%
    mutate(departure_iso2=countrycode::countrycode(Exit, 'country.name', 'iso2c',
                                                   custom_match = custom_match),
           destination_iso2=countrycode::countrycode(Entry, 'country.name', 'iso2c',
                                                     custom_match = custom_match),
    ) %>%
    select(country=Entry,
           partner=Exit,
           departure_iso2,
           destination_iso2,
           point=Borderpoint,
           month,
           value_m3,
           value_mwh) %>%
    mutate(source="IEA",
           commodity=ifelse(partner=='Liquefied Natural Gas', 'lng', 'natural_gas'))
}

iea.get_gas_consumption <- function(){

  f_consumption <- system.file("extdata", "iea/iea_ng_consumption.csv",
                   package="russiacounter")

  f_gcv <- system.file("extdata", "iea/iea_ng_gcv.csv",
                               package="russiacounter")

  f_consumption <- "data/iea/iea_ng_supply.csv"
  # f_gcv <- "data/iea/iea_ng_gcv.csv"

  consumption <- read_csv(f_consumption, skip=2) %>%
    select(country=COUNTRY,
           year=TIME,
           value_tj=`...5`)

  gcv <- read_csv(f_gcv, skip=2) %>%
    filter(grepl('GCV', FLOW)) %>%
    select(country=COUNTRY,
           year=TIME,
           value_kj_per_m3=`...5`)

  consumption %>%
    left_join(gcv) %>%
    mutate(value = as.numeric(value_tj) * 1e9 / as.numeric(value_kj_per_m3)) %>%
    select(-c(value_tj, value_kj_per_m3)) %>%
    filter(!is.na(value)) %>%
    mutate(unit='m3') %>%
    mutate(iso2=countrycode::countrycode(iconv(country, "latin1", "ASCII", sub=""), "country.name", "iso2c",
                                         custom_match=c(`Republic of Turkiye`='TR')))

}
