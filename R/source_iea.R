iea.get_flows <- function(years, remove_kipi=TRUE){
  f <- system.file("extdata", "Export_GTF_IEA_202204.xls",
                   package="russiacounter")
  # f <- "data/iea/Export_GTF_IEA_202202.xls"

  readxl::read_xls(f) %>%
    tidyr::pivot_longer(cols=-c(Borderpoint, Exit, `...3`, Entry,	`MAXFLOW (Mm3/h)`),
                        values_transform=list(value=as.numeric),
                        names_to="month") %>%
    mutate(value_m3=value*1E6,
           month=lubridate::floor_date(as.Date(as.numeric(month), origin = "1900-01-01"), 'month')) %>%
    filter(!remove_kipi | (Borderpoint != 'Kipi')) %>%
    mutate(value_mwh=value * gcv_MWh_per_m3) %>%
    select(country=Entry,
           partner=Exit,
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
           value_tj=X5)

  gcv <- read_csv(f_gcv, skip=2) %>%
    filter(grepl('GCV', FLOW)) %>%
    select(country=COUNTRY,
           year=TIME,
           value_kj_per_m3=X5)

  consumption %>%
    left_join(gcv) %>%
    mutate(value = as.numeric(value_tj) * 1e9 / as.numeric(value_kj_per_m3)) %>%
    select(-c(value_tj, value_kj_per_m3)) %>%
    filter(!is.na(value)) %>%
    mutate(unit='m3') %>%
    mutate(iso2=countrycode::countrycode(iconv(country, "latin1", "ASCII", sub=""), "country.name", "iso2c",
                                         custom_match=c(`Republic of Turkiye`='TR')))

}
