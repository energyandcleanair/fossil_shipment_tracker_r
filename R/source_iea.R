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


