get_iea <- function(years){
  readxl::read_xls("data/iea/Export_GTF_IEA_202112.xls") %>%
    filter(Exit=="Russia" | Entry=="Russia") %>%
    tidyr::pivot_longer(cols=-c(Borderpoint, Exit, `...3`, Entry,	`MAXFLOW (Mm3/h)`),
                        values_transform=list(value=as.numeric),
                        names_to="month") %>%
    mutate(value=value*1E6*ifelse(Entry=="Russia",-1,1),
           unit="m3/month",
           month=lubridate::floor_date(as.Date(as.numeric(month), origin = "1900-01-01"), 'month')) %>%
    mutate(value=value / gcv_MWh_per_m3,
           unit="MWh/month") %>%
    select(country=Entry,
           month,
           value,
           unit) %>%
    mutate(source="IEA Gas Trade Flow",
           commodity="Natural Gas")
}
