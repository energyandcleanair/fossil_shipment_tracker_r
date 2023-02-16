source('R/source_kpler.R')
library(countrycode)
library(remotes)
library(rcrea)
library(pbapply)
library(tidytext)

countries <- c("Russian Federation", "India", "Turkey", "Malaysia", "Egypt", "China", "United Arab Emirates")
# flows_crea <- read_csv(sprintf('https://api.russiafossiltracker.com/v0/voyage?aggregate_by=status,commodity_origin_iso2,commodity_destination_iso2,commodity,date&format=csv&date_from=%s&commodity=crude_oil,oil_products,oil_or_chemical', min(flows_kpler$date)))
flows_crea <- read_csv(sprintf('http://localhost:8080/v0/voyage?aggregate_by=status,commodity_origin_iso2,commodity_destination_iso2,commodity,date&format=csv&date_from=%s&commodity=crude_oil,oil_products,oil_or_chemical', min(flows_kpler$date)))
flows_kpler_laundromat <- kpler.get_laundromat_flows()

flows_kpler_fmt <- flows_kpler_laundromat %>%
  filter(product!='Total',
         unit=='t') %>%
  rename(value_tonne=value) %>%
  mutate(commodity_origin_iso2 = countrycode(from, "country.name", "iso2c"),
         commodity_destination_iso2 = "World",
  ) %>%
  filter(value_tonne>0,
         !is.na(commodity_destination_iso2)) %>%
  select(commodity_origin_iso2, commodity_destination_iso2, date, commodity=product, value_tonne) %>%
  mutate(source='Kpler')

flows_crea_fmt <- flows_crea %>%
  filter(status!='undetected_arrival') %>%
  filter(commodity_destination_iso2 != commodity_origin_iso2) %>%
  select(commodity_origin_iso2, commodity_destination_iso2, date=departure_date, commodity, value_tonne) %>%
  mutate(source='CREA')

recode_commodity <- function(df){
  df %>%
    mutate(commodity =
             case_when(
               grepl("_", commodity) ~ paste0("CREA: ",commodity),
               grepl("Crude", commodity, ignore.case=T) ~ "Crude",
               grepl("Gaso", commodity) ~ "Gasoline",
               grepl("Jet", commodity) ~ "Jet",
               grepl("Diesel", commodity) ~ "Diesel",
               grepl("Fuel Oils|FO", commodity) ~ "Fuel oils",
               T ~ "Others"
             )
    )
}

save_but_keep_older_dates <- function(df, filepath){
  if(file.exists(filepath)){
    old <- read_csv(filepath,
                    col_types = list(unit = col_character())) %>%
      filter(date < min(df$date))
    df <- bind_rows(old, df)
  }
  write_csv(df, filepath)
}

# Crude oil totals --------------------------------------------------------

bind_rows(flows_kpler_fmt, flows_crea_fmt) %>%
  filter(date >= lubridate::ceiling_date(min(flows_kpler$date), "month"),
         date <= '2022-12-31') %>%
  filter(grepl('crude', commodity, ignore.case=T)) %>%
  filter(commodity_origin_iso2 != 'KZ') %>%
  # filter(commodity != 'Crude') %>%
  group_by(commodity_origin_iso2, source, month=floor_date(date, 'month')) %>%
  summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(commodity_origin_iso2, source, month, fill=list(value_tonne=0)) %>%
  ggplot() +
  geom_bar(aes(month, value_tonne / 1e6, fill=source), stat='identity',
           position='dodge') +
  facet_wrap(~commodity_origin_iso2, scale='free_y') +
  rcrea::theme_crea() +
  labs(title='2022 Crude oil export tonnage from selected countries',
       caption='Shipments only.',
       y='million tonnes / month',
       x=NULL) +
  rcrea::scale_fill_crea_d() +
  scale_x_date(date_labels = "%b",
               date_breaks = '1 month')







# Oil products ------------------------------------------------------------
bind_rows(flows_kpler_fmt, flows_crea_fmt) %>%
  filter(date >= lubridate::ceiling_date(min(flows_kpler$date), "month"),
         date <= '2022-12-31') %>%
  mutate(commodity_group=ifelse(grepl('crude', commodity, ignore.case=T),
                                'Crude',
                                'Products')) %>%
  filter(!is.na(commodity_group)) %>%
  filter(commodity_origin_iso2 != 'KZ') %>%
  # filter(commodity != 'Crude') %>%
  group_by(commodity_origin_iso2, commodity_group, source, month=floor_date(date, 'month')) %>%
  summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(commodity_origin_iso2, source, commodity_group, month, fill=list(value_tonne=0)) %>%
  ggplot() +
  geom_bar(aes(month, value_tonne / 1e6, fill=source), stat='identity',
           position='dodge') +
  facet_grid(commodity_origin_iso2 ~ commodity_group, scale='free_y') +
  rcrea::theme_crea() +
  labs(title='2022 Export tonnage from selected countries',
       caption='Shipments only.',
       y='million tonnes / month',
       x=NULL) +
  rcrea::scale_fill_crea_d() +
  scale_x_date(date_labels = "%b",
               date_breaks = '1 month')



# By country --------------------------------------------------------------
for(country in countries){
  iso2 <- countrycode(country, "country.name","iso2c")
  plot_products <- function(iso2, include_crea_crude_oil){
    plt <- bind_rows(flows_kpler_fmt, flows_crea_fmt) %>%
      filter(date >= lubridate::ceiling_date(min(flows_kpler$date), "month"),
             date <= '2022-11-30') %>%
      filter(commodity_origin_iso2==iso2) %>%
      filter(!grepl('crude', commodity, T) | (include_crea_crude_oil & commodity=='crude_oil')) %>%
      recode_commodity() %>%
      filter(commodity_origin_iso2 != 'KZ') %>%
      group_by(commodity_origin_iso2, commodity, source, month=floor_date(date, 'month')) %>%
      summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
      ungroup() %>%
      tidyr::complete(commodity_origin_iso2, commodity, source, month, fill=list(value_tonne=0)) %>%
      ggplot() +
      geom_bar(aes(month, value_tonne / 1e6, fill=commodity), stat='identity') +
      facet_grid(commodity_origin_iso2 ~ source, scale='free_y') +
      rcrea::theme_crea() +
      labs(title='2022 Non crude oil export tonnage from selected countries',
           subtitle=paste(ifelse(include_crea_crude_oil, 'INCLUDING', 'EXCLUDING'),
                          'CREA Crude Oil'),
           caption='Shipments only.',
           y='million tonnes / month',
           x=NULL) +
      rcrea::scale_fill_crea_d() +
      scale_x_date(date_labels = "%b",
                   date_breaks = '1 month')

    filepath <- sprintf("scripts/validation_kpler/%s_oil_products%s.png", iso2,
                        ifelse(include_crea_crude_oil, "_with_crude_oil", ""))
    ggsave(filepath, plot=plt, width=10, height=5)
  }

  plot_products(iso2=iso2, include_crea_crude_oil = T)
  plot_products(iso2=iso2, include_crea_crude_oil = F)
}



# Installations --------------------------------------------------------------
pbapply::pblapply(countries, function(country){
  print(country)
  iso2 <- countrycode(country, "country.name","iso2c")
  flows_refineries <- kpler.get_flows_raw(froms=country,
                                          products=NULL,
                                          split="Origin Installations")
  refineries <- unique(flows_refineries$split)
  flows_refineries_detailed <- kpler.get_flows_raw(from_installations = refineries,
                                                   products=NULL,
                                                   split="Products")

  plt <- flows_refineries_detailed %>%
    rename(commodity=split) %>%
    recode_commodity() %>%
    group_by(commodity, refinery=from_installation) %>%
    summarise(value_tonne=sum(value, na.rm=T)) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(value_tonne/1e6, reorder(refinery, value_tonne), fill=commodity),
             stat='identity') +
    rcrea::scale_fill_crea_d() +
    labs(title=sprintf("%s refineries", country),
         x=NULL,
         y=NULL) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1)),
                       limits=c(0, NA))

  dir.create("scripts/validation_kpler", F, T)
  save_but_keep_older_dates(flows_refineries_detailed, sprintf("scripts/validation_kpler/%s_refineries.csv", iso2))
  ggsave(sprintf("scripts/validation_kpler/%s_refineries.png", iso2),
         width=10, height=5, plot=plt)
})



# Ports -------------------------------------------------------------------

pbapply::pblapply(countries, function(country){
  print(country)
  iso2 <- countrycode(country, "country.name","iso2c")

  flows_tmp <- kpler.get_flows_raw(froms=country,
                                          products=NULL,
                                          split="Origin Ports")
  ports <- unique(flows_tmp$split)
  flows_ports_kpler <- kpler.get_flows_raw(froms=ports,
                                           products=NULL,
                                           split="Products")

  flows_ports_crea <- read_csv(sprintf('https://api.russiafossiltracker.com/v0/voyage?aggregate_by=departure_port,commodity&date_from=%s&date_to=%s&departure_iso2=%s&format=csv&commodity_group=oil&status=completed',
                                      min(flows_ports_kpler$date),
                                      max(flows_ports_kpler$date),
                                      iso2))


  flows_data <- bind_rows(
    flows_ports_kpler %>%
      select(commodity=split, port=from, value_tonne=value) %>%
      mutate(source='Kpler'),

    flows_ports_crea %>%
      filter(commodity_group=='oil') %>%
      mutate(departure_port_name=stringr::str_to_title(gsub(" ANCH","",departure_port_name))) %>%
      select(commodity=commodity_name, port=departure_port_name, value_tonne) %>%
      mutate(source='CREA')
  ) %>%
    recode_commodity() %>%
    group_by(commodity, port, source) %>%
    summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
    mutate(commodity=case_when(commodity=='Crude' ~ 'Crude',
                                      T ~ 'Products'))

  plt <- flows_data %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(value_tonne/1e6,
                 reorder_within(port, value_tonne, source),
                 fill=commodity),
             stat='identity') +
    rcrea::scale_fill_crea_d() +
    labs(title=sprintf("%s ports", country),
         x=NULL,
         y=NULL) +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1)),
                       limits=c(0, NA)) +
    facet_wrap(~source, scales='free_y') +
    tidytext::scale_y_reordered()


  dir.create("scripts/validation_kpler", F, T)
  save_but_keep_older_dates(flows_ports_kpler, sprintf("scripts/validation_kpler/%s_ports.csv", iso2))
  ggsave(sprintf("scripts/validation_kpler/%s_ports.png", iso2),
         width=10, height=5, plot=plt)

})



# Particulars -------------------------------------------------------------



# Compare with Jamnagar alone
flows_jamnagar <- kpler.get_flows_raw_single(from_installation=c("Jamnagar Refinery"),
                                             split="Products")

flows_jamnagar_fmt <- flows_jamnagar %>%
  rename(commodity=split, value_tonne=value) %>%
  mutate(source="Kpler",
         commodity_origin_iso2='IN')

bind_rows(flows_jamnagar_fmt, flows_crea_fmt) %>%
  recode_commodity() %>%
  filter(date >= lubridate::ceiling_date(min(flows_jamnagar_fmt$date), "month"),
         date <= '2022-11-30') %>%
  filter(commodity_origin_iso2=='IN') %>%
  filter(commodity_origin_iso2 != 'KZ') %>%
  group_by(commodity_origin_iso2, commodity, source, month=floor_date(date, 'month'), commodity) %>%
  summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(commodity_origin_iso2, commodity, source, month, fill=list(value_tonne=0)) %>%
  ggplot() +
  geom_bar(aes(month, value_tonne / 1e6, fill=commodity), stat='identity') +
  facet_grid(commodity_origin_iso2 ~ source, scale='free_y') +
  rcrea::theme_crea() +
  labs(title='2022 Jamnagar exports',
       caption='Shipments only.',
       y='million tonnes / month',
       x=NULL) +
  rcrea::scale_fill_crea_d() +
  scale_x_date(date_labels = "%b",
               date_breaks = '1 month')


flows_india =  kpler.get_flows_raw(
  froms="India",
  products=NULL, #kpler.get_products(),
  split="Destination Countries") %>%
  rename(to=split)



# "Crude oil" shipments from India ----------------------------------------

# Kpler shows barely any shipment of crude oil from India,
# while we show quite a lot. Investigating why

voyages_india <- read_csv('http://localhost:8080/v0/voyage?aggregate_by=commodity_origin_iso2,commodity,ship&commodity_origin_iso2=IN&format=csv&date_from=2022-01-01&date_to=2022-12-31&commodity=crude_oil')
voyages_india %>%
  group_by(commodity, ship_type, ship_subtype) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  arrange(desc(value_tonne))

top_ships <- voyages_india %>%
  group_by(ship_imo, ship_name,commodity, ship_type, ship_subtype) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  arrange(desc(value_tonne))

seasearcher <- read_csv('data/seasearcher_oil_ships_20220111.csv')

imos <- top_ships$ship_imo[seq(1,10)]
trades <- kpler.get_trade(from_zones="India",
                          date_from="2022-01-01", date_to="2023-01-01")



