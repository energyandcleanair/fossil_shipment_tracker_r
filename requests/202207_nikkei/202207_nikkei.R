library(tidyverse)
library(lubridate)
library(tidytext)

folder <- 'requests/202207_nikkei'
colours <- list(coal="#351c75",
                gas="#f6b26b",
                oil="#741b47")

# Specifically, I would like to have the data in this graph as of the 24th of March, April, May, June, and July, respectively. I¡@would also appreciate it if you could provide us with the amounts of oil, petrochemical gas, and coal, respectively.

counter <- read_csv('https://api.russiafossiltracker.com/v0/counter?format=csv')
dates <- as.Date(sprintf("2022-%0d-24", seq(2,7)))

counter$month <- month(counter$date) + (day(counter$date) >= 24)
counter$month_str <- sprintf('2022-%02d-24 to 2022-%02d-23', counter$month-1, counter$month)



counter_month <- counter %>%
  group_by(month, month_str, commodity_group, destination_iso2, destination_country, destination_region) %>%
  summarise_if(is.numeric, sum, na.rm=T) %>%
  arrange(month, destination_country, commodity_group)

counter_month <- counter_month %>%
  mutate(destination_country=recode(destination_country,
                                    `United Arab Emirates`='UAE'))

top_importers <- counter_month %>%
  group_by(month, destination_iso2, destination_country) %>%
  summarise(value_eur=sum(value_eur, na.rm=T)) %>%
  group_by(month) %>%
  top_n(20, wt=value_eur) %>%
  arrange(desc(value_eur)) %>%
  # select(-c(value_eur)) %>%
  # group_by(month) %>%
  arrange(desc(value_eur)) %>%
  mutate(rank=row_number(),
         rank_str=paste0(rank, "_", month, "__", destination_country)) %>%
  mutate(rank_str = factor(rank_str,
         levels = .$rank_str)) %>%
  select(-c(value_eur))


ggplot(counter_month %>%
         inner_join(top_importers) %>%
         ungroup() %>%
         filter(month<8) %>%
         mutate(rank_str=factor(rank_str, levels=rev(levels(.$rank_str))))) +
  geom_bar(aes(value_eur/1e6,
               rank_str,
               fill=commodity_group),
           stat='identity',
           position=position_stack())  +
  scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
  scale_y_discrete(labels=function(x){
    gsub(".*__(.*)", "\\1", x)
    }) +
  scale_fill_manual(values=colours) +
  facet_wrap(~month_str, scales='free_y') +
  rcrea::theme_crea() +
  labs(title='Largest importers of fossil fuels from Russia',
       y=NULL,
       x='mnEUR')

ggsave(file.path(folder, 'counter_month.jpg'), width=12, height=6)


write_csv(counter_month, file.path(folder,'counter_month.csv'))


# Could you tell me how Russia's energy export volumes and export revenues in euro changed in the month to July 23th compared to the month to March 23th ?
# I would greatly appreciate it if you could provide me with actual numbers for both export volumes and export revenues, so that I can include them in my articles, such as bar graphs.
# I would appreciate it if you could give me a breakdown by gas, oil, and coal.
comparison <- counter_month %>%
  group_by(month, month_str, commodity_group) %>%
  summarise_at(c('value_eur','value_tonne','value_usd'), sum, na.rm=T)

write_csv(comparison, file.path(folder,'comparison_july_march.csv'))

comparison_wide <- comparison %>%
  filter(month %in% c(3,7)) %>%
  select(month, value_eur, value_tonne)
