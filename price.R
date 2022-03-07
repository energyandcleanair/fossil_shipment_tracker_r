get_gas_price <- function(date_from="2016-01-01"){
  # install.packages("Quandl")
  # library(Quandl)
  # install.packages("tidyquant")
  library(tidyquant)
  p <- getSymbols("TTF=F",
                  from = date_from,
                  warnings = FALSE,
                  auto.assign = F)
  tibble(date=index(p),
         price=as.numeric(coredata(p$`TTF=F.Adjusted`)[]),
         unit_price="EUR",
         unit_quantity="MWh")
}
