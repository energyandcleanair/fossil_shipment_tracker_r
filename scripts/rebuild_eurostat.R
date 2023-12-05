#!/usr/bin/env Rscript

source("./R/price_models_eurostat.R")
source("./R/price_utils.R")
source("./R/source_oilprice.R")

price_models_eurostat.build(production = T)
