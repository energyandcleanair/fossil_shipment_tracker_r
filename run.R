library(russiacounter)
library(argparse)

parser <- ArgumentParser()
parser$add_argument("--rebuild_prices", type = "logical", default = FALSE)
args <- parser$parse_args()

russiacounter::update_counter(rebuild_prices = args$rebuild_prices)
