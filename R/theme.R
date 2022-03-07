palette_power <- function(palette="crea"){
  list(
    crea=c(
      "Other"="#cce7eb",
      "Solar"= "#f6b26b",
      "Wind"= "#8cc9D0",
      "Renewables"= "#8cc9D0",
      "Hydro"= "#35416C",
      "Nuclear"= "#990000",
      "Fossil Gas"= "#cacaca",
      "Coal"="#333333",
      "Thermal"="#666666"
    ))[[palette]]
}


scale_fill_power <- function(){
  scale_fill_manual(values = palette_power())
}