
server <- function(input, output, session) {
    source(file.path("server", "tab_counter.R"),  local = TRUE)$value
}
