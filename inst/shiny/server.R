
server <- function(input, output, session) {

    # URL Management
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$tab)) {
            updateNavbarPage(session,
                             "nav-page",
                             selected = query$tab)
        }
    })


    source(file.path("server", "tab_counter.R"),  local = TRUE)$value
    source(file.path("server", "tab_flows.R"),  local = TRUE)$value
    source(file.path("server", "tab_methodology.R"),  local = TRUE)$value
    source(file.path("server", "tab_about.R"),  local = TRUE)$value

    # shinyURL.server(session)
}
