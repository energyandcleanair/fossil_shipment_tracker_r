
server <- function(input, output, session) {

    # URL Management
    # observe({
    #     query <- parseQueryString(session$clientData$url_search)
    #     if(!is.null(query$tab)) {
    #         updateNavbarPage(session,
    #                          "nav-page",
    #                          selected = query$tab)
    #     }
    # })




    # source(file.path("server", "section_diagnostic.R"),  local = TRUE)$value
    # source(file.path("server", "section_mailchimp.R"),  local = TRUE)$value
    source(file.path("server", "section_methodology.R"),  local = TRUE)$value
    source(file.path("server", "section_about.R"),  local = TRUE)$value

    source(file.path("server", "section_counter.R"),  local = TRUE)$value
    source(file.path("server", "section_payments.R"),  local = TRUE)$value
    source(file.path("server", "section_monthly.R"),  local = TRUE)$value
    source(file.path("server", "section_countries.R"),  local = TRUE)$value

    # source(file.path("server", "tab_trade.R"),  local = TRUE)$value
    # source(file.path("server", "tab_flows.R"),  local = TRUE)$value
    # source(file.path("server", "tab_shipment.R"),  local = TRUE)$value
    # source(file.path("server", "tab_methodology.R"),  local = TRUE)$value
    # source(file.path("server", "tab_about.R"),  local = TRUE)$value

    # shinyURL.server(session)
}
