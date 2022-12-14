#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ## ValueBox for hospital attendance
  callModule(
    mod_dados_gerais_server("dados_gerais_1")
  )

}
