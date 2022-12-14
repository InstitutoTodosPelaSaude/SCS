#' dados_gerais UI Function
#'
#' @description ValueBox for hospital attendance numbers
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dados_gerais_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' dados_gerais Server Functions
#'
#' @noRd
mod_dados_gerais_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## ValeuBox for hospital attendance
    AtendHosp <- input %>%
      ## Selecting coluns to be used into the valueBox
      tidytable::select.(dt_atendimento,
                         tx_unidade_organizacional) %>%
      tidytable::filter.(lubridate::year(dt_atendimento)==2022) %>%
      tidytable::summarise.(n = n(),
                            .by = tx_unidade_organizacional) %>%
      tidytable::filter.(tx_unidade_organizacional == "HMEAS") %>%
      tidytable::select.(n)

    ## Returning
    renderText(
      ## Output
      flexdashboard::valueBox(value = AtendHosp,
                              icon = 'fa-briefcase-medical',
                              color = "#ff6555")
    )

  })
}

## To be copied in the UI
# mod_dados_gerais_ui("dados_gerais_1")

## To be copied in the server
# mod_dados_gerais_server("dados_gerais_1")
