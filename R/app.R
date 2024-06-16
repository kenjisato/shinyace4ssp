#' Run the demo app
#'
#' This app demonstrates how
#'
#' @param debug logical
#'
#' @return nothing. This function runs the app.
#' @export
#'
#' @examples
#' \dontrun{
#'   run_app()
#' }
run_app <- function(debug = FALSE) {

  ui <- fluidPage(
    shinyStorePlus::initStore(),
    shinyjs::useShinyjs(),
    ace_ui("ace", value = "", debug = debug),
    verbatimTextOutput("content")
  )

  server <- function(input, output, session) {

    ace_server("ace")

    output$content <-
      renderText(
        paste(session$userData$ace(), collapse = "\n")
      )

    shinyStorePlus::setupStorage(
      appId = "ace-demo",
      inputs = TRUE
    )
  }

  shinyApp(ui, server)

}
