#' UI and server functions to tweak aceEditor so that it works with
#'   shinyStorePlus
#'
#' UI function `ace_ui()` and server function `ace_server()` makes
#' the content written inside [shinyAce::aceEditor()] can be saved in indexeddb.
#' Internally, the module set up a [shiny::textAreaInput()] to mirror the
#' content of the Ace Editor.
#'
#' The internal id for aceEditor is "editor" and the content can be
#' accessed with `session$userData[[_id_]]`.
#'
#' @param id character. ID for the module.
#' @param value character. Default text.
#' @param ... other parameters passed to [shinyAce::aceEditor()]
#' @param debug logical. If TRUE, the internally mirroring textArea will show up.
#'
#' @return `ace_ui()` returns the UI elements. `ace_server()` returns the
#'   reactive value holding the content of the aceEditor.
#' @export
#'
ace_ui <- function(id, value, ..., debug = FALSE) {
  ns <- NS(id)
  hide <- if (debug) function(x) x else shinyjs::hidden

  tagList(
    hide(textAreaInput(ns("mirror"), NULL, value = NULL)),
    shinyAce::aceEditor(
      outputId = ns("editor"),
      value = value,
      ...
    )
  )
}


#' @rdname ace_ui
#' @export
ace_server <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    ## shinyAce::aceEditor -> textAreaInput
    ##   when indexeddb's data are already retrieved.
    ## This is necessary to backup input in because shinyAce::aceEditor because
    ##   shinyStorePlus does not support shinyAce::aceEditor
    observe({
      updateTextAreaInput(
        session,
        inputId = "mirror", value = input$editor
      )
    })

    ## textAreaInput -> shinyAce::aceEditor on start up
    ## We need to watch invalidation of input$mirror at most twice;
    ##   once when initialization of the textAreaInput and the other
    ##   when copied from indexeddb
    cnt <- 0
    o <- observe({
      cnt <<- cnt + 1
      if (cnt > 1) o$destroy()

      if (!is.null(input$mirror)) {
        shinyAce::updateAceEditor(session, "editor", input$mirror)
      }
    }) |>
      bindEvent(
        input$mirror,
        ignoreInit = TRUE
      )

    session$userData[[id]] <- reactive(input$editor)
  })
}

