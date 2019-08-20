# Help Dialog Modal
helpButton <- function(id, label = NULL) {
    ns <- NS(id);
    tags$button(
        id = ns("helpButton"),
        class = "btn action-button",
        icon("question-circle"),
        style = '
            padding-top: 0px;
            padding-right: 0px;
            padding-bottom: 0px;
            padding-left: 3px;
            margin-bottom: 2px;
        '
    )
}



# Help Dialog Modal
helpModal <- function(input, output, session, helpParameter, textFile) {
    observeEvent(input$helpButton, {
        showModal(
            modalDialog(
                title = helpParameter,
                footer = NULL,
                size = 'm',
                easyClose = TRUE,
                fluidRow(
                    column(10,
                        includeHTML(textFile),
                        offset = 1
                    )
                )
            )
        )
    })
}
