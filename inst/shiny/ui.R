require(shinydashboard);

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


# Header
header <- dashboardHeader(title = "Salbm")

# Sidebar
sidebar <-  dashboardSidebar(
    sidebarMenu(id = "tabs",
        menuItem("Data", tabName = "data", icon = icon("file")),
        menuItem("Settings", tabName = "settings", icon = icon("cogs")),
        menuItem("Results", tabName = "results", icon = icon("table")),
        actionButton("compute", "Submit Data", style = 'margin-top: 20px; margin-left: 66px;')
    )
)

# Body
body <- dashboardBody(
    tags$style(".shiny-file-input-progress {display: none}"),
    includeHTML("www/color.html"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(

        # Data Tab
        tabItem(tabName = "data",
            fluidRow(
                box(title = "Data",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,

                    # File Input
                    column(3,
                        fluidRow(
                            h6("Upload a CSV File"),
                            helpButton("helpUpload"),
                            style = 'margin-top: 20px;'
                        ),
                        fluidRow(
                            fileInput("file", "",
                                accept = c("text/csv",
                                    "text/comma-separated-values",
                                    "text/plain",
                                    ".csv"
                                )
                            )
                        ),
                        fluidRow(
                            column(6,
                                uiOutput("upload"),
                                actionButton("example",
                                    "Try an Example",
                                    width = '150px',
                                    style = 'margin-bottom: 40px; margin-top: 30px;'
                                ),
                                offset = 2,
                                style = 'text-align: center'
                            )
                        )
                    ),

                    # read.csv parameters
                    column(8,
                        fluidRow(
                            column(3,
                                radioButtons("sep",
                                    "Separator",
                                    c(Comma = ',', Semicolon = ';', Tab = '\t', Space = ' '),
                                    selected = ',',
                                ),
                                offset = 1
                            ),
                            column(3,
                                radioButtons("quote",
                                    "Quote",
                                    c(None = '', 'Double Quote' = '"', 'Single Quote' = "'"),
                                    selected = ''
                                )
                            ),
                            column(3,
                                radioButtons("nastrings",
                                    "NA string",
                                    c('NA' = "NA", '.' = '.'),
                                    selected = 'NA')
                            ),
                            column(2,
                                h6("Other"),
                                checkboxInput(inputId = "header", label = "Header", value = TRUE)
                            ),
                            style = 'margin-top: 20px;'
                        ),
                        fluidRow(
                            column(8,
                                uiOutput("confirm"),
                                offset = 2
                            )
                        )
                    )
                ),

                # Show Input Table After Uploading
                uiOutput("table")
            )
        ),

        # Setting Tab
        tabItem(tabName = "settings",
            fluidRow(
                box(title = "Settings",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    footer = textOutput("msg"),
                    width = 12,

                    # Column Selection
                    column(5,
                        fluidPage(
                            id = 'radio',
                            uiOutput("columns")
                        )
                    ),

                    # Parameters
                    column(7,
                        fluidPage(
                            fluidRow(
                                column(6,
                                    fluidRow(
                                        h6("Number of Trees"),
                                        helpButton("helpTrees"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    numericInput("rf.ntree", "", NULL, min = 1),
                                    fluidRow(
                                        h6("Nodesize"),
                                        helpButton("helpNodesize"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    numericInput("rf.nodesize", "", NULL, min = 1),
                                    fluidRow(
                                        h6("Seed"),
                                        helpButton("helpSeed"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    numericInput("rf.seed", "", NULL)
                                ),
                                column(6,
                                    fluidRow(
                                        h6("Number of Bootstraps"),
                                        helpButton("helpBootstraps"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    numericInput("nbootstraps", "", NULL),
                                    fluidRow(
                                        h6("Sample Size"),
                                        helpButton("helpSampsize"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    numericInput("rf.sampsize", "", NULL, min = 1),
                                    fluidRow(
                                        h6("alphas"),
                                        helpButton("helpAlphas"),
                                        style = 'margin-left: 0px;'
                                    ),
                                    textInput("alphas", "", NULL)
                                )
                            ),
                            column(2,
                                actionButton(
                                    "validate",
                                    "Validate Settings",
                                    style = 'margin-top: 30px;'),
                                offset = 9
                            )
                        )
                    )
                )
            )
        ),

        # Result Tab
        tabItem(tabName = "results",
            uiOutput("result"),

            # Hidden Panel
            conditionalPanel(condition = "input.copmute == 1000",
                textInput("greena", "green", "green"),
                textInput("greenb", "green", "green"),
                textInput("reda", "red", "red"),
                textInput("redb", "red", "red"),
                textInput("normal", "normal", "normal")
            )
        )
    )
)

# Create UI
ui <- dashboardPage(header, sidebar, body);





