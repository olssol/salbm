require(dplyr);
require(DT);

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

# Generate Radio Buttons For Column Selection
genRadio <- function(col.names) {
    element <- list(
        HTML(
            "<div class='row'>
            <span class='smdl'></span>
            <span class='smdl'>Treatment</span>
            <span class='smdl'>Outcomes</span>
            <span class='smdlt'>Ignore</span>
            </div>"
        )
    );


    for (i in 1:length(col.names)) {
        if (i == 1) {
            sel <- "trt";
        } else {
            sel <- "use";
        }

        basename <- paste0("colRadio", i);

        element[[i+1]] <- fluidRow(
            column(1,
                h5(col.names[i]),
                align = "center"
            ),
            column(11,
                radioButtons(basename, "",
                    c(" " = "trt", " " = "use", " " = "ignore"),
                    selected = sel,
                    inline = TRUE
                )
            )
        );
    }
    element
}


server <- (function(input, output, session) {

    # Reactive Values
    data <- reactiveValues();
    data$trt <- NULL;
    data$trtlev <- NULL;
    data$rf.ntree <- NULL;
    data$rf.seed <- NULL;
    data$rf.sampsize <- NULL;
    data$rf.nodesize <- NULL;
    data$nbootstraps <- NULL;
    data$alphas <- NULL;

    data$rawData <- NULL;
    data$dataframe <- NULL;
    data$results <- NULL;


    # Validation Values
    v <- reactiveValues();
    v$v <- NULL;
    v$u <- NULL;


    ## Dialog Modals
    callModule(helpModal, "helpUpload", "Upload", "www/help_upload.html");
    callModule(helpModal, "helpTrees", "Number of Trees", "www/help_tree.html");
    callModule(helpModal, "helpNodesize", "Nodesize",  "www/placeholder.html");
    callModule(helpModal, "helpSeed", "Seed",  "www/placeholder.html");
    callModule(helpModal, "helpBootstraps", "Number of Bootstraps", "www/placeholder.html");
    callModule(helpModal, "helpSampsize", "Sample Size",  "www/placeholder.html");
    callModule(helpModal, "helpAlphas", "Alphas", "www/placeholder.html");


    # Initialization Of UIs
    output$columns <- renderUI({
        column(12,
            infoBox("Waiting for Data...",
                "Please upload data first.",
                icon = icon("file"),
                width = 11,
                fill = TRUE
            ),
            style = 'margin-top: 45px;'
        )
    })

    output$msg <- renderText({
        return (NULL);
    })

    output$table <- renderUI({
        return (NULL);
    })

    output$upload <- renderUI({
        fluidRow(
            style = 'margin-top: 54px;'
        )
    })

    output$result <- renderUI({
        column(12,
            infoBox("Missing Information.",
                "Please conplete Data and Settings first.",
                icon = icon("exclamation"),
                color = 'yellow'
            ),
            offset = 4
        )
    })

    observeEvent(input$file, {
        if (is.null(input$file)) {

            output$upload <- renderUI({
                fluidRow(
                    style = 'margin-top: 54px;'
                )
            })

        } else {

            output$upload <- renderUI({
                actionButton("upload",
                    "Upload",
                    width = '150px',
                    style = 'margin-top: 20px;'
                )
            })
        }
    })


    # Upload File
    observeEvent(input$upload, {
        v$u <- NULL;
        v$v <- NULL;
        if (is.null(input$file)) {
            v$u <- FALSE;
        } else {
            data$rawData <- read.csv(
                file = input$file$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote,
                na.strings = input$nastrings
            )
            v$u <- TRUE;
        }
    })


    # Set Up Settings
    observeEvent(v$u, {
        if (is.null(v$u)) return (NULL);

        if (v$u == TRUE) {
            updateTextInput(session, "greena", "green", "green");
            updateTextInput(session, "normal", "normal", "normal");

            inputName <- input$file;

            output$confirm <- renderUI({
                infoBox(title = "File Uploaded!",
                    value = paste(inputName, "has been uploaded!"),
                    fill = TRUE,
                    width = 12,
                    icon = icon("file-upload"),
                    color = 'green'
                )
            })

            output$columns <- renderUI ({
                do.call(fluidPage, genRadio(names(data$rawData)))
            })

            output$msg <- renderText({
                return (NULL);
            })

            # Show RawData
            output$table <- renderUI({
                fluidRow(
                    column(12,
                        box(
                            title = "Data",
                            status = "success",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            renderDataTable({
                                datatable(data$rawData,
                                    options = list(searching = FALSE)
                                )
                            })
                        )
                    )
                )
            })

        } else if (v$u == FALSE) {
            updateTextInput(session, "reda", "red", "red");
            updateTextInput(session, "normal", "normal", "normal");

            output$confirm <- renderUI({
                infoBox(title = "File Not Found!",
                    value = "Unable to locate the file.",
                    fill = TRUE,
                    width = 12,
                    icon = icon("question"),
                    color = 'red'
                )
            })

            output$columns <- renderUI({
                column(12,
                    infoBox("Waiting for Data...",
                        "Please upload data first.",
                        icon = icon("file"),
                        width = 11,
                        fill = TRUE
                    ),
                    style = 'margin-top: 45px;'
                )
            })

            output$msg <- renderText({
                return (NULL);
            })

            output$table <- renderUI({
                return (NULL);
            })
        }
    })


    # Validation
    observeEvent(input$validate, {

        # If a Data Does Not Exist
        if (is.null(data$rawData)) {
            v$v <- FALSE;
            return (NULL);
        }

        # Collect Result From genRadio
        col.result <- c();
        for (i in 1:NCOL(data$rawData)) {
            current <- input[[paste0("colRadio", i)]];
            col.result <- c(col.result, current);
        }

        # Test Number of Treatment/Use/Ignore
        if (length(which(col.result == "use")) < 2 || length(which(col.result == "trt")) != 1)
        {
            v$v <- FALSE;
            return (NULL);
        }

        #  Make New Dataframe.
        newData <- c(data$rawData[,which(col.result == "trt")]);
        for (i in which(col.result == "use")){
            newData <- cbind(newData, data$rawData[,i]);
            colnames(newData)[NCOL(newData)] <- colnames(data$rawData)[i];
        }
        colnames(newData)[1] <- colnames(data$rawData)[which(col.result == "trt")];
        newData <- as.data.frame(newData);

        # Check if trt Column Only Contains Two Unique Integers
        if (length(unique(newData[,1])) != 2) {
            v$v <- FALSE;
            return (NULL);
        }

        # Check if use Column only Contains 0, 1, or NA
        for (i in 2:NCOL(newData)) {
            if (!all(!is.na(match(newData[,i], c(1, 0, NA))))) {
                v$v <- FALSE;
                return (NULL);
            }
        }

        # Check For NA in Settings and Invalid Settings
        if (is.na(input$rf.ntree) ||
            is.na(input$rf.seed) ||
            is.na(input$rf.sampsize) ||
            is.na(input$rf.nodesize) ||
            is.na(input$nbootstraps) ||
            input$rf.ntree <= 0 ||
            input$rf.sampsize <= 0 ||
            input$rf.nodesize <= 0 ||
                length(
                    as.numeric(strsplit(input$alphas, ",")[[1]])
                    [!is.na(as.numeric(strsplit(input$alphas, ",")[[1]]))]
                )   != 2
            )
        {
            v$v <- FALSE;
            return (NULL);
        }

        data$dataframe <- newData;
        data$trt <- colnames(newData)[1];
        data$trtlev <- unique(newData[,1]);
        data$rf.ntree <- input$rf.ntree;
        data$rf.seed <- input$rf.seed;
        data$rf.sampsize <- input$rf.sampsize;
        data$rf.nodesize <- input$rf.nodesize;
        data$nbootstraps <- input$nbootstraps;
        data$alphas <- sort(as.numeric(strsplit(input$alphas, ",")[[1]]))[1]:
            sort(as.numeric(strsplit(input$alphas, ",")[[1]]))[2];

        v$v <- TRUE
    })


    # Comfirming Validation
    observeEvent(v$v, {
        if (is.null(v$v))
            return(NULL);

        if (v$v == TRUE) {
            updateTextInput(session, "greenb", "green", "green");
            output$msg <- renderText({
                "Validation successful! Please click on 'Submit Data' to get your results.";
            })

        } else {

            updateTextInput(session, "redb", "red", "red");

            output$msg <- renderText({
                "Validation failed. Please make sure your data and settings meet all the requirements.";
            })
        }
    })


    # Example
    observeEvent(input$example, {
        if (input$example == 0) return (NULL);
        if (is.null(input$example)) return (NULL);

        data("salbmData");

        data$rawData <- salbmData;
        data$dataframe <- salbmData;

        updateTextInput(session, "greena", "green", "green");

        output$confirm <- renderUI({
            infoBox(title = "File Uploaded!",
                value = "Example data 'salbmData' has been loaded!",
                fill = TRUE,
                width = 12,
                icon = icon("file-upload"),
                color = 'green'
            )
        })

        output$table <- renderUI({
            fluidRow(
                column(12,
                    box(
                        title = "Data",
                        status = "success",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 12,
                        renderDataTable({
                            datatable(data$rawData,
                                options = list(searching = FALSE)
                            )
                        })
                    )
                )
            )
        })

        output$columns <- renderUI ({
            do.call(fluidPage, genRadio(names(isolate(data$dataframe))))
        })

        updateNumericInput(session, "rf.ntree", "", value = 25);
        updateNumericInput(session, "rf.nodesize", "", value = 3);
        updateNumericInput(session, "rf.seed", "", value = -172);
        updateNumericInput(session, "nbootstraps", "", value = 5);
        updateNumericInput(session, "rf.sampsize", "", value = 90);
        updateTextInput(session, "alphas", "", value = "-1, 1");


        v$v <- TRUE;

        data$trt <- colnames(data$dataframe)[1];
        data$trtlev <- unique(data$dataframe[,1]);
        data$rf.ntree <- 25;
        data$rf.seed <- -172;
        data$rf.sampsize <- 90;
        data$rf.nodesize <- 3;
        data$nbootstraps <- 5;
        data$alphas <- -1:1;
    })


    # Get The Results
    observeEvent(input$compute, {
        if (input$compute == 0) return (NULL);
        if (is.null(input$compute)) return (NULL);

        if (is.null(v$v) || v$v == FALSE) {
            showModal(
                modalDialog(
                    title = "Not Validated",
                    "Please complete validation steps first.",
                    footer = NULL,
                    size = 's',
                    easyClose = TRUE
                )
            )
            return (NULL);
        }

        showModal(
            modalDialog(
                title = "Computing with Salbm...",
                fluidRow(
                    column(4,
                        icon("spinner", "fa-spin fa-4x"),
                        offset = 4
                    )
                ),
                footer = NULL,
                size = 's'
            )
        )

        print("Data:");
        print(data$dataframe);
        print(paste("trt:", data$trt));
        print(paste("trtlev:", paste(data$trtlev, collapse=" ")));
        print(paste("rf.ntree:", data$rf.ntree));
        print(paste("rf.seed:", data$rf.seed));
        print(paste("rf.sampsize:", data$rf.sampsize));
        print(paste("rf.nodesize:", data$rf.nodesize))
        print(paste("nbootstraps:", data$nbootstraps));
        print(paste("alphas:", paste(data$alphas, collapse=" ")));

        data$result <- salbm(data = data$dataframe,
            trt = data$trt,
            trtlev = data$trtlev,
            rf.ntree = data$rf.ntree,
            rf.seed = data$rf.seed,
            rf.sampsize = data$rf.sampsize,
            rf.nodesize = data$rf.nodesize,
            nbootstraps = data$nbootstraps,
            alphas = data$alphas
        );

        output$result <- renderUI({
            tabBox(title = "Results",
                width = 12,

                tabPanel("Results1",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$Results1),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$Results1)), digits = 6)
                    })
                ),

                tabPanel("Results2",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$Results2),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$Results2)), digits = 6)
                    })
                ),

                tabPanel("ResultsD",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$ResultsD),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$ResultsD)), digits = 6)
                    })
                ),

                tabPanel("bootstraps1",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$bootstraps1),
                            options = list(searching = FALSE, dom = 'ft')
                        )  %>%
                        formatRound(columns = colnames(isolate(data$result$bootstraps1)), digits = 6)
                    })
                ),

                tabPanel("bootstraps2",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$bootstraps2),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$bootstraps2)), digits = 6)
                    })
                ),

                tabPanel("bootstrapsD",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$bootstrapsD),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$bootstrapsD)), digits = 6)
                    })
                ),

                tabPanel("CI1",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$CI1),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$CI1)), digits = 6)
                    })
                ),

                tabPanel("CI2",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$CI2),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$CI2)), digits = 6)
                    })
                ),

                tabPanel("CID",
                    DT::renderDataTable({
                        DT::datatable(isolate(data$result$CID),
                            options = list(searching = FALSE, dom = 'ft')
                        ) %>%
                        formatRound(columns = colnames(isolate(data$result$CID)), digits = 6)
                    })
                )
            )
        })

        removeModal();
        updateTabItems(session, "tabs", selected = "results");
    })
})
