# nocov start

#' Use biproportional apportionment interactively in a shiny app
#'
#' @param votes_matrix Optional votes_matrix to load upon start
#' @param district_seats Optional district_seats to load upon start
#' @returns Calling the function starts the shiny app
#' @examples
#' if(interactive()) {
#'     # You need to have the packages 'shiny' and 'shinyMatrix' installed to run the app
#'     run_app()
#'
#'     # It's possible to load a matrix with the app
#'     run_app(uri2020$votes_matrix, uri2020$seats_vector)
#' }
#' @importFrom stats setNames
#' @export
run_app = function(votes_matrix = NULL, district_seats = NULL) {
    # load packages / "import" ####
    if(!requireNamespace("shiny", quietly = TRUE)) {
        stop("Please install shiny: install.packages('shiny')", call. = FALSE)
    }
    if(!requireNamespace("shinyMatrix", quietly = TRUE)) {
        stop("Please install shinyMatrix: install.packages('shinyMatrix')", call. = FALSE)
    }
    tags = shiny::tags
    fluidRow = shiny::fluidRow
    column = shiny::column
    observeEvent = shiny::observeEvent
    sidebarPanel = shiny::sidebarPanel

    # default parameters ####
    assert(!is.null(votes_matrix) == !is.null(district_seats))
    if(is.null(votes_matrix)) {
        base_votes_matrix = shiny_create_empty_votes_matrix(3, 4)
    } else {
        base_votes_matrix = votes_matrix
    }
    rm(votes_matrix)
    if(is.null(district_seats)) {
        base_district_seats_mtrx = shiny_create_seats_matrix(base_votes_matrix)
        base_district_seats = setNames(c(base_district_seats_mtrx), colnames(base_district_seats_mtrx))
    } else {
        base_district_seats = prep_district_seats(district_seats, base_votes_matrix, "district_seats", "votes_matrix")
        base_district_seats_mtrx = shiny_create_seats_matrix(base_votes_matrix, base_district_seats)
    }
    rm(district_seats)

    # UI ####
    ui = shiny::fluidPage(
        tags$head(tags$style(type = "text/css", "#CSVerrors {color: red; font-weight: bold}")),
        shiny::titlePanel("Biproportional Apportionment"),
        # UI input ####
        fluidRow(
            # input matrix ####
            column(
                width = 9,
                tags$h3(tags$strong("Input")),
                shinyMatrix::matrixInput(
                    inputId = "votesMatrix",
                    label = "Vote Matrix (click into matrix to edit votes and names)",
                    value = base_votes_matrix,
                    class = "numeric",
                    cols = list(
                        names = TRUE,
                        editableNames = TRUE
                    ),
                    rows = list(
                        names = TRUE,
                        editableNames = TRUE
                    )
                ),
                shinyMatrix::matrixInput(
                    inputId = "seatsMatrix",
                    label = "Seats per district",
                    value = base_district_seats_mtrx,
                    class = "numeric",
                    cols = list(
                        names = FALSE
                    ),
                    rows = list(
                        names = TRUE
                    )
                ),
                shiny::textOutput("CSVerrors")),
            # input options ####
            column(
                width = 3,
                sidebarPanel(
                    width = 12,
                    shiny::selectInput("load_example", "Load an example",
                                       c("...", "Zug 2018", "Uri 2020", "Wikipedia EN", "Wikipedia DE")),
                    tags$hr(),
                    shiny::fileInput("uploadCSV", "... or upload a CSV file with votes", accept = ".csv"),
                    tags$hr(),
                    tags$h5(tags$strong("... or create an empty table"), style = "margin-bottom:1em"),
                    shiny::numericInput("n_cols", "number of district", 4, min = 2),
                    shiny::numericInput("n_rows", "number of parties", 3, min = 2),
                    shiny::actionButton("run_update_matrix", "set table dimensions"),
                    shiny::checkboxInput("set_seats_per_district", "define seats per district", TRUE)
                )
            )),
        tags$hr(),
        # UI output ####
        fluidRow(
            # output matrix ####
            column(
                width = 9,
                tags$h3(tags$strong("Output")),
                tags$p(shiny::checkboxInput("show_seat_totals", "Show seat totals", FALSE)),
                tags$div(shiny::tableOutput("biproporz_result"))
            ),
            # output options ####
            column(
                width = 3,
                sidebarPanel(
                    width = 12,
                    tags$h4(tags$strong("Apportionment parameters"), style = "margin-bottom:1em"),
                    shiny::numericInput("quorum_districts", "Quorum (districts)", 0, min = 0),
                    shiny::numericInput("quorum_total", "Quorum (total)", 0, min = 0),
                    shiny::checkboxInput("quorum_all", "Both quorums necessary", FALSE),
                    shiny::checkboxInput("weight_votes", "Weight votes", TRUE),
                    shiny::checkboxInput("wto", "district winner must have at least one seat", FALSE)
                )
            )
        )
    )

    # server ####
    server = function(input, output, session) {
        vals = shiny::reactiveValues(votes_matrix = base_votes_matrix,
                                     seats_vector = base_district_seats)

        # run biproportional apportionment ####
        output$biproporz_result <- shiny::renderTable(
            result(),
            digits = 0,
            rownames = TRUE)

        result = eventReactive({
            c(vals$votes_matrix, vals$seats_vector,
              input$weight_votes, input$show_seat_totals, input$set_seats_per_district,
              input$quorum_districts, input$quorum_total, input$quorum_all)
        }, {
            if(sum(vals$votes_matrix) == 0 ||
               sum(vals$seats_vector) == 0 ||
               (length(vals$seats_vector) > 1 && length(vals$seats_vector) != NCOL(vals$votes_matrix)) ||
               (any(nchar(colnames(vals$votesMatrix)) == 0))) {
                return(NULL)
            }
            .quorum = shiny_get_quorum_function(input$quorum_districts, input$quorum_total, input$quorum_all)

            if(!input$wto) {
                .method = "round"
            } else {
                .method = "wto"
            }

            if(input$set_seats_per_district) {
                .seats = vals$seats_vector
            } else {
                .seats = sum(vals$seats_vector)
            }

            if(length(.seats) > 1) {
                names(.seats) <- colnames(vals$votes_matrix)
            }

            bp = tryCatch(
                biproporz(vals$votes_matrix, .seats,
                          quorum = .quorum,
                          weight_votes = input$weight_votes,
                          method = .method),
                error = function(e) {
                    em = matrix(e$message)
                    colnames(em) <- "Error"
                    rownames(em) <- " "
                    em
                })

            # add seat totals
            if(!is.character(bp) && input$show_seat_totals) {
                bpt <- matrix(NA, nrow = nrow(bp) + 1, ncol = ncol(bp) + 1)
                bpt[2:nrow(bpt), 2:ncol(bpt)] <- bp
                bpt[1, seq_len(ncol(bpt))] <- c(sum(bp), colSums(bp))
                bpt[2:nrow(bpt), 1] <- rowSums(bp)
                colnames(bpt) <- c("TOTAL", colnames(bp))
                rownames(bpt) <- c("TOTAL", rownames(bp))
                bp <- bpt
            }

            return(bp)
        })

        # convert reactive matrix inputs ####
        observeEvent(input$seatsMatrix, {
            vals$seats_vector <- c(input$seatsMatrix)
            result()
        }, ignoreInit = TRUE)

        observeEvent(input$votesMatrix, {
            vals$votes_matrix <- input$votesMatrix
            result()
        }, ignoreInit = TRUE)

        # update inputs ####
        observeEvent(input$run_update_matrix, {
            vals$votes_matrix <- shiny_create_empty_votes_matrix(input$n_rows, input$n_cols)
            vals$seats_vector <- setNames(rep(0, input$n_cols), colnames(vals$votes_matrix))
            shiny::updateSelectInput(session, "load_example", selected = "...")

            update_votesMatrix(vals$votes_matrix)
            update_seatsMatrix(vals$seats_vector)
        }, ignoreInit = TRUE)

        update_votesMatrix = function(.votes_matrix) {
            shinyMatrix::updateMatrixInput(session, "votesMatrix", .votes_matrix)
        }

        update_seatsMatrix = function(.seats) {
            if(!is.matrix(.seats)) {
                if(length(.seats) == 1L) {
                    .seats <- matrix(.seats, 1, 1,
                                     dimnames = list("seats (total)", NULL))
                } else {
                    .seats = matrix(.seats, nrow = 1,
                                    dimnames = list("seats", NULL))
                }
            }
            shinyMatrix::updateMatrixInput(session, "seatsMatrix", .seats)
        }

        # seats per district option ####
        observeEvent(input$set_seats_per_district, {
            if(input$set_seats_per_district) {
                if(length(vals$seats_vector) == 1) {
                    update_seatsMatrix(rep(0, ncol(vals$votes_matrix)))
                }
            } else {
                if(length(vals$seats_vector) > 1) {
                    update_seatsMatrix(sum(vals$seats_vector))
                }
            }
        }, ignoreInit = TRUE)

        # Load examples ####
        observeEvent(input$load_example, {
            if(input$load_example == "...") {
                return()
            } else if(input$load_example == "Zug 2018") {
                update_votesMatrix(shinyapp_examples$zug_2018$votes)
                update_seatsMatrix(shinyapp_examples$zug_2018$seats)
                set_inputs(quorum_districts = 0.05, quorum_total = 0.03, wto = TRUE)
                set_dim_input(8, 11)
            } else if(input$load_example == "Uri 2020") {
                update_votesMatrix(shinyapp_examples$uri_2020$votes)
                update_seatsMatrix(shinyapp_examples$uri_2020$seats)
                set_inputs()
                set_dim_input(4, 4)
            } else if(input$load_example == "Wikipedia EN") {
                update_votesMatrix(shinyapp_examples$wikipedia_en$votes)
                update_seatsMatrix(shinyapp_examples$wikipedia_en$seats)
                set_inputs(weight_votes = FALSE, set_seats_per_district = FALSE)
                set_dim_input(3, 3)
            } else if(input$load_example == "Wikipedia DE") {
                update_votesMatrix(shinyapp_examples$wikipedia_de$votes)
                update_seatsMatrix(shinyapp_examples$wikipedia_de$seats)
                set_inputs()
                set_dim_input(3, 3)
            }
        }, ignoreInit = TRUE)

        set_inputs = function(quorum_districts = 0, quorum_total = 0,
                              weight_votes = TRUE, set_seats_per_district = TRUE,
                              wto = FALSE) {
            shiny::updateCheckboxInput(session, "weight_votes", value = weight_votes)
            shiny::updateNumericInput(session, "quorum_districts", value = quorum_districts)
            shiny::updateNumericInput(session, "quorum_total", value = quorum_total)
            shiny::updateCheckboxInput(session, "set_seats_per_district", value = set_seats_per_district)
            shiny::updateCheckboxInput(session, "wto", value = wto)
        }

        set_dim_input = function(n_row, n_col) {
            shiny::updateNumericInput(session, "n_row", value = n_row)
            shiny::updateNumericInput(session, "n_col", value = n_col)
        }

        uploaded_data <- reactive({
            req(input$uploadCSV)
            check_uploaded_csv(read.csv(input$uploadCSV$datapath))
        })

        output$CSVerrors <- renderText({
            csv = uploaded_data()
            if(is.character(csv)) {
                return(csv)
            }
            NULL
        })

        observeEvent(uploaded_data(), {
            csv = uploaded_data()
            if(is.character(csv)) {
                return(NULL)
            }

            votes = as.matrix(csv[, 2:ncol(csv)])
            pnames = csv[[1]]
            if(pnames[length(pnames)] != "") {
                seats = setNames(rep(0, ncol(votes)), colnames(votes))
                rownames(votes) <- pnames
            } else {
                seats = votes[nrow(votes), ]
                votes <- votes[-nrow(votes), ]
                rownames(votes) <- pnames[-length(pnames)]
            }

            vals$votes_matrix <- votes
            vals$seats_districts <- seats

            update_votesMatrix(vals$votes_matrix)
            update_seatsMatrix(vals$seats_districts)
            set_dim_input(nrow(votes), ncol(votes))
            shiny::updateSelectInput(session, "load_example", selected = "...")
            result()
            return(NULL)
        })

        # download file ####
        output$download_csv <- shiny::downloadHandler(
            filename = function() {
                paste0("biproportional-", Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(run_biproporz(), file)
            })
    }

    # Run the application ####
    shiny::shinyApp(ui = ui, server = server)
}

# nocov end

# helper functions ####
shiny_create_empty_votes_matrix = function(nrows, ncols) {
    assert(nrows >= 0)
    assert(ncols >= 0)
    m = matrix(0, nrows, ncols)
    colnames(m) <- paste0("District ", 1:ncols)
    rownames(m) <- paste0("Party ", 1:nrows)
    return(m)
}

shiny_create_seats_matrix = function(votes_matrix,
                                     district_seats = setNames(rep(0, ncol(votes_matrix)), colnames(votes_matrix))) {
    assert(is.matrix(votes_matrix))
    assert(is.numeric(district_seats))
    if(length(district_seats) == 1L) {
        district_seats_matrix = matrix(district_seats, 1, 1,
                                       dimnames = list("seats", "total"))
    } else {
        district_seats_matrix = matrix(district_seats, nrow = 1,
                                       dimnames = list("seats", names(district_seats)))
    }
    return(district_seats_matrix)
}

shiny_get_quorum_function = function(q_districts, q_total, q_all) {
    if(length(q_districts) == 0) q_districts <- 0
    if(length(q_total) == 0) q_total <- 0
    if(length(q_all) == 0) q_all <- TRUE
    assert_num1(q_districts)
    assert_num1(q_total)
    if(q_districts > 0 && q_total > 0) {
        assert_bool1(q_all)
        if(q_all) {
            return(quorum_all(any_district = q_districts, total = q_total))
        } else {
            return(quorum_any(any_district = q_districts, total = q_total))
        }
    }
    if(q_districts > 0) {
        return(quorum_any(any_district = q_districts))
    }
    if(q_total > 0) {
        return(quorum_any(total = q_total))
    }
    return(NULL)
}

check_uploaded_csv = function(csv) {
    if(nrow(csv) < 2) {
        return("Input CSV must have at least 2 rows (excluding header with district names)")
    }
    if(nrow(csv) < 2) {
        return("Input CSV must have at least 2 columns (one of them party names)")
    }
    if(sum(csv[[1]] == "") > 1) {
        return("Input CSV must have party names in first column")
    }
    if(anyNA(csv[2:ncol(csv)])) {
        return("Input CSV cannot have missing values")
    }
    return(csv)
}
