# nocov start

#' Use biproportional apportionment interactively in a shiny app
#'
#' @param votes_matrix optional votes_matrix to load upon start
#' @param district_seats optional district_seats to load upon start
#' @returns Calling the function starts the shiny app
#' @examples
#' if(interactive()){
#'     # You need to have the packages 'shiny' and 'shinyMatrix' installed to run the app
#'     run_app()
#'
#'     # It's possible to load a matrix with the app
#'     run_app(uri2020$votes_matrix, uri2020$seats_vector)
#' }
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

    apport_methods_choices = unique(unname(unlist(proporz_methods)))[c(2,1,3:6)]

    # UI ####
    ui = shiny::fluidPage(
        shiny::titlePanel("Biproportional Apportionment"),
        # UI input ####
        fluidRow(
            # input matrix ####
            column(9,
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
                           names = TRUE
                       ),
                       rows = list(
                           names = TRUE
                       )
                   )),
            # input options ####
            column(width = 3,
                   sidebarPanel(width = 12,
                                shiny::selectInput("load_example", "Load example", c("...", "Zug 2018", "Uri 2020", "Wikipedia EN", "Wikipedia DE")),
                                tags$hr(),
                                tags$h4(tags$strong("Edit input table"), style = "margin-bottom:1em"),
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
            column(9,
                   tags$h3(tags$strong("Output")),
                   tags$p(shiny::checkboxInput("show_seat_totals", "Show seat totals", FALSE)),
                   tags$div(shiny::tableOutput("biproporz_result"))
            ),
            # output options ####
            column(3,
                   sidebarPanel(width = 12,
                                tags$h4(tags$strong("Apportionment parameters"), style = "margin-bottom:1em"),
                                shiny::numericInput("quorum_districts", "Quorum (districts)", 0, min = 0),
                                shiny::numericInput("quorum_total", "Quorum (total)", 0, min = 0),
                                shiny::checkboxInput("quorum_all", "Both quorums necessary", FALSE),
                                shiny::checkboxInput("use_list_votes", "Use list votes", TRUE),
                                shiny::checkboxInput("wto", "district winner must have at least one seat", FALSE),
                   )
            )
        )
    )

    # server ####
    server = function(input, output, session) {

        vals = shiny::reactiveValues(votes_matrix = base_votes_matrix,
                                     seats_districts = base_district_seats,
                                     seats_total = sum(base_district_seats))

        # run biproportional apportionment ####
        output$biproporz_result <- shiny::renderTable(
            run_biproporz(),
            digits = 0,
            rownames = TRUE)

        run_biproporz = function() {
            if(sum(vals$votes_matrix) == 0) return(NULL)
            if(sum(vals$seats_districts) == 0) return(NULL)
            if(sum(vals$seats_total) == 0) return(NULL)
            if(any(nchar(colnames(input$votesMatrix)) == 0)) return(NULL)
            .quorum = shiny_get_quorum_function(input$quorum_districts, input$quorum_total, input$quorum_all)

            if(!input$wto) {
                .method = "round"
            } else {
                .method = "wto"
            }

            if(input$set_seats_per_district) {
                .seats = vals$seats_districts
            } else {
                .seats = vals$seats_total
            }

            bp = biproporz(vals$votes_matrix, .seats,
                           quorum = .quorum,
                           use_list_votes = input$use_list_votes,
                           method = .method)

            # add seat totals
            if(input$show_seat_totals) {
                bpt <- matrix(NA, nrow = nrow(bp)+1, ncol = ncol(bp)+1)
                bpt[2:nrow(bpt),2:ncol(bpt)] <- bp
                bpt[1,1:ncol(bpt)] <- c(sum(bp), colSums(bp))
                bpt[2:nrow(bpt),1] <- rowSums(bp)
                colnames(bpt) <- c("TOTAL", colnames(bp))
                rownames(bpt) <- c("TOTAL", rownames(bp))
                bp <- bpt
            }

            return(bp)
        }

        # convert reactive matrix inputs ####
        observeEvent(input$seatsMatrix, {
            if(input$set_seats_per_district) {
                vals$seats_districts <- setNames(c(input$seatsMatrix),
                                                 colnames(input$seatsMatrix))
            } else {
                vals$seats_total <- sum(input$seatsMatrix)
            }
        }, ignoreInit = TRUE)

        observeEvent(input$votesMatrix, {
            vals$votes_matrix <- input$votesMatrix

            if(input$set_seats_per_district) {
                vals$seats_districts <- setNames(vals$seats_districts, colnames(vals$votes_matrix))
                update_seatsMatrix(vals$seats_districts)
            }
        }, ignoreInit = TRUE)

        # update inputs ####
        observeEvent(input$run_update_matrix, {
            vals$votes_matrix <- shiny_create_empty_votes_matrix(input$n_rows, input$n_cols)
            vals$seats_districts <- setNames(rep(0, input$n_cols), colnames(vals$votes_matrix))
            shiny::updateSelectInput(session, "load_example", selected = "...")
            update_input_matrices()
        }, ignoreInit = TRUE)

        update_input_matrices = function() {
            update_votesMatrix(vals$votes_matrix)
            if(input$set_seats_per_district) {
                update_seatsMatrix(vals$seats_districts)
            } else {
                update_seatsMatrix(vals$seats_total)
            }
        }

        update_votesMatrix = function(.votes_matrix) {
            shinyMatrix::updateMatrixInput(session, "votesMatrix", .votes_matrix)
        }

        update_seatsMatrix = function(.seats) {
            if(!is.matrix(.seats)) {
                if(length(.seats) == 1) {
                    .seats <- matrix(.seats, 1, 1,
                                     dimnames = list("seats", "total"))
                } else {
                    .seats = matrix(.seats, nrow = 1,
                                    dimnames = list("seats", names(.seats)))
                }
            }
            shinyMatrix::updateMatrixInput(session, "seatsMatrix", .seats)
        }

        # seats per district option ####
        observeEvent(input$set_seats_per_district, {
            if(input$set_seats_per_district) {
                update_seatsMatrix(vals$seats_districts)
            } else {
                update_seatsMatrix(vals$seats_total)
            }
        }, ignoreInit = TRUE)

        # Load examples ####
        observeEvent(input$load_example, {
            if(input$load_example == "...") {
                return()
            } else if(input$load_example == "Zug 2018") {
                vals$votes_matrix <- shinyapp_examples$zug_2018$votes
                vals$seats_districts <- shinyapp_examples$zug_2018$seats
                vals$seats_total <- sum(vals$seats_districts)
                set_inputs(quorum_districts = 0.05, quorum_total = 0.03, wto = TRUE)
                update_votesMatrix(vals$votes_matrix)
                update_seatsMatrix(vals$seats_districts)
            } else if(input$load_example == "Uri 2020") {
                vals$votes_matrix <- shinyapp_examples$uri_2020$votes
                vals$seats_districts <- shinyapp_examples$uri_2020$seats
                vals$seats_total <- sum(vals$seats_districts)
                set_inputs()
                update_votesMatrix(vals$votes_matrix)
                update_seatsMatrix(vals$seats_districts)
            } else if(input$load_example == "Wikipedia EN") {
                vals$votes_matrix <- shinyapp_examples$wikipedia_en$votes
                vals$seats_total <- shinyapp_examples$wikipedia_en$seats
                vals$seats_districts <- divisor_round(colSums(shinyapp_examples$wikipedia_en$votes),
                                                      shinyapp_examples$wikipedia_en$seats)
                set_inputs(use_list_votes = FALSE, set_seats_per_district = FALSE)
                update_votesMatrix(vals$votes_matrix)
                update_seatsMatrix(vals$seats_total)
            } else if(input$load_example == "Wikipedia DE") {
                vals$votes_matrix <- shinyapp_examples$wikipedia_de$votes
                vals$seats_districts <- shinyapp_examples$wikipedia_de$seats
                vals$seats_total <- sum(vals$seats_districts)
                set_inputs()
                update_votesMatrix(vals$votes_matrix)
                update_seatsMatrix(vals$seats_districts)
            }
        }, ignoreInit = TRUE)

        set_inputs = function(quorum_districts = 0, quorum_total = 0,
                              use_list_votes = TRUE, set_seats_per_district = TRUE,
                              wto = FALSE) {
            shiny::updateCheckboxInput(session, "use_list_votes", value = use_list_votes)
            shiny::updateNumericInput(session, "quorum_districts", value = quorum_districts)
            shiny::updateNumericInput(session, "quorum_total", value = quorum_total)
            shiny::updateCheckboxInput(session, "set_seats_per_district", value = set_seats_per_district)
            shiny::updateCheckboxInput(session, "wto", value = wto)
        }
    }

    # Run the application ####
    shiny::shinyApp(ui = ui, server = server)
}

# helper functions ####
shiny_create_empty_votes_matrix = function(nrows, ncols) {
    m = matrix(0, nrows, ncols)
    colnames(m) <- paste("District ", 1:ncols)
    rownames(m) <- paste("Party ", 1:nrows)
    return(m)
}

shiny_create_seats_matrix = function(votes_matrix,
                                     district_seats = setNames(rep(0, ncol(votes_matrix)), colnames(votes_matrix))) {
    if(length(district_seats) == 1) {
        district_seats_matrix = matrix(district_seats, 1, 1,
                                       dimnames = list("seats", "total"))
    } else {
        district_seats_matrix = matrix(district_seats, nrow = 1,
                                       dimnames = list("seats", names(district_seats)))
    }
    return(district_seats_matrix)
}

shiny_get_quorum_function = function(q_districts, q_total, q_all) {
    if(q_districts > 0 && q_total > 0) {
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
}

# nocov end
