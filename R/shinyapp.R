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
    if(is.null(votes_matrix)) {
        base_votes_matrix = shiny_create_empty_votes_matrix(3, 4)
    } else {
        base_votes_matrix = votes_matrix
    }
    if(is.null(district_seats)) {
        base_district_seats_mtrx = shiny_create_seats_matrix(base_votes_matrix)
        base_district_seats = setNames(c(base_district_seats_mtrx), colnames(base_district_seats_mtrx))
    } else {
        base_district_seats_mtrx = shiny_create_seats_matrix(base_votes_matrix, district_seats)
        base_district_seats = district_seats
    }

    apport_methods_choices = unique(unname(unlist(proporz_methods)))[c(2,1,3:6)]

    rm(votes_matrix)
    rm(district_seats)

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
                       inputId = "districtSeatsMatrix",
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

        vals = reactiveValues(votes_matrix = base_votes_matrix,
                              district_seats = base_district_seats,
                              loading = FALSE)

        # run biproportional apportionment ####
        output$biproporz_result <- shiny::renderTable(
            run_biproporz(),
            digits = 0,
            rownames = TRUE)

        run_biproporz = function() {
            if(sum(vals$votes_matrix) == 0) return(NULL)
            if(sum(vals$district_seats) == 0) return(NULL)
            if(any(nchar(colnames(input$votesMatrix)) == 0)) return(NULL)
            .quorum = shiny_get_quorum_function(input$quorum_districts, input$quorum_total, input$quorum_all)

            if(!input$wto) {
                method = "round"
            } else {
                method = "wto"
            }

            if(input$set_seats_per_district) {
                seats = vals$district_seats
            } else {
                seats = sum(vals$district_seats)
            }

            bp = biproporz(vals$votes_matrix, seats,
                           quorum = .quorum,
                           use_list_votes = input$use_list_votes,
                           method = method)

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
        observeEvent(input$districtSeatsMatrix, {
            if(!vals$loading) {
                vals$district_seats <- setNames(c(input$districtSeatsMatrix), colnames(input$districtSeatsMatrix))
            }
        })

        observeEvent(input$votesMatrix, {
            if(!vals$loading) {
                vals$votes_matrix <- input$votesMatrix

                if(input$set_seats_per_district) {
                    # sync district seat matrix names
                    updated_vec = setNames(vals$district_seats, colnames(vals$votes_matrix))
                    updated_mtx = shiny_create_seats_matrix(input$votesMatrix, updated_vec)

                    vals$district_seats <- updated_vec
                    shinyMatrix::updateMatrixInput(session, "districtSeatsMatrix", updated_mtx)
                }
            }
        })

        # update inputs ####
        observeEvent(input$run_update_matrix, {
            if(!vals$loading) {
                vm = shiny_create_empty_votes_matrix(input$n_rows, input$n_cols)
                update_input_matrices(vm)
            }
        })

        update_input_matrices = function() {
            shinyMatrix::updateMatrixInput(session, "votesMatrix", vals$votes_matrix)

            dsm = shiny_create_seats_matrix(vals$votes_matrix, vals$district_seats)
            shinyMatrix::updateMatrixInput(session, "district_seats_matrix", dsm)
        }

        # seats per district option ####
        observeEvent(input$set_seats_per_district, {
            if(input$set_seats_per_district) {

            }
            # if(sum(input$votes_matrix) == 0) return(NULL)
            # if(sum(input$district_seats_matrix) == 0) return(NULL)
            # if(any(nchar(colnames(input$votes_matrix)) == 0)) return(NULL)
            #
            # if(input$set_seats_per_district) {
            #     tmp_seats = upper_apportionment(input$votes_matrix,
            #                                     district_seats = sum(input$district_seats_matrix))
            #
            #     m = shiny_create_seats_matrix(input$votes_matrix, tmp_seats$district)
            # } else {
            #     m = shiny_create_seats_matrix(input$votes_matrix, sum(input$district_seats_matrix))
            # }
            # shinyMatrix::updateMatrixInput(session, "district_seats_matrix", m)
        })

        # Load examples ####
        observeEvent(input$load_example, {
            vals$loading <- TRUE
            if(input$load_example == "Zug 2018") {
                set_inputs(quorum_districts = 0.05, quorum_total = 0.03, wto = TRUE)
                vals$votes_matrix <- shinyapp_examples$zug_2018$votes
                vals$district_seats <- shinyapp_examples$zug_2018$seats
            } else if(input$load_example == "Uri 2020") {
                set_inputs()
                vals$votes_matrix <- shinyapp_examples$uri_2020$votes
                vals$district_seats <- shinyapp_examples$uri_2020$seats
            } else if(input$load_example == "Wikipedia EN") {
                set_inputs(use_list_votes = FALSE, set_seats_per_district = FALSE)
                vals$votes_matrix <- shinyapp_examples$wikipedia_en$votes
                vals$district_seats <- shinyapp_examples$wikipedia_en$seats
            } else if(input$load_example == "Wikipedia DE") {
                set_inputs()
                vals$votes_matrix <- shinyapp_examples$wikipedia_de$votes
                vals$district_seats <- shinyapp_examples$wikipedia_de$seats
            }
            update_input_matrices()
            vals$loading <- FALSE
        })

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
                                       dimnames = list("seats", colnames(votes_matrix)))
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

# run_app(t(testdata$GR_2022$votes_matrix), testdata$GR_2022$district_seats_df)
# run_app()
