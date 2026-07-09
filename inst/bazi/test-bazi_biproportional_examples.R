# BAZI Dataset (data.zip) available from:
# https://www.tha.de/Geistes-und-Naturwissenschaften/Data-Science/BAZI.html
# Direct URL: https://www.tha.de/Binaries/Binary78393/data.zip
# Extract to "data/" folder

# Load all biproportional example datasets from BAZI and run them
# with biproporz(). The apportionment results are not validated, this
# script is intended to find unexpected errors.

# Setup functions ####
get_proporz_method = function(bazi_data) {
    if(!is.null(bazi_data$DISTRICTOPTION) && tolower(bazi_data$DISTRICTOPTION) == "nzz") {
        return("nzz")
    }

    data_method = tolower(gsub(" ", "", bazi_data$METHOD))
    if(data_method == "divstd") {
        method = list("round", "round")
    } else if(data_method == "divauf") {
        method = "divisor_ceiling"
    } else if(data_method == "divabr") {
        method = "divisor_floor"
    } else if(data_method == "divgeo") {
        method = "divisor_geometric"
    } else if(data_method == "divstd,divabr") {
        method = list("divisor_round", "divisor_floor")
    } else if(data_method == "divabr,divstd") {
        method = list("divisor_floor", "divisor_round")
    } else {
        stop(bazi_data$METHOD)
    }
    return(method)
}

# Neue Zürcher Zuteilungsmethode
nzz = function(vm, ds) {
    weighted_votes_matrix = weight_votes_matrix(vm, ds)
    # weighted votes are rounded with the nzz method
    rounded_matrix = ceil_at(weighted_votes_matrix, 0.5)
    seats_party = proporz(rowSums(rounded_matrix), sum(ds), "round")
    names(seats_party) <- rownames(vm)

    seats_matrix = lower_apportionment(vm, ds, seats_party)
    t(seats_matrix)
}

# function to run pukelsheim with bazi_data
pukelsheim_bazi = function(bazi_data) {
    method = get_proporz_method(bazi_data)

    # datasets using voter counts are not described in the data files
    no_vote_weighting = c(
        "data/zTest_data/NZZ_problems/Tied_cases/AS1.bazi",
        "data/zTest_data/NZZ_problems/Tied_cases/AH2.bazi",
        "data/zTest_data/Biproportional_problems/Tied_cases/FP6.bazi",
        "data/zTest_data/Biproportional_problems/Nonexistence/FG3.bazi",
        "data/zTest_data/Biproportional_problems/Nonexistence/NE4.bazi",
        "data/zTest_data/Biproportional_problems/Nonexistence/Gassner2000-62Exemple24.bazi",
        "data/zTest_data/NZZ_problems/AH1-AH14/AH14.bazi"
    )
    weight_votes = !bazi_data$filename %in% no_vote_weighting

    # run biproporz
    vm = pivot_to_matrix(bazi_data$data[, c(2,1,3)])
    ds = setNames(bazi_data$seats[["SEATS"]], bazi_data$seats[["DISTRICT"]])

    if("nzz" %in% unlist(method)) {
        if(!weight_votes) {
            method <- "round"
        } else {
            return(nzz(vm, ds))
        }
    }
    seats = biproporz(vm, ds, method = method,
                      weight_votes = weight_votes)
    t(seats)
}

load_bazi_dir = function(path) {
    stopifnot(dir.exists(path))
    stopifnot(!endsWith(path, "/"))
    bazi_data_list = list.files(path, full.names = T, recursive = T, pattern = "bazi")
    bazi_data_list <- lapply(bazi_data_list, read_bazi_data)
    names(bazi_data_list) <- lapply(bazi_data_list, getElement, "filename")
    bazi_data_list
}

# Load working example data ####
bazi_examples = c(
    load_bazi_dir("data/zTest_data/Biproportional_problems/Diverse"),
    load_bazi_dir("data/zTest_data/NZZ_problems/Diverse"),
    load_bazi_dir("data/zTest_data/NZZ_problems/AH1-AH14"),
    # tied votes are actually broken in alternate scaling
    "data/zTest_data/NZZ_problems/Tied_cases/AS1.bazi" = list(read_bazi_data("data/zTest_data/NZZ_problems/Tied_cases/AS1.bazi"))
    )

# Fix/remove datasets with issues ####
# typo: XI as district name instead of IX
bazi_examples[["data/zTest_data/Biproportional_problems/Diverse/MLB-michigan.bazi"]]$data$DISTRICT[17:18] <- "=District IX="
bazi_examples[["data/zTest_data/Biproportional_problems/Diverse/MLB-michigan.bazi"]]$seats$DISTRICT[9] <- "=District IX="

# parsing: additional undefined column between party and votes
bazi_examples[["data/zTest_data/NZZ_problems/Diverse/FP1.bazi"]]$data$Parteist. <- NULL

# typo: EINGABE only with 2 instead of 3 values (missing ",")
bazi_examples[["data/zTest_data/Biproportional_problems/Diverse/Swiss2003Sim5006M.bazi"]] <- NULL

# "Numerischer Problemfall zum Testen des NZZ-Algorithmus" (2 gleichberechtigte Unterzuteilungen)
bazi_examples[["data/zTest_data/NZZ_problems/AH1-AH14/AH2.bazi"]] <- NULL

# Run workable datasets, expecting no errors ####
for(bazi_data in bazi_examples) {
    testthat::expect_no_error(pukelsheim_bazi(bazi_data))
}

# Load edge case datasets ####
bazi_errors = c(
    load_bazi_dir("data/zTest_data/Biproportional_problems/Nonexistence"),
    load_bazi_dir("data/zTest_data/NZZ_problems/Tied_cases")
)

# Remove datasets that exceed iterations ####
# method: use a winner-take-TWO method (not implemented in proporz) and has no result even with intended method
bazi_errors[["data/zTest_data/Biproportional_problems/Nonexistence/SM3.bazi"]] <- NULL
bazi_errors[["data/zTest_data/Biproportional_problems/Nonexistence/SM4.bazi"]] <- NULL

# The following examples lead to a "iterations exceeded" error that is not caught beforehand
bazi_errors[["data/zTest_data/Biproportional_problems/Nonexistence/FG3.bazi"]] <- NULL # flow criterion check in bazi uses district sets
bazi_errors[["data/zTest_data/NZZ_problems/Tied_cases/AS2.bazi"]] <- NULL # "2 gleichberechtige Unterzuteilungen"
bazi_errors[["data/zTest_data/NZZ_problems/Tied_cases/mlb1.bazi"]] <- NULL # "2 gleichberechtige Unterzuteilungen"
bazi_errors[["data/zTest_data/NZZ_problems/Tied_cases/AH2.bazi"]] <- NULL # "2 gleichberechtige Unterzuteilungen"

bazi_errors <- bazi_errors[setdiff(names(bazi_errors), names(bazi_examples))]

# Expect errors in edge case datasets ####
for(bazi_data in bazi_errors) {
	testthat::expect_error(
	    pukelsheim_bazi(bazi_data),
	    "(Result is undefined, tied votes)|(Result is undefined, equal quotient)|(Result is undefined, cannot assign)|(Not enough seats)")
}
