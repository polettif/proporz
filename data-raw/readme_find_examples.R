xgrid = expand.grid(
    A = seq(600, 700, 10),
    B = seq(300, 400, 10),
    C = seq(200, 300, 10),
    D = seq( 10, 200, 10))

unique_rows = function(x) {
    apply(x, 1, \(row) length(unique(row)) == ncol(x))
}
xgrid <- xgrid[unique_rows(xgrid),]

xgrid_list = asplit(xgrid, MARGIN = 1) # split into list of row vectors

proporz_catch = function(votes, n_seats, method) {
    tryCatch(
        proporz(votes, n_seats, method),
        error = \(e) rep(0, length(votes)))
}

hashes = list()
for(method in c("floor", "round", "ceiling", "harmonic", "geometric")) {
    print(method)
    .seats_list = lapply(xgrid_list, proporz_catch, 10, method)
    .hash_list = lapply(.seats_list, paste, collapse = "")
    hashes[[method]] <- unlist(.hash_list)
}

xgrid[unique_rows(as.data.frame(hashes)),]
