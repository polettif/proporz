# Read a bazi data file
read_bazi_data = function(file.bazi) {
    lines = tryCatch({
        con = file(file.bazi, encoding = "windows-1252")
        readLines(con)
    }, warning = \(w) {
        if(startsWith(w$message, "incomplete final line found")) {
            con = file(file.bazi, encoding = "windows-1252")
            return(readLines(con, warn = FALSE))
        }
        readLines(file.bazi, warn = FALSE)
    })
	Encoding(lines) <- "UTF-8"
	stopifnot(all(!grepl("::TOKEN::", lines)))

	vals = .get_token_values(lines)
	vals$filename <- file.bazi

	if(length(unique(names(vals))) == length(vals)) {
		if(strcount(vals[["DATA"]], "\n\\+") <= 2) {
			vals <- .parse_bazi_single_district(vals)
		} else {
			# district data per party
			vals <- .parse_bazi_single_district_party_fill(vals)
		}
	} else {
		vals <- .parse_bazi_multiple_districts(vals)
	}

	class(vals) <- c("bazi_data", "list")

	return(vals)
}

# parse bazi files ####
.get_token_values = function(lines) {
	lines <- trimws(lines)

	# rename tokens
	gsub. = function(x, pattern, replacement, ignore.case = TRUE) {
	    gsub(pattern, replacement, x, ignore.case = ignore.case)
	}
	lines <- lines |>
		gsub.("=TITEL=", "=TITLE=") |>
		gsub.("=METHODE=", "=METHOD=") |>
		gsub.("=EINGABE=", "=INPUT=") |>
		gsub.("=AUSGABE=", "=OUTPUT=") |>
		gsub.("=DISTRIKT=", "=DISTRICT=") |>
		gsub.("=DISTRIKTOPTION=", "=DISTRICTOPTION=") |>
		gsub.("=MANDATE=", "=SEATS=") |>
        gsub.("=MANDATES=", "=SEATS=") |>
		gsub.("=DATEN=", "=DATA=") |>
		gsub.("=ENDE=", "=END=") |>
		gsub.("=KODIERUNG=", "=ENCODING=") |>
		gsub.("=GENAUIGKEIT=", "=ACCURACY=")

	# trim to end
	end_index = which(lines == "=END=")
	stopifnot(length(end_index) == 1)
	lines <- lines[1:end_index]

	# all tokens
	tokens = c("TITLE", "METHOD", "OUTPUT", "INPUT", "DISTRICTOPTION",
			   "DISTRICT", "SEATS",
			   "DATA", "INFO", "PATTS", "ENCODING", "ACCURACY")

	# sanitize info
	info_index = which(startsWith(lines, "=INFO="))
	if(length(info_index) > 0) {
		for(token in tokens) {
			info_range = seq(info_index+1, end_index-1)
			lines[info_range] <- gsub(
				paste0("=", token, "="),
				paste0("", token," \n"),
				lines[info_range], ignore.case = TRUE)
		}
	}

	# find relevant tokens
	txt = paste0(lines, collapse = "\n")
	for(token in tokens) {
		txt <- gsub(paste0("=", token, "="),
					paste0("::TOKEN::",token,"\n"),
					txt, ignore.case = TRUE)
	}
	stopifnot(!any(grepl("\n=[A-Z]+=", gsub("=END=", "", txt))))
	tokenized_lines = strsplit1(txt, "\n")

	# create list from tokens
	token_index = which(startsWith(tokenized_lines, "::TOKEN::"))
	token_index <- c(token_index, length(tokenized_lines))
	stopifnot(tokenized_lines[length(tokenized_lines)] == "=END=")

	token_data_list = list()
	token_names = list()
	for(i in seq(1, length(token_index)-1)) {
		from = token_index[i]+1
		to = token_index[i+1]-1
		data_lines = trimws(tokenized_lines[seq(from, to)])
		data_lines <- data_lines[data_lines != "" & data_lines != " "]

		name = gsub("::TOKEN::", "", tokenized_lines[token_index[i]])
		data = paste(data_lines, collapse = "\n")

		token_names[[length(token_names)+1]] <- name
		token_data_list[[length(token_data_list)+1]] <- data
	}

	names(token_data_list) <- token_names
	return(token_data_list)
}

.parse_bazi_single_district = function(vals) {
	vals$data <- .read.table_bazi(vals[["DATA"]])
	cns = trimws(strsplit1(vals[["INPUT"]], ","))
	colnames(vals$data) <- cns[seq(1,ncol(vals$data))]
	vals$seats <- suppressWarnings(as.integer(vals[["SEATS"]]))
	vals
}

.parse_bazi_single_district_party_fill = function(vals) {
	vals[["DATA"]] <- gsub("\n\\+", "\n\\+ ", vals[["DATA"]])
	vals$data <- .read.table_bazi(vals[["DATA"]])

	vals$data[[1]] <- fill_plus(vals$data[[1]])
	cns = trimws(strsplit1(vals[["INPUT"]], ","))
	if(length(cns) == ncol(vals$data) && cns[length(cns)] == "---") {
		cns <- cns[-length(cns)]
	}

	colnames(vals$data) <- c(cns[1], "DISTRICT",
							 cns[2:length(cns)])

	vals$seats <- suppressWarnings(as.integer(vals[["SEATS"]]))

	vals
}

.parse_bazi_multiple_districts = function(vals) {
	# more than one disctrict data
	distrikt_mandate_daten = vals[names(vals) %in% c("DISTRICT", "SEATS", "DATA")]
	stopifnot(length(unique(table(names(distrikt_mandate_daten)))) == 1)

	# split into sets of three
	district_data_list = split(distrikt_mandate_daten,
							   rep(seq_len(length(distrikt_mandate_daten)/3), each = 3))

	# data votes
	data_list = district_data_list |>
		lapply(\(dmd) {
			x = strsplit1(dmd[["DATA"]], "\n")
			x <- gsub("^\\+", "", x)
			x <- lapply(x, \(x) {
				if(!grepl('"', dmd[["DISTRICT"]])) {
					return(paste0('"', dmd[["DISTRICT"]], '" ', x))
				} else if(!grepl("'", dmd[["DISTRICT"]])) {
					return(paste0("'", dmd[["DISTRICT"]], "' ", x))
				}
				stop() # nocov
			})
			unlist(x)
		}) |> unlist(use.names = F)

	vals$data <- data_list |>
		lapply(\(txt) {
			read.table(text = txt, fill = TRUE,
					   col.names = c("DISTRIKT", trimws(strsplit1(vals[["INPUT"]], ","))))
		})
	vals$data <- do.call(rbind, vals$data)

	# district seats
	vals$seats <- district_data_list |>
		lapply(\(dmd) {
			data.frame(district = dmd[["DISTRICT"]], seats = as.integer(dmd[["SEATS"]]))
		})
	vals$seats <- do.call(rbind, vals$seats)

	vals
}

.read.table_bazi = function(text) {
	read.table(text = text, fill = TRUE, header = FALSE, row.names = NULL)
}

# utils ####
strsplit1 = function(x, split, fixed = FALSE) {
	strsplit(x, split, fixed = fixed)[[1]]
}

strcount = function(x, pattern) {
	length(gregexpr(pattern, x)[[1]])
}

fill_plus = function(x) {
	values = x[x != "+"]
	y = rep(1, length(x))
	y[x == "+"] <- 0
	lvls = cumsum(y)

	values[lvls]
}
