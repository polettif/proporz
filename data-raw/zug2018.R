zug2018 = read.csv("https://wab.zug.ch/elections/kantonsratswahl-2018/data-csv",
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")
usethis::use_data(zug2018, overwrite = TRUE)
