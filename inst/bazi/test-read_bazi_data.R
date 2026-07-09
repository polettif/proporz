# BAZI Dataset (data.zip) available from:
# https://www.tha.de/Geistes-und-Naturwissenschaften/Data-Science/BAZI.html
# Direct URL: https://www.tha.de/Binaries/Binary78393/data.zip
# Extract to "data/" folder

bazi_files = list.files("./inst/bazi/data",
                        full.names = TRUE,
                        recursive = TRUE,
                        pattern = "\\.bazi$")

for(.bazi_file in bazi_files) {
    testthat::expect_no_condition(read_bazi_data(.bazi_file))
}
