library(dplyr)

# Source: https://tulospalvelu.vaalit.fi/EKV-2019/en/ladattavat_tiedostot.html

# Download data ####
zipfile = tempfile(fileext = ".zip")
download.file("https://tulospalvelu.vaalit.fi/EKV-2019/ekv-2019_puo_maa.csv.zip",
              zipfile)

zip::unzip(zipfile, exdir = "finland_2019")

# Load data ####
suomi19.csv = data.table::fread("finland_2019/ekv-2019_tpat_maa.csv",
                                header = F,
                                encoding = "Latin-1")

suomi19 <- suomi19.csv |>
    as_tibble() |>
    rename(district_nr = V2,
           municipality_nr = V3,
           area_type = V4,
           district_name = V6, # V7
           list_nr = V10,
           party_nr = V8,
           party_name = V11, # V12, V13
           votes = V41, seats = V45) |>
    select(-V7, -V12, -V13)

# only keep V = electoral district
suomi19 <- suomi19 |>
    filter(area_type == "V")

# remove Aland (special election system)
suomi19 <- suomi19 |> filter(district_name != "AHV")

# build votes_df ####
votes_df = suomi19 |>
    group_by(party_name, district_name) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    arrange(desc(votes))

# build seats_df ####
seats_df = suomi19 |> group_by(district_name) |>
    summarise(seats = sum(seats), .groups = "drop")

# build data
finland2019 = list(
    votes_df = as.data.frame(votes_df),
    district_seats_df = as.data.frame(seats_df)
)

usethis::use_data(finland2019,
                  overwrite = TRUE)
