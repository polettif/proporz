shinyapp_examples = list(
	zug_2018 = list(
		votes = pivot_to_matrix(unique(zug2018[c("list_name", "entity_name", "list_votes")])),
		seats = unique(zug2018[c("entity_id", "election_mandates")])$election_mandates
	),
	wikipedia_en = list(
		votes = matrix(c(123, 912, 312, 45, 714, 255, 815, 414, 215), nrow = 3,
					   dimnames = list(c("Party A", "Party B", "Party C"), c("Region I", "Region II", "Region III"))),
		seats = 20
	),
	wikipedia_de = list(
		votes = matrix(c(5100, 6000, 6300, 9800, 10000, 10200, 4500, 12000, 14400), nrow = 3,
					   dimnames = list(c("Party 1", "Party 2", "Party 3"), c("Wahlkreis A", "Wahlkreis B", "Wahlkreis C"))),
		seats = c(4, 5, 6)
	),
	uri_2020 = list(
	    votes = uri2020$votes_matrix,
	    seats = uri2020$seats_vector
	)
)

usethis::use_data(shinyapp_examples, overwrite = TRUE, internal = TRUE)
