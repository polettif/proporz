reprex::reprex({
    library(proporz)
    votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)

    proporz(votes, n_seats = 10, method = "sainte-lague")

    proporz(votes, 10, "hill-huntington", quorum = 0.05)
})

reprex::reprex({
    library(proporz)

    (votes_matrix <- uri2020$votes_matrix)

    (district_seats <- uri2020$seats_vector)

    biproporz(votes_matrix, district_seats)
})

reprex::reprex({
    library(proporz)

    # In this data set, parties are called 'lists' and districts 'entities'.
    votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
    district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])

    seats_df = pukelsheim(votes_df,
                          district_seats_df,
                          quorum = quorum_any(any_district = 0.05, total = 0.03),
                          winner_take_one = TRUE)

    head(seats_df)

    # compare to actual result
    actual_result = unique(zug2018[c("list_id", "entity_id", "list_number_of_mandates")])
    all.equal(seats_df[order(seats_df$entity_id, seats_df$list_id),"seats"],
              actual_result[order(actual_result$entity_id, actual_result$list_id),"list_number_of_mandates"])
})

reprex::reprex({
    library(proporz)
    votes = c("Party A" = 690, "Party B" = 370, "Party C" = 210, "Party D" = 10)

    # D'Hondt, Jefferson or Hagenbach-Bischoff method
    divisor_floor(votes, 10)

    # Sainte-Laguë or Webster method
    divisor_round(votes, 10)

    # Adams method
    divisor_ceiling(votes, 10)

    # Dean method
    divisor_harmonic(votes, 10)

    # Huntington-Hill method
    divisor_geometric(votes, 10)
})

reprex::reprex({
    library(proporz)
    votes = c("I" = 16200, "II" = 47000, "III" = 12700)

    # Hamilton, Hare-Niemeyer or Vinton method
    largest_remainder_method(votes, 20)
})

# unused
reprex::reprex({
    library(proporz)
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)

    quorum_functions = quorum_any(any_district = 0.1, total = 100)
    reached_quorums(votes_matrix, quorum_functions)
})
