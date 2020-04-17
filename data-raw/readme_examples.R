reprex::reprex({
    library(proporz)
    votes = c("Party A" = 690, "Party B" = 400, "Party C" = 250, "Party D" = 120)

    divisor_round(votes, 10)
    divisor_floor(votes, 10)
    quota_largest_remainder(votes, 10)
})

reprex::reprex({
    library(proporz)
    votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)

    proporz(votes, 10, "sainte-lague")
    proporz(votes, 10, "hill-huntington", quorum = 0.01)
    proporz(votes, 10, "hill-huntington", quorum = 0.05)
})

reprex::reprex({
    library(proporz)

    votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
    district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])

    seats_df = pukelsheim(votes_df,
                          district_seats_df,
                          quorum_districts = 0.05,
                          quorum_total = 0.03)

    head(seats_df)

    divisors(seats_df)
})

reprex::reprex({
    library(proporz)

    votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
    votes_matrix = pivot_to_matrix(votes_df)
    votes_matrix

    distr_df = unique(zug2018[c("entity_id", "election_mandates")])
    district_seats = setNames(distr_df$election_mandates, distr_df$entity_id)
    district_seats

    seats_matrix = biproportional(votes_matrix, district_seats, 0.05, 0.03)
    seats_matrix

    divisors(seats_matrix)
})

reprex::reprex({
    library(proporz)
    votes_matrix = matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3)
    district_seats = 4:6

    seats_matrix = biproportional(votes_matrix, district_seats)
    divisors(seats_matrix)
})
