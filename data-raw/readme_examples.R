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
                          quorum = quorum(any_district = 0.05, total = 0.03))

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

# quorum_all
reprex::reprex({
    library(proporz)
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
    dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
    seats = c(Z1 = 50, Z2 = 20)

    # use as parameter in biproportional (general use case)
    biproporz(votes_matrix, seats)

    biproporz(votes_matrix, seats, quorum = quorum_any(any_district = 0.1))

    biproporz(votes_matrix, seats, quorum = quorum_any(total = 100))

    biproporz(votes_matrix, seats, quorum = quorum_any(any_district = 0.1, total = 100))

    biproporz(votes_matrix, seats, quorum = quorum_all(any_district = 0.1, total = 100))
})

reprex::reprex({
    library(proporz)
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)

    quorum_functions = quorum_any(any_district = 0.1, total = 100)
    reached_quorums(votes_matrix, quorum_functions)
})
