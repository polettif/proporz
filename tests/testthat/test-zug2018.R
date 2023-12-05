context("zug18")

# https://www.zg.ch/behoerden/staatskanzlei/kanzlei/abstimmungen-und-wahlen/wahlen-kr
# Gesamtergebnis der Sitzverteilung:
# https://www.zg.ch/behoerden/staatskanzlei/kanzlei/abstimmungen-und-wahlen/wahlen-kr/archiv-2018/downloads/2018-kr-dpt-gesamtergebnis-sitzverteilung.pdf

seats_mtrx_exp = matrix(
	c(0,3,3,5,2,3,3,
	  0,0,2,1,0,0,1,
	  0,1,1,1,0,1,2,
	  0,0,1,1,0,0,1,
	  0,2,3,2,1,3,4,
	  0,1,3,2,1,1,2,
	  0,1,2,1,0,1,1,
	  0,2,2,1,0,0,1,
	  0,1,2,2,0,0,2,
	  0,0,1,1,0,0,0,
	  0,0,1,0,0,0,1),
	nrow = 7
)

test_that("data input", {
	expect_equal(sum(seats_mtrx_exp), 80)
	expect_equal(unname(colSums(seats_mtrx_exp)), c(19,4,6,3,15,10,6,6,7,2,2))
	expect_equal(unname(rowSums(seats_mtrx_exp)), c(0,11,21,17,4,9,18))
})

colnames(seats_mtrx_exp) <- c("Zug", "Oberägeri", "Unterägeri", "Menzingen",
							  "Baar", "Cham", "Hünenberg", "Steinhausen",
							  "Risch", "Walchwil", "Neuheim")
rownames(seats_mtrx_exp) <- 1:7
names(dimnames(seats_mtrx_exp)) <- c("list_id", "entity_name")

test_that("pukelsheim with zug2018 is as expected", {
	votes_df = unique(zug2018[c("list_id", "entity_name", "list_votes")])
	district_seats_df = unique(zug2018[c("entity_name", "election_mandates")])

	seats_df = pukelsheim(votes_df,
						  district_seats_df,
						  quorum = quorum_any(any_district = 0.05,
						  total = 0.03))
	seats_mtrx = pivot_to_matrix(seats_df[c(1,2,4)])

	expect_equal(seats_mtrx[,colnames(seats_mtrx_exp)], seats_mtrx_exp)
})

test_that("upper apportionment for districts", {
	votes_df = unique(zug2018[c("list_id", "entity_name", "list_votes")])
	votes_matrix = pivot_to_matrix(votes_df)

	x = biproporz(votes_matrix, 80)
	expect_equal(sum(x), 80)
})
