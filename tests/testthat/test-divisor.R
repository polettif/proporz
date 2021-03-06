context("divisor")

# http://www.wahlrecht.de/verfahren/dhondt123.html
test_that("divisor_floor", {
    v1 = c(4160, 3380, 2460)
    n1 = 10
    e1 = c(4,4,2)
	expect_equal(divisor_floor(v1, n1), e1)
	expect_equal(proporz(v1, n1, "d'hondt"), e1)
	expect_equal(proporz(v1, n1, "jefferson"), e1)
})

# http://www.wahlrecht.de/verfahren/stlague12.html
test_that("divisor_floor", {
    v2 = c(4160, 3380, 2460)
    n2 = 10
    e2 = c(4,3,3)
    expect_equal(divisor_round(v2, n2), e2)
    expect_equal(proporz(v2, n2, "webster"), e2)
    expect_equal(proporz(v2, n2, "sainte-lague"), e2)
})

# https://de.wikipedia.org/wiki/Adams-Verfahren
test_that("divisor_ceiling", {
    v3 = c(450, 350, 199, 1)
    n3 = 50
    e3 = c(22,17,10,1)
    expect_equal(divisor_ceiling(v3, n3), e3)
    expect_equal(proporz(v3, n3, "adams"), e3)
})

# https://de.wikipedia.org/wiki/Adams-Verfahren
test_that("divisor_harmonic", {
    v4 = c(450, 350, 199, 1)
    n4 = 50
    e4 = c(22,17,10,1)
    expect_equal(divisor_harmonic(v4, n4), e4)
    expect_equal(proporz(v4, n4, "dean"), e4)
})

# https://de.wikipedia.org/wiki/Hill-Huntington-Verfahren#H%C3%B6chstzahlverfahren
test_that("divisor_geometric", {
    v5 = c(450,350,199,1)
    n5 = 50
    e5 = c(22,17,10,1)
    expect_equal(divisor_geometric(v5, n5), e5)
})

# https://en.wikipedia.org/wiki/Huntington%E2%80%93Hill_method#Example
test_that("huntington_hill", {
    v6 = c(100,80,30,20)
    n6 = 8
    e6 = c(4,3,1,0)
    quor = sum(v6)/n6
    expect_equal(proporz(v6, n6, "huntington-hill", quor), e6)
})
