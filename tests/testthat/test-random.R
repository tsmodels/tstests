test_that("mincer", {
    set.seed(100)
    actual <- rnorm(1000)
    predicted <- rev(actual)
    test <- minzar_test(actual, predicted)
    expect_lt(test$table[3]$`Pr(>|t|)`, 0.01)

    set.seed(100)
    actual <- rnorm(1000)
    predicted <- actual + rnorm(1000, 0, 0.01)
    test <- minzar_test(actual, predicted)
    expect_equal(test$table[2]$Estimate, 1, tolerance = 0.01)
})

test_that("dac", {
    set.seed(100)
    actual <- rnorm(1000)
    predicted <- actual + rnorm(1000, 0, 0.01)
    test <- dac_test(actual, predicted)
    expect_lt(test$table[1]$`Pr(>|t|)`, 0.01)
    expect_lt(test$table[2]$`Pr(>|t|)`, 0.01)
})

test_that("signbias", {
    set.seed(100)
    x <- rnorm(1000)
    test <- signbias_test(x, sigma = 1)
    expect_gt(test$table[4]$`Pr(>|t|)`, 0.5)
})



