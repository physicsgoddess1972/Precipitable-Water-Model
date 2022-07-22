library(testthat)


testthat::test_that("Testing exp.regression", {
    testthat::expect_error(
        exp.regression(mean.out=list(list(1), 
                                    list(x=list(1), y=list(2)))),
        err$D[[1]]$code
    )
})