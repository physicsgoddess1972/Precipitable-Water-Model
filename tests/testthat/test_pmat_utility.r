library(testthat)

logg("INFO", "Starting PMAT Utility Unit Tests", lev="DEBUG")

## 
# Test for reset_time function
##
testthat::test_that("errors", {
    testthat::expect_error(
        reset_time("1"),
        err$D[[2]]$code
    )

    testthat::expect_error(
        reset_time("abc"),
        err$D[[2]]$code
    )

    testthat::expect_equal(
        reset_time("2022-01-01"),
        as.POSIXct("2022-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
    )

    testthat::expect_error(
        reset_time("2022-15-01"),
        err$D[[2]]$code
    )

    testthat::expect_equal(
        reset_time("2022-01-01 10:11:12"),
        as.POSIXct("2022-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
    )
})

## 
# Test for time_axis_init function
##
testthat::test_that("errors", {
    testthat::expect_equal(
        time_axis_init(seq(as.POSIXct("2022-01-01 00:00:00"), 
                            as.POSIXct("2022-03-01 00:00:00"), by="months")),
        list(c("2022-01-01", "2022-03-01"), c("2022-01-01", "2022-02-01", "2022-03-01"))
    )

    testthat::expect_error(
        time_axis_init("abc"),
        err$D[[2]]$code
    )

})

## 
# Test for stnd_title function
##
testthat::test_that("errors", {
    testthat::expect_equal(
        stnd_title("Test", TRUE),
        "Test\nCondition: Overcast"
    )

    testthat::expect_equal(
        stnd_title("Test", FALSE),
        "Test\nCondition: Clear Sky"
    )

    testthat::expect_equal(
        stnd_title("Test", NA),
        "Test\nCondition: NA"
    )
})