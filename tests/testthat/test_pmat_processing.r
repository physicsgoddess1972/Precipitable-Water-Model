library(testthat)

logg("INFO", "Starting PMAT Preprocessing Unit Tests", lev="DEBUG")

testthat::test_that("Testing colscheme", {
    testthat::expect_error(
        colscheme("b"),
        "non-numeric argument to binary operator"
    )

    testthat::expect_equal(
        colscheme(4),
        c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
    )

    testthat::expect_equal(
        colscheme(4, "Spectral"),
        c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA")

    )
})

testthat::test_that("Testing nan.filter", {    
    testthat::expect_equal(
        nan.filter(list(list(NA, 1, 2), list(3, 4, 5), list(6, 7, 8))),
        list(list(list(1, 2), list(4, 5), list(7, 8)), c(1))
    )

    testthat::expect_equal(
        nan.filter(list(list(NA, 1, 2), list(3, NA, 5), list(6, 7, NA))),
        list(list(list(), list(), list()), c(1, 2, 3))
    )

    testthat::expect_error(
        nan.filter(list()),
        "subscript out of bounds"
    )
})

# testthat::test_that("errors", {
#     testthat::expect_equal(
#         testthat::expect_equal(
#             mean.filter(list(list(list(1, 2), list(4, 5), list(7, 8)), c(1)), 1),
#             "Test"
#         )
#     )
# })

testthat::test_that("Testing dna.filter", {
    testthat::expect_equal(
        dna.filter(list(snsr_sky1=c(1, 2, 3),
                        snsr_gro1=c(3, 4, 5),
                        pw_loc1=c(10, 20, 30),
                        com=c("", "", "DNA"))),
        list(snsr_sky1=c(1, 2, NaN), 
                snsr_gro1=c(3, 4, NaN), 
                pw_loc1=c(10, 20, NaN), 
                com=c("", "", "DNA"))
    )

    testthat::expect_equal(
        dna.filter(list(snsr_sky1=c(), snsr_gro1=c(), pw_loc1=c(), com=c())),
        list(snsr_sky1=c(), snsr_gro1=c(), pw_loc1=c(), com=c())
    )

    testthat::expect_error(
        dna.filter(list(snsr_s=c(1, 2, 3),
                        snsr_gro=c(3, 4, 5),
                        pw_loc=c(10, 20, 30),
                        com=c("", "", "DNA"))),
        err$D[[1]]$code
    )

    testthat::expect_error(
        dna.filter(list(snsr_sky=c(1, 2, 3),
                        snsr_g=c(3, 4, 5),
                        pw_loc=c(10, 20, 30),
                        com=c("", "", "DNA"))),
        err$D[[1]]$code
    )
    
    testthat::expect_error(
        dna.filter(list(snsr_sky=c(1, 2, 3),
                        snsr_gro=c(3, 4, 5),
                        pw_l=c(10, 20, 30),
                        com=c("", "", "DNA"))),
        err$D[[3]]$code
    )
})

testthat::test_that("Testing inf.counter", {
    testthat::expect_equal(
        inf.counter(FALSE, list(snsr_sky1=list('-Inf', 2, 3)), "sky"),
        list(list(NaN, 2, 3))
    )

    testthat::expect_equal(
        inf.counter(TRUE, list(snsr_sky1=list('-Inf', 2, 3)), "sky"),
        list(list('-Inf', 2, 3))
    )

    testthat::expect_error(
        inf.counter(FALSE, list(snsr_sky1=list("-Inf", 2, 3)), "gro"),
        err$error$D[[4]]$code
    )

    testthat::expect_error(
        inf.counter(TRUE, list(s=list('-Inf', 2, 3)), "sky"),
        err$error$D[[4]]$code
    )
})


testthat::test_that("Testing index.norm", {
    testthat::expect_equal(
        index.norm(c(1, 2, 3)),
        c(0, 0.5, 1)
    )

    testthat::expect_equal(
        index.norm(c(1)),
        c(NaN)
    )
    testthat::expect_error(
        index.norm(c()),
        err$error$D[[1]]$code
    )
})