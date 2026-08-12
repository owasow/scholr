test_that("inline model helpers extract and combine estimates", {
    model <- stats::lm(mpg ~ wt, data = mtcars)

    expect_equal(unname(b(model, "wt")), round(stats::coef(model)[["wt"]], 2))
    expect_equal(se(model, "wt"), round(summary(model)$coefficients["wt", "Std. Error"], 2))
    expect_match(p(model, "wt"), "^< \\.001$|^= \\.[0-9]{3}$")
    expect_match(bp(model, "wt"), "^b = .+, p ")
    expect_match(ci95(model, "wt"), "^\\[.+, .+\\]$")
})

test_that("p-value formatters honor threshold boundaries", {
    values <- c(0.0009, 0.001, 0.009, 0.01, 0.049, 0.05)

    expect_equal(
        pval(values),
        c(
            "$p < 0.001$", "$p < 0.01$", "$p < 0.01$",
            "$p < 0.05$", "$p < 0.05$", "$p > 0.05$"
        )
    )
})
