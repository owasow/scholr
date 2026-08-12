test_that("star_or displays odds ratios by default", {
    model <- glm(am ~ wt, data = mtcars, family = binomial())

    output <- utils::capture.output(star_or(model, type = "text"))

    expected_or <- sprintf("%.3f", exp(stats::coef(model)[["wt"]]))
    expect_true(any(grepl(expected_or, output, fixed = TRUE)))
})

test_that("stargazer2 warns while preserving its original default", {
    model <- glm(am ~ wt, data = mtcars, family = binomial())

    expect_warning(
        old_output <- utils::capture.output(stargazer2(model, type = "text")),
        "deprecated"
    )
    raw_output <- utils::capture.output(
        suppressWarnings(star_or(model, odds.ratio = FALSE, type = "text"))
    )

    expect_equal(old_output, raw_output)
})
