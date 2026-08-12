capture_star <- function(expr) {
    utils::capture.output(expr)
}

test_that("new table-style wrappers expose their intended defaults", {
    model <- lm(mpg ~ wt + hp, data = mtcars)

    compact <- capture_star(star_compact(model, type = "text"))
    small <- capture_star(star_small(model, type = "text"))
    normal <- capture_star(star_normal(model, type = "text"))

    expect_true(any(grepl("-3.878", compact, fixed = TRUE)))
    expect_true(any(grepl("-3.88", small, fixed = TRUE)))
    expect_equal(small, normal)
})

test_that("star_compact supports omission, custom notes, and omitted statistics", {
    model <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)

    output <- capture_star(star_compact(
        model,
        type = "text",
        omit = "^factor\\(cyl\\)",
        omit.stat = "ser",
        notes = "Fixed effects included"
    ))

    expect_false(any(grepl("factor(cyl)", output, fixed = TRUE)))
    expect_false(any(grepl("Residual Std. Error", output, fixed = TRUE)))
    expect_true(any(grepl("Fixed effects included", output, fixed = TRUE)))
})

test_that("legacy wrapper names warn and preserve output", {
    model <- lm(mpg ~ wt + hp, data = mtcars)

    expect_warning(old0 <- capture_star(star0(model, type = "text")), "deprecated")
    expect_equal(old0, capture_star(star_compact(model, type = "text")))

    expect_warning(old_ft <- capture_star(star_ft(model, type = "text")), "deprecated")
    expect_equal(old_ft, capture_star(star_compact(model, type = "text")))

    expect_warning(old_sm <- capture_star(star_sm(model, type = "text")), "deprecated")
    expect_equal(old_sm, capture_star(star_small(model, type = "text")))

    expect_warning(old_nrm <- capture_star(star_nrm(model, type = "text")), "deprecated")
    expect_equal(old_nrm, capture_star(star_normal(model, type = "text")))

    expect_warning(old1 <- capture_star(star1(model, type = "text")), "deprecated")
    expect_equal(
        old1,
        capture_star(star_normal(model, type = "text", omit.stat = "theta"))
    )
})
