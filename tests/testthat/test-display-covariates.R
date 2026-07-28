# Oracle: what stargazer actually prints, parsed from text output.
# Only trustworthy for short covariate names and clean model calls --
# exactly the regime where the legacy parser worked.
sg_printed_rows <- function(..., omit = NULL) {
  out <- utils::capture.output(
    stargazer::stargazer(..., type = "text", omit = omit)
  )
  # covariate rows live between the two full-width dashed rules
  rules <- grep("^-{5,}$", out)
  stopifnot(length(rules) >= 2)
  out <- out[(rules[1] + 1):(rules[length(rules)] - 1)]
  cut <- which(startsWith(out, "Constant"))
  if (length(cut) > 0) out <- out[1:(cut[1] - 1)]
  lines <- grep("^[[:alpha:]]", out, value = TRUE)
  unname(sapply(lines, function(l) strsplit(l, "  +")[[1]][1]))
}

dc <- function(models, ...) {
  scholr:::.display_covariates(lapply(models, scholr:::.model_coefs), ...)
}

set.seed(42)
n <- 60
d <- data.frame(
  y  = rnorm(n), yb = rbinom(n, 1, .5),
  a = rnorm(n), b = rnorm(n), c = rnorm(n), x = rnorm(n), z = rnorm(n),
  f = factor(sample(c("lo", "mid", "hi"), n, TRUE), levels = c("lo", "mid", "hi"))
)

test_that("coef engine matches stargazer output: single model", {
  m <- lm(y ~ a + b + c, data = d)
  expect_equal(dc(list(m)), sg_printed_rows(m))
})

test_that("coef engine matches stargazer output: multi-model interleave", {
  m1 <- lm(y ~ a + b + c, data = d)
  m2 <- lm(y ~ a + x + c, data = d)   # x is new, shares a and c
  m3 <- glm(yb ~ z + a, data = d, family = binomial)
  expect_equal(dc(list(m1, m2)), sg_printed_rows(m1, m2))
  expect_equal(dc(list(m1, m2, m3)), sg_printed_rows(m1, m2, m3))
  expect_equal(dc(list(m3, m1)), sg_printed_rows(m3, m1))
})

test_that("coef engine matches stargazer output: factors and interactions", {
  m <- lm(y ~ a * b + f, data = d)
  expect_equal(dc(list(m)), sg_printed_rows(m))
})

test_that("coef engine matches stargazer output: omit / keep / order", {
  m1 <- lm(y ~ a + b + c + x, data = d)
  m2 <- lm(y ~ a + b + z, data = d)
  expect_equal(dc(list(m1, m2), omit = "^b$"),
               sg_printed_rows(m1, m2, omit = "^b$"))
  expect_equal(dc(list(m1, m2), keep = c("^a$", "^z$")),
               sg_printed_rows(m1, m2, keep = c("^a$", "^z$")))
  expect_equal(dc(list(m1, m2), order = c("^z$", "^c$")),
               sg_printed_rows(m1, m2, order = c("^z$", "^c$")))
  expect_equal(
    dc(list(m1, m2), keep = c("^a$", "^x$", "^z$"),
       order = c("^x$", "^z$", "^a$")),
    sg_printed_rows(m1, m2, keep = c("^a$", "^x$", "^z$"),
                    order = c("^x$", "^z$", "^a$"))
  )
})

test_that("keep overrides omit, mirroring stargazer", {
  m <- lm(y ~ a + b + c, data = d)
  expect_equal(dc(list(m), omit = "^a$", keep = "^a$"),
               sg_printed_rows(m, omit = "^a$", keep = "^a$"))
})

test_that("aliased (NA) terms keep their row, matching stargazer", {
  d2 <- d
  d2$dup <- d2$a                       # perfectly collinear -> NA coefficient
  m <- lm(y ~ a + dup + b, data = d2)
  expect_equal(dc(list(m)), sg_printed_rows(m))
})

test_that("long covariate names: coef engine returns clean names", {
  d3 <- d
  d3$this_is_an_extremely_long_covariate_name_that_wraps <- rnorm(n)
  d3$another_very_long_variable_name_for_testing <- rnorm(n)
  m <- lm(y ~ this_is_an_extremely_long_covariate_name_that_wraps +
            another_very_long_variable_name_for_testing + a, data = d3)
  expect_equal(
    dc(list(m)),
    c("this_is_an_extremely_long_covariate_name_that_wraps",
      "another_very_long_variable_name_for_testing", "a")
  )
})

test_that("paste()-built formulas (header junk) do not leak into labels", {
  rhs <- paste(c("a", "b", "c"), collapse = " + ")
  m <- lm(stats::as.formula(paste("y ~", rhs)), data = d)
  # legacy text engine chokes on the deparsed call in the header;
  # the coef engine never sees it
  expect_equal(dc(list(m)), c("a", "b", "c"))
})

test_that("star_var end-to-end: coef and text engines agree when Constant prints", {
  clear_label_mappings()
  set_label_mappings(c("^a$" = "Alpha", "^b$" = "Beta"))
  m1 <- lm(y ~ a + b + c, data = d)
  m2 <- lm(y ~ a + x, data = d)
  expect_equal(
    star_var(m1, m2, order = c("^x$")),
    scholr:::.star_var_text(m1, m2, order = c("^x$"))
  )
  clear_label_mappings()
})

test_that("with keep, coef engine drops the stats-line junk legacy appended", {
  # keep= suppresses the Constant row, so the legacy parser never truncated
  # and swept "Observations", "R2", "Note:" etc. into the label vector
  # (stargazer discarded the excess labels, hiding the bug).
  clear_label_mappings()
  m1 <- lm(y ~ a + b + c, data = d)
  m2 <- lm(y ~ a + x, data = d)
  kp <- c("^a$", "^b$", "^x$")
  legacy <- scholr:::.star_var_text(m1, m2, keep = kp, order = "^x$")
  exact <- star_var(m1, m2, keep = kp, order = "^x$")
  expect_equal(exact, c("x", "a", "b"))
  expect_equal(legacy[seq_along(exact)], exact)  # same prefix
  expect_true(length(legacy) > length(exact))    # legacy trailed junk
  expect_true("Observations" %in% legacy)
  clear_label_mappings()
})

test_that("models in a plain list are flattened like stargazer does", {
  m1 <- lm(y ~ a + b, data = d)
  m2 <- lm(y ~ a + c, data = d)
  expect_equal(star_var(list(m1, m2)), star_var(m1, m2))
})

test_that("coeftest objects are supported by the coef engine", {
  skip_if_not_installed("lmtest")
  m <- lm(y ~ a + b, data = d)
  ct <- lmtest::coeftest(m)
  expect_equal(dc(list(ct)), c("a", "b"))
})
