test_that("basic formatting utilities are vectorized", {
    expect_equal(number_to_word(c(1, 5, 11)), c("one", "five", "11"))
    expect_equal(na_to_dash(c(1, NA, 3)), c("1", "-", "3"))
    expect_equal(na_to_blank(c(1, NA, 3)), c("1", "", "3"))
    expect_equal(c(1, 2, 3) %nin% c(2, 4), c(TRUE, FALSE, TRUE))
})
