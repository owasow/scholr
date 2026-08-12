test_that("title casing handles its documented special cases", {
    expect_equal(to_title_case("the quick brown fox"), "The Quick Brown Fox")
    expect_equal(
        to_title_case("a tale of two cities: the best of times"),
        "A Tale of Two Cities: The Best of Times"
    )
    expect_equal(
        to_title_case("testing iPhone and http://example.com"),
        "Testing iPhone and http://example.com"
    )
    expect_equal(to_title_case("US policy with dplyr"), "US Policy with dplyr")
})

test_that("title casing is vectorized and preserves missing values", {
    expect_equal(
        to_title_case(c("the first title", NA_character_, "", "the second title")),
        c("The First Title", NA_character_, "", "The Second Title")
    )
    expect_identical(to_title_case(character()), character())
    expect_error(to_title_case(1:2), "character vector")
})
