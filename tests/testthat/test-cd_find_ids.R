test_that("can find ID", {
  # Create a data frame
  results <- data.frame(
    id = 1:3,
    code1 = c("A", "B", "C"),
    code2 = c("B", "C", "D")
  ) |>
    cd_find_ids(c("code1", "code2"), c("A", "B")) # Find A and B in code1 and code2

  expect_equal( # Check if the results are correct
    all(results$any_codes == c(TRUE, TRUE, FALSE)),
    is.list((results$codes_found))
  )
})
