test_that(
  "createInitialModels works with all combinations of numeric and character input", {

    # character-character
    expect_vector(
      createInitialModels(
        maxFactors = c("fac1", "fac2", "fac3"),
        items = paste0("item", 1:9)
      ),
      'character',
      2
    )
    # character-numeric
    expect_vector(
      createInitialModels(
        maxFactors = c("fac1", "fac2", "fac3"),
        items = 9
      ),
      'character',
      2
    )
    # numeric-numeric
    expect_vector(
      createInitialModels(
        maxFactors = 4,
        items = 9
      ),
      'character',
      3
    )
    # numeric-character
    expect_vector(
      createInitialModels(
        maxFactors = 5,
        items = paste0("item", 1:20)
      ),
      'character',
      4
    )
  }
)