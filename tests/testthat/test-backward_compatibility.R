# Backward Compatibility Tests
# Ensure that old code still works with the new balance_type parameter

test_that("allocate_samples without balance_type defaults to p_value mode", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  # Call without specifying balance_type (old API)
  result <- allocate_samples(
    data = toy_data,
    method = "random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 12,
    seed = 123
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))

  # Should have both p_value and effect_size columns
  expect_true("p_value" %in% names(result$results))
  expect_true("effect_size" %in% names(result$results))
})

test_that("calculate_balance_score with just p_values vector (old API) still works", {
  # Old API: just pass a vector of p_values
  p_vals <- c(0.5, 0.6, 0.7)

  # This should work without specifying balance_type
  score <- calculate_balance_score(p_values = p_vals)

  # Check that score is a single numeric value
  expect_true(is.numeric(score))
  expect_length(score, 1)
  expect_true(score >= 0 & score <= 1)
})

test_that("calculate_balance_score explicitly with balance_type='p_value' works", {
  p_vals <- c(0.5, 0.6, 0.7)

  # Explicitly specify balance_type='p_value'
  score <- calculate_balance_score(p_values = p_vals, balance_type = "p_value")

  # Check that score is a single numeric value
  expect_true(is.numeric(score))
  expect_length(score, 1)
  expect_true(score >= 0 & score <= 1)
})

test_that("all four parameter combinations work (balance_type × balance_metric)", {
  p_vals <- c(0.5, 0.6, 0.7)
  effects <- c(0.1, 0.2, 0.15)

  # 1. p_value + harmonic_mean (default)
  score1 <- calculate_balance_score(
    p_values = p_vals,
    balance_type = "p_value",
    balance_metric = "harmonic_mean"
  )
  expect_true(is.numeric(score1))
  expect_length(score1, 1)

  # 2. p_value + product
  score2 <- calculate_balance_score(
    p_values = p_vals,
    balance_type = "p_value",
    balance_metric = "product"
  )
  expect_true(is.numeric(score2))
  expect_length(score2, 1)

  # 3. effect_size + harmonic_mean
  score3 <- calculate_balance_score(
    effect_sizes = effects,
    balance_type = "effect_size",
    balance_metric = "harmonic_mean"
  )
  expect_true(is.numeric(score3))
  expect_length(score3, 1)

  # 4. effect_size + product
  score4 <- calculate_balance_score(
    effect_sizes = effects,
    balance_type = "effect_size",
    balance_metric = "product"
  )
  expect_true(is.numeric(score4))
  expect_length(score4, 1)
})
