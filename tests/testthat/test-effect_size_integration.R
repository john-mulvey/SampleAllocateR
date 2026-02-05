# Tests for Effect Size Functionality

test_that("calculate_balance_score with invalid balance_type throws error", {
  p_vals <- c(0.5, 0.6, 0.7)

  expect_error(
    calculate_balance_score(p_values = p_vals, balance_type = "invalid"),
    "balance_type must be 'p_value' or 'effect_size'"
  )
})

test_that("calculate_balance_score with balance_type='effect_size' but no effect_sizes throws error", {
  expect_error(
    calculate_balance_score(balance_type = "effect_size"),
    "effect_sizes required when balance_type = 'effect_size'"
  )
})

test_that("calculate_effect_sizes returns correct structure", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  # Create a simple allocation
  toy_data$batch_allocation <- factor(rep(1:5, each = 12))

  result <- calculate_effect_sizes(
    layout = toy_data,
    blocking_variable = NULL
  )

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("covariate" %in% names(result))
  expect_true("effect_size" %in% names(result))

  # Check that it detected all covariates (excluding sample_id and batch_allocation)
  expected_covariates <- c("age_at_baseline", "bmi_at_baseline", "sex")
  expect_equal(sort(result$covariate), sort(expected_covariates))

  # Check effect sizes are numeric and in valid range
  expect_true(all(is.numeric(result$effect_size)))
  expect_true(all(result$effect_size >= 0 & result$effect_size <= 1, na.rm = TRUE))
})

test_that("calculate_effect_sizes excludes blocking variable and padding samples", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  # Create a simple allocation and add padding
  toy_data$batch_allocation <- factor(rep(1:5, each = 12))
  toy_data <- rbind(
    toy_data,
    data.frame(
      sample_id = "padding_1",
      age_at_baseline = NA,
      bmi_at_baseline = NA,
      sex = factor(NA),
      subject_id = "padding",
      batch_allocation = factor(1)
    )
  )

  result <- calculate_effect_sizes(
    layout = toy_data,
    blocking_variable = "subject_id"
  )

  # Check that subject_id is not in the results
  expect_false("subject_id" %in% result$covariate)

  # Check that we got results for the other covariates
  expected_covariates <- c("age_at_baseline", "bmi_at_baseline", "sex")
  expect_equal(sort(result$covariate), sort(expected_covariates))
})
