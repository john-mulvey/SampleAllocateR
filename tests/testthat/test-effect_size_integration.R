# Integration Tests for Effect Size Functionality
# Tests that the new balance_type="effect_size" parameter works end-to-end

test_that("allocate_samples with method='random' returns p_values (does not support effect_size)", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  # Note: method="random" does not support balance_type parameter
  # It always returns p_values via test_covariates()
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
  expect_true("batch_allocation" %in% names(result$layout))

  # Random method always returns p_values
  expect_true("p_value" %in% names(result$results))
})

test_that("allocate_samples with method='best_random' and balance_type='effect_size' works", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  result <- allocate_samples(
    data = toy_data,
    method = "best_random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 12,
    balance_type = "effect_size",
    iterations = 20,
    seed = 123
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("batch_allocation" %in% names(result$layout))

  # Note: results still contains p_values (from test_covariates),
  # but balance_type="effect_size" means effect sizes were used to SELECT the best layout
  expect_true("p_value" %in% names(result$results))

  # Verify the function completed without error
  expect_true(nrow(result$layout) > 0)
})

test_that("allocate_samples with method='simulated_annealing' and balance_type='effect_size' works", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 12,
    balance_type = "effect_size",
    iterations = 100,
    seed = 123
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))  # Note: British spelling
  expect_true("batch_allocation" %in% names(result$layout))

  # Results still contains p_values (from test_covariates)
  expect_true("p_value" %in% names(result$results))

  # Verify optimisation_data has objective_value column
  expect_true("objective_value" %in% names(result$optimisation_data))

  # Verify the function completed without error
  expect_true(nrow(result$layout) > 0)
  expect_gt(nrow(result$optimisation_data), 0)
})

test_that("effect_size mode with blocking (equal block sizes) works", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 12,
    balance_type = "effect_size",
    iterations = 100,
    seed = 123
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))

  # Results contains p_values
  expect_true("p_value" %in% names(result$results))

  # Check that blocks are kept together
  layout_no_padding <- result$layout[!grepl("padding", result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))

  expect_true(all(subjects_split$n_batches == 1))
})

test_that("effect_size mode with blocking (unequal block sizes) works and routes correctly", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  # Verify unequal block sizes exist
  block_sizes <- table(toy_data$subject_id)
  expect_true(length(unique(block_sizes)) > 1)

  result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 15,
    balance_type = "effect_size",
    iterations = 100,
    seed = 123
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))

  # Results contains p_values
  expect_true("p_value" %in% names(result$results))

  # Check that it routed to unequal block size algorithm (has swap_possible column)
  expect_true("swap_possible" %in% names(result$optimisation_data))

  # Check that blocks are kept together
  layout_no_padding <- result$layout[!grepl("padding", result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))

  expect_true(all(subjects_split$n_batches == 1))
})

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
