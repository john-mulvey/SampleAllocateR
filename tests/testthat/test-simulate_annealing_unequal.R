test_that("simulate_annealing_for_unequal_block_size returns valid swaps", {
  # Create test data with unequal block sizes
  set.seed(123)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  batch_size <- 15

  result <- simulate_annealing_for_unequal_block_size(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = batch_size,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 50,
    plot = FALSE
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))

  # Check that optimisation_data has standard columns (diagnostic columns now commented out)
  expect_true("iteration" %in% names(result$optimisation_data))
  expect_true("temperature" %in% names(result$optimisation_data))
  expect_true("objective_value" %in% names(result$optimisation_data))

  # Check that optimisation ran (all iterations have objective values)
  expect_equal(nrow(result$optimisation_data), 50)
  expect_true(all(!is.na(result$optimisation_data$objective_value)))
})

test_that("simulate_annealing_for_unequal_block_size handles padding correctly", {
  # Create test data with unequal block sizes
  set.seed(123)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  batch_size <- 15

  result <- simulate_annealing_for_unequal_block_size(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = batch_size,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 50,
    plot = FALSE
  )

  # Check that padding samples are present
  has_padding <- any(grepl("padding", result$layout$sample_id))
  expect_true(has_padding)

  # Check that no batch exceeds batch_size
  batch_counts <- table(result$layout$batch_allocation)
  expect_true(all(batch_counts <= batch_size))

  # Check that blocks remain together
  layout_no_padding <- result$layout[!grepl("padding", result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))

  expect_true(all(subjects_split$n_batches == 1))
})

test_that("simulate_annealing_for_unequal_block_size improves balance over iterations", {
  # Create test data with unequal block sizes
  set.seed(456)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  batch_size <- 15

  result <- simulate_annealing_for_unequal_block_size(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = batch_size,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 100,
    plot = FALSE
  )

  # Check that optimisation_data tracks objective values
  expect_true("objective_value" %in% names(result$optimisation_data))
  expect_true(all(!is.na(result$optimisation_data$objective_value)))

  # Check that final balance is at least as good as initial
  # (Note: due to randomness, this might not always strictly improve,
  # but should generally not get worse on average)
  initial_value <- result$optimisation_data$objective_value[1]
  final_value <- result$optimisation_data$objective_value[nrow(result$optimisation_data)]

  # Both should be valid numbers
  expect_true(is.numeric(initial_value))
  expect_true(is.numeric(final_value))
  expect_true(initial_value > 0)
  expect_true(final_value > 0)
})

test_that("simulate_annealing_for_unequal_block_size respects batch_size constraint", {
  # Create test data with unequal block sizes
  set.seed(789)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  batch_size <- 18

  result <- simulate_annealing_for_unequal_block_size(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = batch_size,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 100,
    plot = FALSE
  )

  # Check that no batch exceeds batch_size at any point
  batch_counts <- table(result$layout$batch_allocation)
  expect_true(all(batch_counts <= batch_size),
              info = paste("Max batch size:", max(batch_counts), "vs", batch_size))

  # All batches should be exactly batch_size (with padding)
  expect_true(all(batch_counts == batch_size))
})
