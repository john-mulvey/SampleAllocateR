test_that("allocate_samples routes to correct method for no blocking", {
  # Create test data without blocking
  set.seed(123)
  toy_data <- simulate_data(n_samples = 50, block_size = 1)

  # Mock simulate_annealing to check it's called
  # We'll test by checking the output structure is correct
  result <- allocate_samples(
    data = toy_data,
    id_column = "sample_id",
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 10,
    iterations = 10,
    plot_convergence = FALSE
  )

  # Check that result has expected structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))
})

test_that("allocate_samples routes to correct method for equal block sizes", {
  # Create test data with equal block sizes
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)

  result <- allocate_samples(
    data = toy_data,
    id_column = "sample_id",
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 12,
    iterations = 10,
    plot_convergence = FALSE
  )

  # Check that result has expected structure (from simulate_annealing)
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))
})

test_that("allocate_samples routes to simulate_annealing_for_unequal_block_size for unequal blocks", {
  # Create test data with unequal block sizes by removing some samples
  set.seed(123)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  result <- allocate_samples(
    data = toy_data,
    id_column = "sample_id",
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 15,
    iterations = 10,
    plot_convergence = FALSE
  )

  # Check that result has expected structure (from simulate_annealing_for_unequal_block_size)
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("optimisation_data" %in% names(result))

  # The experimental version should have additional columns in optimisation_data
  expect_true("swap_possible" %in% names(result$optimisation_data))
  expect_true("swap_size" %in% names(result$optimisation_data))
})
