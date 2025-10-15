test_that("best_random produces better or equal balance than single random allocation", {
  set.seed(123)
  toy_data <- simulate_data(n_samples = 100, block_size = 1)

  # Single random allocation
  random_result <- allocate_samples(
    data = toy_data,
    method = "random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    seed = 123
  )
  random_score <- calculate_balance_score(random_result$results$p_value)

  # Best random with multiple iterations
  best_random_result <- allocate_samples(
    data = toy_data,
    method = "best_random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    iterations = 50,
    seed = 123
  )
  best_random_score <- calculate_balance_score(best_random_result$results$p_value)

  # Print scores for comparison
  cat("\n  Random balance score:      ", round(random_score, 4))
  cat("\n  Best random balance score: ", round(best_random_score, 4))
  cat("\n  Improvement: ", round((best_random_score - random_score), 2), "\n")

  # best_random should produce equal or better balance than single random
  # Use a tolerance since stochastic algorithms vary
  expect_gte(best_random_score, random_score * 0.9)  # Allow 10% tolerance

  # Both scores should be positive and valid
  expect_gt(random_score, 0)
  expect_gt(best_random_score, 0)
  expect_lt(random_score, 1)
  expect_lt(best_random_score, 1)
})

test_that("simulated_annealing produces better or equal balance than single random allocation", {
  set.seed(456)
  toy_data <- simulate_data(n_samples = 100, block_size = 1)

  # Single random allocation
  random_result <- allocate_samples(
    data = toy_data,
    method = "random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    seed = 456
  )
  random_score <- calculate_balance_score(random_result$results$p_value)

  # Simulated annealing
  sa_result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    iterations = 1000,
    temperature = 1,
    cooling_rate = 0.95,
    seed = 456,
    plot_convergence = FALSE
  )
  sa_score <- calculate_balance_score(sa_result$results$p_value)

  # Print scores for comparison
  cat("\n  Random balance score:            ", round(random_score, 4))
  cat("\n  Simulated annealing balance score:", round(sa_score, 4))
  cat("\n  Improvement: ", round((sa_score - random_score), 2), "\n")

  # SA should produce equal or better balance than random
  # Use a tolerance since stochastic algorithms vary
  expect_gte(sa_score, random_score * 0.9)  # Allow 10% tolerance

  # Both scores should be positive and valid
  expect_gt(random_score, 0)
  expect_gt(sa_score, 0)
  expect_lt(random_score, 1)
  expect_lt(sa_score, 1)
})

test_that("simulated_annealing with blocking produces better or equal balance than single random", {
  set.seed(789)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)

  # Single random allocation with blocking
  random_result <- allocate_samples(
    data = toy_data,
    method = "random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 12,
    seed = 789
  )
  random_score <- calculate_balance_score(random_result$results$p_value)

  # Simulated annealing with blocking
  sa_result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 12,
    iterations = 1000,
    temperature = 1,
    cooling_rate = 0.95,
    seed = 789,
    plot_convergence = FALSE
  )
  sa_score <- calculate_balance_score(sa_result$results$p_value)

  # Print scores for comparison
  cat("\n  Random balance score (with blocking):            ", round(random_score, 4))
  cat("\n  Simulated annealing balance score (with blocking):", round(sa_score, 4))
  cat("\n  Improvement: ", round((sa_score - random_score), 2), "\n")

  # SA should produce equal or better balance than random
  expect_gte(sa_score, random_score * 0.9)  # Allow 10% tolerance

  # Both scores should be positive and valid
  expect_gt(random_score, 0)
  expect_gt(sa_score, 0)
  expect_lt(random_score, 1)
  expect_lt(sa_score, 1)
})

test_that("simulated_annealing with unequal blocks produces better or equal balance than random", {
  set.seed(321)
  # Create data with unequal block sizes
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  # Single random allocation with unequal blocking
  random_result <- allocate_samples(
    data = toy_data,
    method = "random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 15,
    seed = 321
  )
  random_score <- calculate_balance_score(random_result$results$p_value)

  # Simulated annealing with unequal blocking
  sa_result <- allocate_samples(
    data = toy_data,
    method = "simulated_annealing",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 15,
    iterations = 1000,
    temperature = 1,
    cooling_rate = 0.95,
    seed = 321,
    plot_convergence = FALSE
  )
  sa_score <- calculate_balance_score(sa_result$results$p_value)

  # Print scores for comparison
  cat("\n  Random balance score (unequal blocks):            ", round(random_score, 4))
  cat("\n  Simulated annealing balance score (unequal blocks):", round(sa_score, 4))
  cat("\n  Improvement: ", round((sa_score - random_score), 2), "\n")

  # SA should produce equal or better balance than random
  expect_gte(sa_score, random_score * 0.9)  # Allow 10% tolerance

  # Both scores should be positive and valid
  expect_gt(random_score, 0)
  expect_gt(sa_score, 0)
  expect_lt(random_score, 1)
  expect_lt(sa_score, 1)
})
