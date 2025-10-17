test_that("allocate_single_random works with no blocking (Case 1)", {
  # Create test data without blocking
  set.seed(123)
  toy_data <- simulate_data(n_samples = 50, block_size = 1)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex")]

  batch_size <- 10

  result <- allocate_single_random(
    data = toy_data,
    batch_size = batch_size
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))

  # Check that all samples are allocated
  expect_true("batch_allocation" %in% names(result$layout))

  # Check that no batch exceeds batch_size
  batch_counts <- table(result$layout$batch_allocation)
  expect_true(all(batch_counts <= batch_size))

  # Check that each batch has exactly batch_size samples (including padding)
  expect_true(all(batch_counts == batch_size))
})

test_that("allocate_single_random works with equal block sizes (Case 2a)", {
  # Create test data with equal block sizes
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  batch_size <- 12

  result <- allocate_single_random(
    data = toy_data,
    batch_size = batch_size,
    blocking_variable = "subject_id"
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))

  # Check that all samples are allocated
  expect_true("batch_allocation" %in% names(result$layout))

  # Check that no batch exceeds batch_size
  batch_counts <- table(result$layout$batch_allocation)
  expect_true(all(batch_counts <= batch_size))

  # Check that blocks are kept together
  # For each subject, all samples should be in the same batch
  layout_no_padding <- result$layout[!grepl("padding", result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))

  expect_true(all(subjects_split$n_batches == 1))
})

test_that("allocate_single_random works with unequal block sizes (Case 2b)", {
  # Create test data with unequal block sizes
  set.seed(123)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  batch_size <- 15

  result <- allocate_single_random(
    data = toy_data,
    batch_size = batch_size,
    blocking_variable = "subject_id"
  )

  # Check result structure
  expect_true("layout" %in% names(result))
  expect_true("results" %in% names(result))

  # Check that all samples are allocated
  expect_true("batch_allocation" %in% names(result$layout))

  # Check that no batch exceeds batch_size
  batch_counts <- table(result$layout$batch_allocation)
  expect_true(all(batch_counts <= batch_size))

  # Check that blocks are kept together
  # For each subject, all samples should be in the same batch
  layout_no_padding <- result$layout[!grepl("padding", result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))

  expect_true(all(subjects_split$n_batches == 1))

  # Verify unequal block sizes exist in test data
  block_sizes <- table(toy_data$subject_id)
  expect_true(length(unique(block_sizes)) > 1)
})

test_that("allocate_single_random throws error when block size exceeds batch size", {
  # Create test data where a block is larger than batch_size
  set.seed(123)
  toy_data <- simulate_data(n_samples = 60, block_size = 6)
  toy_data <- toy_data[, c("sample_id", "age_at_baseline", "bmi_at_baseline", "sex", "subject_id")]

  # Set batch_size smaller than block_size
  batch_size <- 5

  expect_error(
    allocate_single_random(
      data = toy_data,
      batch_size = batch_size,
      blocking_variable = "subject_id"
    ),
    "The maximum number of rows for a single level of the blocking variable exceeds half the batch size - there is little flexibility to create bias free layouts."
  )
})
