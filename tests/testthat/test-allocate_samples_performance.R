# ============================================================================
# Tests that optimisation algorithms improve balance from a known-bad layout
# ============================================================================

test_that("best_random improves balance from a deliberately biased layout", {
  set.seed(42)
  toy_data <- simulate_data(n_samples = 100, block_size = 1)

  # Create a deliberately biased layout: sort by age so batch 1
  # gets the youngest samples, batch 5 gets the oldest
  biased_layout <- toy_data[order(toy_data$age_at_baseline), ]
  biased_layout$batch_allocation <- factor(rep(1:5, each = 20))

  # Measure p-values of the biased layout
  biased_results <- test_covariates(biased_layout, blocking_variable = NULL)
  biased_score <- calculate_balance_score(p_values = biased_results$p_value)

  # The biased layout should have a poor balance score (low p-values)
  expect_lt(biased_score, 0.1)

  # Best random with multiple iterations should find a better layout
  best_random_result <- allocate_samples(
    data = toy_data,
    method = "best_random",
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    iterations = 50,
    seed = 42
  )
  best_random_results <- test_covariates(best_random_result$layout, blocking_variable = NULL)
  best_random_score <- calculate_balance_score(p_values = best_random_results$p_value)

  # best_random should produce strictly better balance than the biased layout
  expect_gt(best_random_score, biased_score)
})

test_that("SA improves balance from a deliberately biased layout (no blocking)", {
  set.seed(42)
  toy_data <- simulate_data(n_samples = 100, block_size = 1)

  # Create a deliberately biased layout: sort by age
  biased_layout <- toy_data[order(toy_data$age_at_baseline), ]
  biased_layout$batch_allocation <- factor(rep(1:5, each = 20))

  # Measure p-values of the biased layout
  biased_results <- test_covariates(biased_layout, blocking_variable = NULL)
  biased_score <- calculate_balance_score(p_values = biased_results$p_value)

  # The biased layout should have a poor balance score
  expect_lt(biased_score, 0.1)

  # Run SA starting from the biased layout
  sa_result <- simulate_annealing(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    batch_size = 20,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 300,
    plot = FALSE,
    balance_type = "p_value",
    initial_layout = biased_layout
  )

  # SA should have strictly improved the balance score
  sa_results <- test_covariates(sa_result$layout, blocking_variable = NULL)
  sa_score <- calculate_balance_score(p_values = sa_results$p_value)
  expect_gt(sa_score, biased_score)
})

test_that("SA improves balance from a deliberately biased layout (equal block sizes)", {
  set.seed(42)
  toy_data <- simulate_data(n_samples = 60, block_size = 3)

  # Create a biased layout: sort by age, assign whole blocks to batches
  # so that blocks with younger subjects cluster in earlier batches
  toy_data_sorted <- toy_data[order(toy_data$age_at_baseline), ]
  subjects_ordered <- unique(toy_data_sorted$subject_id)
  n_per_batch <- 4  # subjects per batch (12 samples / 3 per block)
  n_batches <- ceiling(length(subjects_ordered) / n_per_batch)
  subject_batch <- setNames(
    rep(1:n_batches, each = n_per_batch, length.out = length(subjects_ordered)),
    subjects_ordered
  )
  biased_layout <- toy_data_sorted
  biased_layout$batch_allocation <- factor(subject_batch[as.character(biased_layout$subject_id)])

  # Pad to fill batches
  batch_counts <- table(biased_layout$batch_allocation)
  padding_rows <- do.call(rbind, lapply(names(batch_counts), function(b) {
    n_pad <- 12 - batch_counts[b]
    if (n_pad > 0) {
      pad <- as.data.frame(matrix(NA, nrow = n_pad, ncol = ncol(biased_layout)))
      colnames(pad) <- colnames(biased_layout)
      pad$sample_id <- paste0("padding_", b, "_", seq_len(n_pad))
      pad$batch_allocation <- factor(b, levels = levels(biased_layout$batch_allocation))
      pad
    }
  }))
  if (!is.null(padding_rows)) {
    biased_layout <- rbind(biased_layout, padding_rows)
  }

  # Measure p-values of the biased layout
  biased_results <- test_covariates(biased_layout, blocking_variable = "subject_id")
  biased_score <- calculate_balance_score(p_values = biased_results$p_value)

  # Run SA starting from the biased layout
  sa_result <- simulate_annealing(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = 12,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 300,
    plot = FALSE,
    balance_type = "p_value",
    initial_layout = biased_layout
  )

  # SA should have strictly improved the balance score
  sa_results <- test_covariates(sa_result$layout, blocking_variable = "subject_id")
  sa_score <- calculate_balance_score(p_values = sa_results$p_value)
  expect_gt(sa_score, biased_score)

  # Verify blocks stay together
  layout_no_padding <- sa_result$layout[!grepl("padding", sa_result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))
  expect_true(all(subjects_split$n_batches == 1))
})

test_that("SA improves balance from a deliberately biased layout (unequal block sizes)", {
  set.seed(42)
  toy_data <- simulate_data(n_samples = 102, block_size = 6) %>%
    dplyr::slice(-sample(1:dplyr::n(), 12))

  batch_size <- 15

  # Verify unequal block sizes exist
  block_sizes_table <- table(toy_data$subject_id)
  expect_true(length(unique(block_sizes_table)) > 1)

  # Create a biased layout using the same first-fit bin packing as
  # allocate_single_random, but sorting blocks by mean age instead of
  # randomising. This clusters subjects with similar ages into the
  # same batches, creating deliberate covariate imbalance.
  subject_mean_age <- toy_data %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(mean_age = mean(age_at_baseline, na.rm = TRUE)) %>%
    dplyr::arrange(mean_age)

  block_df <- data.frame(
    block = subject_mean_age$subject_id,
    size = as.numeric(block_sizes_table[subject_mean_age$subject_id]),
    stringsAsFactors = FALSE
  )

  # First-fit bin packing (same logic as allocate_single_random Case 2b)
  batch_capacities <- c()
  batch_assignments <- setNames(integer(nrow(block_df)), block_df$block)

  for (i in seq_len(nrow(block_df))) {
    blk <- block_df$block[i]
    blk_size <- block_df$size[i]
    placed <- FALSE
    for (b in seq_along(batch_capacities)) {
      if (batch_capacities[b] + blk_size <= batch_size) {
        batch_capacities[b] <- batch_capacities[b] + blk_size
        batch_assignments[blk] <- b
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      batch_capacities <- c(batch_capacities, blk_size)
      batch_assignments[blk] <- length(batch_capacities)
    }
  }

  # Assign samples to batches
  biased_data <- toy_data
  biased_data$batch_allocation <- factor(
    sapply(as.character(biased_data$subject_id), function(id) batch_assignments[[id]])
  )

  # Pad batches
  actual_per_batch <- table(biased_data$batch_allocation)
  unfilled <- batch_size - actual_per_batch
  padding <- do.call(rbind, lapply(names(unfilled), function(b) {
    if (unfilled[b] > 0) {
      pad <- as.data.frame(matrix(NA, nrow = unfilled[b], ncol = ncol(biased_data)))
      colnames(pad) <- colnames(biased_data)
      pad$batch_allocation <- factor(rep(b, unfilled[b]),
                                     levels = levels(biased_data$batch_allocation))
      pad
    }
  }))
  if (!is.null(padding) && nrow(padding) > 0) {
    padding$sample_id <- paste0("padding", seq_len(nrow(padding)))
    biased_layout <- dplyr::bind_rows(biased_data, padding)
  } else {
    biased_layout <- biased_data
  }

  # Measure p-values of the biased layout
  biased_results <- test_covariates(biased_layout, blocking_variable = "subject_id")
  biased_score <- calculate_balance_score(p_values = biased_results$p_value)

  # Run SA for unequal blocks starting from the biased layout
  sa_result <- simulate_annealing_for_unequal_block_size(
    data = toy_data,
    covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
    blocking_variable = "subject_id",
    batch_size = batch_size,
    temperature = 1,
    cooling_rate = 0.95,
    iterations = 300,
    plot = FALSE,
    balance_type = "p_value",
    initial_layout = biased_layout
  )

  # SA should have strictly improved the balance score
  sa_results <- test_covariates(sa_result$layout, blocking_variable = "subject_id")
  sa_score <- calculate_balance_score(p_values = sa_results$p_value)
  expect_gt(sa_score, biased_score)

  # Verify blocks stay together
  layout_no_padding <- sa_result$layout[!grepl("padding", sa_result$layout$sample_id), ]
  subjects_split <- layout_no_padding %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(n_batches = dplyr::n_distinct(batch_allocation))
  expect_true(all(subjects_split$n_batches == 1))
})
