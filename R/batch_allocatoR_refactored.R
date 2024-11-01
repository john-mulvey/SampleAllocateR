## ---------------------------------------------------------------------------------------------------------------------------------
# prevent R CMD check warning about missing visible binding for global varaiables (due to non-standard evaluation by dplyr)
utils::globalVariables(c(
  "Temperature", "Value", "Variable", "adj_p_value", "batch", "batch_allocation", "covariate", "balance_score", "objective_value",
  "p_value", "sample_id", "value"
))

## ---------------------------------------------------------------------------------------------------------------------------------
#' Simulate Data with Optional Blocking
#'
#' This function generates a simulated dataset containing a specified number of samples. Users can optionally specify block sizes to group samples into blocks. If the total number of samples is not a multiple of the block size, additional samples are generated to complete the last block.
#'
#' @param n_samples Integer; the number of samples to generate. If `block_size` is specified and `n_samples` is not a multiple of `block_size`, the function will generate additional samples to ensure all blocks are complete.
#' @param block_size Integer; the size of each block for blocking variable creation. If `NA` (the default), no blocking is applied. If specified, `block_size` must be a positive integer, and the function will create a blocking variable to group samples into blocks of this size.
#' @param seed Integer; the seed for random number generation to ensure reproducibility.
#'
#' @return A `data.frame` with columns for sample ID, three covariates (`covariate1`, `covariate2`, `covariate3`), and, if `block_size` is specified, a `block_id` column. The first two covariates are generated from uniform and normal distributions, respectively, while the third is a categorical variable with levels "A", "B", and "C". If blocking is applied, a `block_id` column indicates the block to which each sample belongs.
#'
#' @details
#' The function allows for the simulation of data with or without blocking. When `block_size` is provided, it ensures that the data is divided into blocks of the specified size, potentially increasing the total number of samples to meet this requirement. This is particularly useful for simulations or analyses where the concept of blocks is relevant.
#'
#' @examples
#' # Generate a dataset without blocking
#' simulate_data(n_samples = 100)
#'
#' # Generate a dataset with blocking, block size of 10
#' simulate_data(n_samples = 95, block_size = 10)
#'
#' @importFrom stats rnorm runif
#'
#' @export
#'

simulate_data <- function(n_samples, block_size = NA, seed = 123) {
  # Validate input params
  if (!is.numeric(n_samples) || n_samples <= 0 || floor(n_samples) != n_samples) {
    stop("n_samples must be a positive integer")
  }

  if (!is.na(block_size)) {
    if (!is.numeric(block_size) || block_size <= 0 || floor(block_size) != block_size) {
      stop("block_size must be a positive integer or NA")
    }
    if (block_size > n_samples) {
      stop("block_size cannot be larger than n_samples")
    }
  }

  if (!is.numeric(seed) || floor(seed) != seed) {
    stop("seed must be an integer")
  }

  # function
  set.seed(seed)

  if (missing(block_size)) {
    data <- data.frame(sample_id = paste0("Sample", 1:n_samples),
                       covariate1 = runif(n_samples),  # uniform distribtuion
                       covariate2 = rnorm(n_samples), # normal distribtuion
                       covariate3 = factor(sample(c("A", "B", "C"), n_samples, replace = TRUE)))

  } else {
    n_samples_blocked = ceiling(n_samples/block_size)*block_size
    if (n_samples_blocked != n_samples) {
      warning("The number of samples is not a multiple of the block size. We currently require complete blocks, so will simualte more samples than specified")
    }

    data <- data.frame(sample_id = paste0("Sample", 1:n_samples_blocked),
                        covariate1 = runif(n_samples_blocked), # uniform distribtuion
                        covariate2 = rnorm(n_samples_blocked), # normal distribtuion
                        covariate3 = factor(sample(c("A", "B", "C"), n_samples_blocked, replace = TRUE)),
                        block_id = factor(paste("block", rep(1:ceiling(n_samples_blocked / block_size), each = block_size, length.out = n_samples_blocked), sep = "_")))
}
  return(data)
}


## ---------------------------------------------------------------------------------------------------------------------------------
pad_samples <- function(data, batch_size) {
  n_samples <- nrow(data)

  # Calculate the number of batches, pad with empty spaces if required
  batch_n_needed <- ceiling(n_samples / batch_size)
  n_samples_padded <- batch_n_needed * batch_size

  # check if all batches are already full
  if (n_samples == n_samples_padded) {
    return(data)
  }
  else {
  ### Create a data frame with NA values and the same number of columns as 'data', and sample_id e.g. padded1
  padding_data = as.data.frame(matrix(NA, nrow = n_samples_padded - n_samples, ncol = ncol(data)))
  colnames(padding_data) <- colnames(data)
  padding_data$sample_id = paste0("padding", 1:(n_samples_padded - n_samples))

  data_padded <- rbind(data, padding_data)

  return(data_padded)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------
test_covariates = function(layout, blocking_variable = "block_id"){
  # test for differences in continuous covariates between batches
  continuous_vars <- names(layout)[sapply(layout, is.numeric)]
  continuous_vars <- continuous_vars[!continuous_vars %in% c("batch_allocation", blocking_variable)]

  test_results_continuous <- layout %>%
    dplyr::mutate(batch = batch_allocation) %>%
    dplyr::select(-sample_id) %>%
    tidyr::pivot_longer(cols = all_of(continuous_vars), names_to = "covariate", values_to = "value") %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(p_value =  kruskal.test(value ~ batch)$p.value)

  # test for differences in categorical covariates between batches
  factor_vars <- names(layout)[sapply(layout, is.factor)]
  factor_vars <- factor_vars[!factor_vars %in% c("batch_allocation", blocking_variable)]

  test_results_factor <- layout %>%
    dplyr::mutate(batch = batch_allocation) %>%
    dplyr::select(-sample_id) %>%
    tidyr::pivot_longer(cols = all_of(factor_vars), names_to = "covariate", values_to = "value") %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(p_value = {
      contingency_table <- table(droplevels(value), batch)
      if (sum(rowSums(contingency_table) == 0) > 0) {
        NA
      } else {
        stats::fisher.test(contingency_table, simulate.p.value = TRUE)$p.value
      }
    })

  # Combine the results for continuous and factor variables and reformat
  test_results <- dplyr::bind_rows(test_results_continuous, test_results_factor)

  # Combine the results for continuous and factor variables and reformat
  test_results <- dplyr::bind_rows(test_results_continuous, test_results_factor)

  result_table <- test_results %>%
    tidyr::pivot_wider(names_from = covariate, values_from = p_value)

  return(test_results)

}


## ---------------------------------------------------------------------------------------------------------------------------------
allocate_single_random <- function(data, batch_size, blocking_variable = NA) {

  n_samples <- nrow(data)

  # randomly allocate either samples or blocks
  if (missing(blocking_variable) || is.na(blocking_variable) || blocking_variable == "") {

    # Calculate the number of batches required, pad with empty spaces if required
    batch_n_needed <- ceiling(n_samples / batch_size)
    data_padded <- pad_samples(data, batch_size)

    # Randomly allocate a layout
    batch_allocations <- sample(rep(1:batch_n_needed, times = batch_size), batch_n_needed*batch_size, replace = FALSE)
    names(batch_allocations) <- data_padded$sample_id  # Set the names to "batch_allocations"

    data_padded$batch_allocation <- as.factor(batch_allocations)

    layout = data_padded
  } else {

    # check if all blocks are the same size, and stop and print a warning statement if not
    block_size = table(data[[blocking_variable]])
    all(block_size == block_size[1])
    if (!all(block_size == block_size[1])) {
      stop("Blocks are not all the same size, which is a current requirement of the method")
    }

    # Calculate the actual number of batches etc needed based on constraints
    block_size = block_size[1]
    batches_needed <- ceiling(n_samples / (floor(batch_size / block_size) * block_size))
    n_blocks <- length(unique(data[[blocking_variable]]))

    # Generate a random assignment of batch numbers for each block
    batch_assignments <- rep(1:batches_needed, each = ceiling(n_blocks / batches_needed)) %>%
      sample(size = n_blocks)
    batch_assignments <- setNames(batch_assignments, unique(data[[blocking_variable]]))

    data$batch_allocation <- unlist(lapply(data[[blocking_variable]], function(id) batch_assignments[id])) %>%
      as.factor()

    # pad empty slots
    actual_samples_per_batch <- table(data$batch_allocation)
    unfilled_spots <- batch_size - actual_samples_per_batch # Assuming each batch should have 13 samples

    # Step 3: Create new rows for unfilled spots
    empty_rows <- do.call(rbind, lapply(names(unfilled_spots), function(batch) {
      if (unfilled_spots[batch] > 0) {
        empty_row <- as.data.frame(matrix(NA, nrow = unfilled_spots[batch], ncol = ncol(data)))
        colnames(empty_row) <- colnames(data)
        empty_row$batch_allocation <- factor(rep(batch, unfilled_spots[batch]), levels = levels(data$batch_allocation))
        return(empty_row)
      }
    }))
    empty_rows$sample_id <- paste0("padding", seq_len(nrow(empty_rows)))

    # Step 4: Append new rows to data
    data <- dplyr::bind_rows(data, empty_rows)

    layout = data
  }

  # Test for covariate balance
  test_results <- test_covariates(layout, blocking_variable = blocking_variable)

  # Return the layout
  return(list(layout = layout,
              results = test_results))
}

## ---------------------------------------------------------------------------------------------------------------------------------
#' Calculate Balance Score Using Fisher's method for combining p-values
#'
#' This function calculates a covariate balance score by calculating the harmonic mean of the p-values from individual covariate balance tests.
#' This does not require the assumptions that the covariates are independent.
#'
#' @param p_values A numeric vector containing p-values from individual covariate balance tests.
#'   All values must be between 0 and 1.
#' @param na.rm Logical; if TRUE (default), NA values are removed before calculation.
#'   If FALSE and NA values are present, the function will return NA.
#'
#' @return A numeric value representing the overall balance score.
#'   Higher values indicate better covariate balance.
#'
#'
#' @examples
#' # Example with well-balanced covariates
#' p_vals <- c(0.8, 0.9, 0.85)
#' calculate_balance_score(p_vals)
#'
#' # Example with poorly balanced covariates
#' p_vals <- c(0.01, 0.02, 0.03)
#' calculate_balance_score(p_vals)
#'
#' # Handling NA values
#' p_vals_with_na <- c(0.8, NA, 0.85)
#' calculate_balance_score(p_vals_with_na, na.rm = TRUE)
#'
#' @export

calculate_balance_score = function(p_values, na.rm = TRUE){
  # remove NA values if present
  if(na.rm){
    p_values = p_values[!is.na(p_values)]
  }
  else if(any(is.na(p_values))){
    return(NA)
  }

  # harmonic mean
  harmonic_mean = 1 / mean(1/p_values)

  return(harmonic_mean)
}

## ---------------------------------------------------------------------------------------------------------------------------------
allocate_best_random <- function(data, batch_size, iterations, blocking_variable = NA) {

  # Allocate the number of layouts specified by "iterations"
  many_layouts <- replicate(iterations,
                            allocate_single_random(data,
                                                   batch_size = batch_size,
                                                   blocking_variable = blocking_variable),
                            simplify = FALSE)

  # calculate balance score for each layout
  balance_scores <- sapply(many_layouts, function(x) calculate_balance_score(x$results$p_value))

  # Return the layout with the highest balance score
  index_of_best = which.max(balance_scores)
  best_layout <- many_layouts[[index_of_best]]$layout %>%
    mutate(batch_allocation = as.factor(batch_allocation))

  best_layout_result <- many_layouts[[index_of_best]]$results

  cat("Balance Score:", calculate_balance_score(best_layout_result$p_value), "\n")

  # Return the layout
  return(list(layout = best_layout,
              results = best_layout_result))
}

## ---------------------------------------------------------------------------------------------------------------------------------
# Function for simulated annealing allocation
simulate_annealing <- function(data,
                                covariates,
                                blocking_variable = NA,
                                batch_size,
                                temperature,
                                cooling_rate,
                                iterations,
                                plot = TRUE) {
  # Step 1: Define the objective function to minimize the sum of p-values
  objective <- function(layout_inc_data, blocking_variable = blocking_variable) {
    metrics <- test_covariates(layout_inc_data, blocking_variable = blocking_variable)
    balance_score <- calculate_balance_score(metrics$p_value)
    return(balance_score)
  }

  # Step 2: simulate annealing
  # inital conditions
  if(is.na(blocking_variable)){
    current_arrangement <- allocate_single_random(data, batch_size)$layout
  }
  else{
    current_arrangement <- allocate_single_random(data, batch_size, blocking_variable = blocking_variable)$layout

    # block_size = table(current_arrangement[[blocking_variable]])[[1]]
    #
    # # count the number of padding samples in each block
    # n_padding_samples <- current_arrangement %>%
    #   filter(grepl("padding", sample_id)) %>%
    #   group_by(batch_allocation) %>%
    #   summarise(n = n()) %>%
    #   mutate(m = n %/% block_size)
    #
    # # For all batches with padding samples, assign padding samples to a level of "blocking_varaible" if possible
    # for (batch in n_padding_samples$batch_allocation[n_padding_samples$m > 0]) {
    #   # Get padding samples for this batch
    #   padding_samples <- current_arrangement %>%
    #     filter(batch_allocation == batch,
    #            grepl("padding", sample_id))
    #
    #   # Number of complete blocks we can make
    #   n_blocks <- nrow(padding_samples) %/% block_size
    #
    #   # Randomly assign block numbers to these samples
    #   if (n_blocks > 0) {
    #     # Create vector of block assignments
    #     block_assignments <- paste0("empty_block_", batch, "_", rep(1:n_blocks, each = block_size))
    #
    #     # If there are any remaining samples, they keep their original block_id
    #     if (nrow(padding_samples) > length(block_assignments)) {
    #       block_assignments <- c(block_assignments,
    #                              rep(NA, nrow(padding_samples) - length(block_assignments)))
    #     }
    #
    #     # Randomly assign the blocks
    #     block_assignments <- sample(block_assignments)
    #     # Replace NA with original block_id
    #     padding_indices <- which(grepl("padding", current_arrangement$sample_id) &
    #                                current_arrangement$batch_allocation == batch)
    #
    #     # Add the new levels to the factor before assignment
    #     current_arrangement[[blocking_variable]] <- factor(current_arrangement[[blocking_variable]],
    #                                            levels = union(levels(current_arrangement[[blocking_variable]]),
    #                                                           unique(block_assignments[!is.na(block_assignments)])))
    #
    #     # assign
    #     current_arrangement[[blocking_variable]][padding_indices] <- block_assignments
    #   }
    # }

  }

  batch_n_needed <- length(levels(current_arrangement$batch_allocation))
  initial_value <- objective(current_arrangement, blocking_variable = blocking_variable)

  current_value <- initial_value
  # define a vectors to store values over time
  optimisation_data <- data.frame(iteration = numeric(iterations),
                                  temperature = numeric(iterations),
                                    objective_value = numeric(iterations))

  # run algorithm
  for (iteration in 1:iterations) {
    # store current values, to show progress over time
    optimisation_data$iteration[iteration] <- iteration
    optimisation_data$temperature[iteration] <- temperature
    optimisation_data$objective_value[iteration] <- current_value

    # Generate a neighboring arrangement by swapping two random samples between blocks
    ## select batch_a at random
    batch_a <- sample(1:batch_n_needed, 1)

    ## select batch_b at random, from all batches except batch_a
    batch_b <- sample(setdiff(1:batch_n_needed, batch_a), 1)

    if (missing(blocking_variable) || is.na(blocking_variable) || blocking_variable == "") {
      # swap two samples between batch_a and batch_b
      ## choose samples at random to swap
      batch_a_samples <- which(current_arrangement$batch_allocation == batch_a)
      batch_a_chosen_sample = sample(batch_a_samples, 1)
      batch_b_samples <- which(current_arrangement$batch_allocation == batch_b)
      batch_b_chosen_sample = sample(batch_b_samples, 1)

      ## swap the samples
      neighbor_arrangement <- current_arrangement
      neighbor_arrangement$batch_allocation[batch_a_chosen_sample] <- batch_b
      neighbor_arrangement$batch_allocation[batch_b_chosen_sample] <- batch_a

    } else {
      # if samples are blocked, swap two blocks between batch_a and batch_b
      ## Choose a block at random from batch_a and batch_b
      batch_a_samples <- which(current_arrangement$batch_allocation == batch_a &
                                 !is.na(current_arrangement[[blocking_variable]]))
      batch_a_block_id <- current_arrangement[[blocking_variable]][sample(batch_a_samples, 1)]
      batch_b_samples <- which(current_arrangement$batch_allocation == batch_b &
                                 !is.na(current_arrangement[[blocking_variable]]))
      batch_b_block_id <- current_arrangement[[blocking_variable]][sample(batch_b_samples, 1)]

      ## Find all samples with the same block_id as the chosen samples in both batches
      batch_a_block_samples <- which(current_arrangement[[blocking_variable]] == batch_a_block_id)
      batch_b_block_samples <- which(current_arrangement[[blocking_variable]] == batch_b_block_id)

      ## Create a copy of the current arrangement to modify
      neighbor_arrangement <- current_arrangement
      neighbor_arrangement$batch_allocation[batch_a_block_samples] <- batch_b
      neighbor_arrangement$batch_allocation[batch_b_block_samples] <- batch_a

    }

    # Calculate the neighbor's value
    neighbor_value <- objective(neighbor_arrangement, blocking_variable = blocking_variable)

    # Calculate the difference in values
    delta_value <- neighbor_value - current_value

    # Decide whether to accept the neighbor
    if (delta_value > 0 || runif(1) < exp(delta_value / temperature)) {
      current_arrangement <- neighbor_arrangement
      current_value <- neighbor_value
    }

    # Reduce the temperature
    temperature <- temperature * cooling_rate
  }

  # plot optimisation, if indicated
  if (plot == TRUE) {
    # plot  objective_value vs iteration, with temperature on a second y axis
    scaleFactor <- max(optimisation_data$objective_value) / max(optimisation_data$temperature)

    optimisation_plot = optimisation_data %>%
      dplyr::mutate(Temperature = temperature * scaleFactor) %>%
      dplyr::rename(balance_score = objective_value) %>%
      tidyr::gather(key = "Variable", value = "Value", balance_score, Temperature) %>%
      ggplot2::ggplot(ggplot2::aes(x = iteration, y = Value, color = Variable)) +
        ggplot2::geom_line() +
        ggplot2::ggtitle("Optimisation data") +
        ggplot2::scale_y_continuous(name = "Balance Score",
                           sec.axis = sec_axis(~./scaleFactor, name = "Temperature"))

    print(optimisation_plot)
  }

    # select final layout
  optimal_layout <- current_arrangement
  optimal_value <- current_value

  cat("Balance Score of final layout:", optimal_value, "\n")

  # test covaraite balance of final layout
  optimal_layout_result = test_covariates(optimal_layout, blocking_variable = blocking_variable)

  # Return the layout
  return(list(layout = optimal_layout,
              results = optimal_layout_result,
              optimisation_data = optimisation_data))

}

## ---------------------------------------------------------------------------------------------------------------------------------
#' Allocate Samples to Batches Using Specified Method
#'
#' This function allocates samples to batches based on the specified method, which can be random allocation, best random allocation, or simulated annealing. It supports optional blocking and handles various types of covariates.
#'
#' @param data A `data.frame` containing the dataset to be processed.
#' @param id_column A string specifying the column name in `data` that contains the sample IDs. The default is "sample_id".
#' @param method A string specifying the allocation method to be used. Valid options are "random", "best_random", and "simulated_annealing". The default is "simulated_annealing".
#' @param covariates A character vector listing the names of the covariate columns in `data` that should be considered during allocation.
#' @param blocking_variable An optional string specifying the name of the column to be used as a blocking variable. If not provided or `NA`, blocking is not applied.
#' @param batch_size An integer specifying the size of each batch.
#' @param iterations An integer specifying the number of iterations to run for the "best_random" or "simulated_annealing" methods. The default is 1000.
#' @param temperature A numeric specifying the initial temperature for the simulated annealing method. The default is 1.
#' @param cooling_rate A numeric specifying the cooling rate for the simulated annealing method. The default is 0.975.
#' @param seed An integer used for setting the seed to ensure reproducibility. The default is 123.
#' @param plot_convergence = TRUE A logical indicating whether to plot the convergence of the optimisation process, only relevant if the method specified is "simulated_annealing". The default is `TRUE`.
#' @param collapse_rare_factors = FALSE A logical indicating whether to collapse rare factor levels in the covariates. The default is `FALSE`. Factor levels that are present with counts either less than 5 or in 0.05*batch_size are collapsed into a single level "other".
#'  Please note that the layout returned will contain these collapsed factor levels, rather than your original input data.
#'
#' @return An object containing the allocation layout of samples to batches, along with any specified blocking and covariate adjustments. The exact structure of the return value depends on the allocation method used.
#'
#' @details
#' The function first checks the validity of the specified method and parameters, then preprocesses the data according to the specified covariates and blocking variable. It applies the specified allocation method to assign samples to batches, aiming to balance the distribution of covariates and, if applicable, blocking levels across batches.
#'
#' @examples
#' # Allocate samples using simulated annealing without blocking
#' my_data = simulate_data(n_samples = 100)
#'
#' allocated_data <- allocate_samples(data = my_data,
#'                                    id_column = "sample_id",
#'                                    method = "simulated_annealing",
#'                                    covariates = c("covariate1", "covariate2", "covariate3"),
#'                                    batch_size = 13)
#'
#'
#' @importFrom stats fisher.test kruskal.test setNames
#'
#' @export

allocate_samples <- function(data,
                             id_column = "sample_id",
                             method = "simulated_annealing",
                             covariates,
                             blocking_variable = NA,
                             batch_size,
                             iterations = 1000,
                             temperature = 1,
                             cooling_rate = 0.975,
                             seed = 123,
                             plot_convergence = TRUE,
                             collapse_rare_factors = FALSE) {

  # Valid input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.character(id_column) || length(id_column) != 1) {
    stop("id_column must be a single character string")
  }

  valid_methods <- c("random", "best_random", "simulated_annealing")
  if (!method %in% valid_methods) {
    stop(sprintf("Invalid method '%s'. Must be one of: %s",
                 method, paste(valid_methods, collapse = ", ")))
  }

  if (!is.numeric(batch_size) || batch_size <= 0 || floor(batch_size) != batch_size) {
    stop("batch_size must be a positive integer")
  }

  if (batch_size > nrow(data)) {
    stop("batch_size cannot be larger than the number of samples")
  }

  if (!is.logical(collapse_rare_factors)) {
    stop("collapse_rare_factors must be TRUE or FALSE")
  }

  # Validate simulated annealing parameters
  if (method == "simulated_annealing") {
    if (!is.numeric(temperature) || temperature <= 0) {
      stop("temperature must be a positive number")
    }
    if (!is.numeric(cooling_rate) || cooling_rate <= 0 || cooling_rate >= 1) {
      stop("cooling_rate must be between 0 and 1")
    }
    if (!is.numeric(iterations) || iterations <= 0 || floor(iterations) != iterations) {
      stop("iterations must be a positive integer")
    }
  }

  # function body
  # rename id_column
  names(data)[names(data) == id_column] <- "sample_id"

  # Check if covariates and are valid column names
  data_columns <- names(data)
  if (!all(covariates %in% data_columns)) {
    stop("One or more covariates are not valid column names in the data.")
  }

  # Check if blocking_variable is a factor
  if (missing(blocking_variable) || is.na(blocking_variable) || blocking_variable == "") {
    cat("No blocking variable specified. \n")
  } else {
    cat("Blocking variable: ", blocking_variable, "\n")
    if (!is.factor(data[[blocking_variable]])) {
      stop("Blocking variable is not a factor.")
    }
    if (!blocking_variable %in% data_columns) {
      stop("Blocking variable is not a valid column name in the data.")
    }
    if (!id_column %in% data_columns) {
      stop("id_column is not a valid column name in the data.")
    }
      # Check if the maximum number of rows per level of blocking_variable is less than half the batch_size
   if (max(table(data[[blocking_variable]])) >= (batch_size / 2)) {
    stop("The maximum number of rows for a single level of the blocking variable exceeds half the batch size - there is little flexibility to create bias free layouts.")
  }
  }

  # convert any logical covaritates to factors
  for (covariate in names(data)) {
    if (is.logical(data[[covariate]])) {
      data[[covariate]] <- factor(data[[covariate]])
    }
  }

  # collapse rare factor levels
  if (collapse_rare_factors) {
    min_count <- max(5, ceiling(batch_size * 0.05))

    for (col in covariates) {
      if (is.factor(data[[col]])) {
        # Calculate counts of each level
        level_counts <- table(data[[col]])

        # Identify rare levels (count < min_count)
        rare_levels <- names(level_counts)[level_counts < min_count]

        if (length(rare_levels) > 0) {
          # Create a new factor with rare levels collapsed
          data[[col]] <- droplevels(factor(ifelse(data[[col]] %in% rare_levels,
                                                  "other",
                                                  as.character(data[[col]]))))

          # Print information about collapsed levels
          message(sprintf("Collapsed %d rare levels in %s (counts < %d): %s\nCounts were: %s",
                          length(rare_levels),
                          col,
                          min_count,
                          paste(rare_levels, collapse = ", "),
                          paste(paste(rare_levels, level_counts[rare_levels], sep=": "),
                                collapse=", ")))

        }
      }
    }
  }

  # subset data to only include covariates and blocking variable
  relevant_columns <- if (missing(blocking_variable) || is.na(blocking_variable) || blocking_variable == "") {
    c(covariates, "sample_id")
  } else {
    c(covariates, blocking_variable, "sample_id")
  }

  original_data <- data
  data <- data[, relevant_columns]

  # print a summary of the provided data, to facilitate the user checking they have specified everything as expected
  for (covariate in covariates) {
    covariate_class <- class(data[[covariate]])

    if (covariate_class == "numeric" || covariate_class == "integer") {
      cat("Covariate:", covariate, "- continuous\n")
    } else if (covariate_class == "factor") {
      cat("Covariate:", covariate, "- categorical\n")
    }
  }
  # remove any completely empty rows
  # data <- data[complete.cases(data), ]
  cat("Number of samples:", nrow(data), "\n")

  # run specified method
  set.seed(seed)
  if (method == "random") {
    output = allocate_single_random(data = data, batch_size = batch_size, blocking_variable = blocking_variable)
  } else if (method == "best_random") {
    output = allocate_best_random(data = data, batch_size = batch_size, blocking_variable = blocking_variable, iterations = iterations)
  } else if (method == "simulated_annealing") {
    output = simulate_annealing(data = data, batch_size = batch_size, blocking_variable = blocking_variable, temperature = temperature, cooling_rate = cooling_rate, iterations = iterations, plot = plot_convergence)
  } else {
    stop("Invalid method specified")
  }
  # join the output with the original data
  output$layout = output$layout %>%
    dplyr::left_join(original_data) %>%
    dplyr::rename_with(~ id_column, .cols = "sample_id")

  return(output)
}



## ---------------------------------------------------------------------------------------------------------------------------------
#' Plot the Layout of Samples by Covariate Type
#'
#' This function generates two types of plots for the given layout: one for continuous covariates and another for categorical covariates. It visualizes how samples are allocated across batches, with separate plots for each covariate.
#'
#' @param output A lsit created by `allocate_samples()`, containing an item called `layout` which is  `data.frame` containing the layout information, including sample IDs, batch allocation, and covariates. The `batch_allocation` column is expected to indicate the batch each sample is assigned to, and covariates can be of numeric or factor type.
#' @param id_column The name of the column in the layout data frame that contains the sample IDs. The default value is "sample_id".
#' @param covariates A character vector of covariate names to be visualized. The default value is `NULL`, which will visualize all covariates in the layout data frame.
#'
#' @return The function does not return an object; instead, it directly prints two plots to the R graphics device. The first plot visualizes the distribution of continuous covariates across batches, using a scatter plot for each covariate. The second plot shows the distribution of categorical covariates using a bar plot for each covariate level across batches.
#'
#' @details
#' The function first processes the layout to separate continuous and categorical covariates. For continuous covariates, it creates a scatter plot showing the value of each covariate by batch. For categorical covariates, it aggregates the data to count the number of samples in each category by batch and then creates a bar plot. Both plots are faceted by covariate to provide a clear and comparative view of the distribution across batches.
#'
#' @examples
#' my_data = simulate_data(n_samples = 100)
#'
#' allocated_data <- allocate_samples(data = my_data,
#'                                    id_column = "sample_id",
#'                                    method = "simulated_annealing",
#'                                    covariates = c("covariate1", "covariate2", "covariate3"),
#'                                    batch_size = 13)
#'
#' plot_layout(allocated_data, covariates = c("covariate1", "covariate2", "covariate3"))
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

plot_layout <- function(output, id_column = "sample_id", covariates) {
  # validate input
  if (!is.list(output) || !("layout" %in% names(output))) {
    stop("output must be a list containing a 'layout' element")
  }

  if (!is.data.frame(output$layout)) {
    stop("output$layout must be a data frame")
  }

  if (!is.character(id_column) || length(id_column) != 1) {
    stop("id_column must be a single character string")
  }

  if (!is.null(covariates) && !all(covariates %in% names(output$layout))) {
    missing_covs <- setdiff(covariates, names(output$layout))
    stop(sprintf("Covariates not found in layout: %s",
                 paste(missing_covs, collapse = ", ")))
  }

  # function body
  layout = output$layout

  if (missing(covariates) || any(is.na(covariates)) || any(covariates == "")) {
    covariates = names(layout)[!names(layout) %in% c(id_column, "batch_allocation")]
  }
  else {
    covariates = covariates
  }

  layout = layout %>%
    dplyr::select({{id_column}}, batch_allocation, all_of(covariates))

  # continuous covariates
  continuous_plot = layout %>%
    dplyr::select(where(is.numeric) | {{id_column}}, batch_allocation) %>%
    tidyr::pivot_longer(cols = !c({{id_column}}, batch_allocation), names_to = "covariate", values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = batch_allocation, y = value)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ covariate)

  print(continuous_plot)

# categorical covariates
  categorical_plot = layout %>%
    dplyr::select(where(is.factor) | {{id_column}}, batch_allocation) %>%
    tidyr::pivot_longer(cols = !c(id_column, batch_allocation), names_to = "covariate", values_to = "value") %>%
    droplevels() %>%
    dplyr::group_by(covariate, value, batch_allocation) %>%
    dplyr::summarise(n = n()) %>%
    ggplot2::ggplot(aes(x = batch_allocation, y = n, fill = value)) +
      geom_col()  +
      facet_wrap(~ covariate)

  print(categorical_plot)
}

## ---------------------------------------------------------------------------------------------------------------------------------
#' Check for Significant Covariates in Output
#'
#' This function evaluates the significance of covariates based on adjusted p-values. It applies the Bonferroni correction to the p-values associated with each covariate and identifies those that are statistically significant.
#' We *do not* recommend using this function to conduct hypothesis testing, but instead only as a sanity check to identify which covariates may potentially be problematic.
#'
#' @param output A list or similar object expected to contain a `results` data frame. The `results` data frame should have at least two columns: `covariate` for the name of the covariate and `p_value` for the associated p-value of some statistical test.
#'
#' @return A data frame listing the covariates that have been found to be statistically significant after adjusting their p-values for multiple testing. If no covariates are found to be significant, the function will return an empty data frame. The return data frame includes columns for the covariate name and the count of significant instances, although the count will typically be one for each listed covariate unless the input `output` structure allows for duplicate covariate names.
#'
#' @details
#' The function first retrieves the `results` data frame from the provided `output` object. It then calculates adjusted p-values using the Bonferroni correction method. Covariates with adjusted p-values less than 0.05 are considered statistically significant. The function outputs the names of significant covariates and their counts.
#'
#' @examples
#' my_data = simulate_data(n_samples = 100)
#'
#' allocated_data <- allocate_samples(data = my_data,
#'                                    id_column = "sample_id",
#'                                    method = "simulated_annealing",
#'                                    covariates = c("covariate1", "covariate2", "covariate3"),
#'                                    batch_size = 13)
#'
#' significant_covariates <- check_significance(allocated_data)
#'
#' @import dplyr
#' @importFrom stats p.adjust
#' @export
check_significance <- function(output) {
  # validate input
  if (!is.list(output) || !("results" %in% names(output))) {
    stop("Output must be a list containing a 'results' element, for example as generated by the allocate_samples() function.")
  }

  # function body
  ## count pvalues < 0.05
  result_table <- output$results
  n_covariates = nrow(result_table)

  # validate input cols
  required_cols <- c("covariate", "p_value")
  missing_cols <- setdiff(required_cols, names(result_table))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in results: ",
         paste(missing_cols, collapse = ", "))
  }

  n_significant_covariates = result_table %>%
    dplyr::mutate(adj_p_value = p.adjust(p_value, method = "bonferroni")) %>%
    dplyr::filter(adj_p_value < 0.05) %>%
    dplyr::count(covariate)

  if (nrow(n_significant_covariates) == 0) {
    cat("No significant covariates\n")
  } else {
    cat("Significant covariates:\n")
    print(n_significant_covariates)
  }

  return(n_significant_covariates)
}


