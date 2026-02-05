# Simulated Annealing Allocation for Unequal Block Sizes (Internal Function)

This function approximates simulated annealing to extends the standard
simulated annealing approach to handle blocking variables with unequal
block sizes. The swapping routine enables swapping of up to 2 blocks
between batches, and can additionally move empty "padding" samples if
required. this helps to prevent the algorithm becoming stuck in local
optima.

## Usage

``` r
simulate_annealing_for_unequal_block_size(
  data,
  covariates,
  blocking_variable = NA,
  batch_size,
  temperature,
  cooling_rate,
  iterations,
  plot = TRUE,
  balance_metric = "harmonic_mean",
  balance_type = "p_value",
  initial_layout = NULL
)
```

## Arguments

- data:

  A data.frame containing the dataset to be allocated. Must include a
  sample_id column.

- covariates:

  A character vector listing the covariate column names to balance.

- blocking_variable:

  Optional string specifying the blocking variable column name. If NA or
  missing, no blocking is applied.

- batch_size:

  Integer specifying the size of each batch.

- temperature:

  Numeric specifying the initial temperature for the annealing process.

- cooling_rate:

  Numeric between 0 and 1 specifying the rate at which temperature
  decreases.

- iterations:

  Integer specifying the number of iterations to run.

- plot:

  Logical indicating whether to plot optimisation progress. Default is
  TRUE.

- balance_metric:

  Character string specifying the metric to use ("harmonic_mean" or
  "product").

- balance_type:

  A character string specifying what metric to calculate. Options are
  "p_value" (default) or "effect_size".

- initial_layout:

  Optional data.frame providing a pre-computed initial layout to use
  instead of generating a random one. Default is NULL.

## Value

A list with three elements:

- layout: data.frame with batch_allocation column added (may include
  padding rows)

- results: data.frame with covariate balance test results

- optimisation_data: data.frame tracking the optimisation process across
  iterations (iteration, temperature, objective_value). Diagnostic
  columns for swap mechanics are available in the code but commented out
  by default.

## Details

This function addresses the complex problem of optimizing batch
allocation when blocks have unequal sizes.

The algorithm considers four swap scenarios:

**Scenario 1:** Single block vs single block (with optional padding)

**Scenario 2:** Single block vs two blocks (with optional padding)

**Scenario 3:** Two blocks vs single block (with optional padding)

**Scenario 4:** Two blocks vs two blocks (with optional padding)

For each iteration, the algorithm:

1.  Identifies all possible swaps between two randomly selected batches

2.  Randomly selects one valid swap

3.  Evaluates the swap using the balance score

4.  Accepts or rejects based on simulated annealing criteria

Swaps are constrained to involve fewer than half the samples in a batch
to maintain diversity across batches.
