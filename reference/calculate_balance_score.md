# Calculate Balance Score

This function calculates a covariate balance score by combining
individual covariate metrics using either harmonic mean or product.

## Usage

``` r
calculate_balance_score(
  p_values = NULL,
  effect_sizes = NULL,
  na.rm = TRUE,
  balance_metric = "harmonic_mean",
  balance_type = "p_value"
)
```

## Arguments

- p_values:

  A numeric vector containing p-values from individual covariate balance
  tests. Required when `balance_type = "p_value"`. All values must be
  between 0 and 1.

- effect_sizes:

  A numeric vector containing effect sizes (Cramér's V or eta). Required
  when `balance_type = "effect_size"`. All values must be between 0 and
  1.

- na.rm:

  Logical; if TRUE (default), NA values are removed before calculation.
  If FALSE and NA values are present, the function will return NA.

- balance_metric:

  A character string specifying how to combine multiple metrics. Valid
  options are:

  - `"harmonic_mean"` (default): Non-compensatory combination. Ensures
    all covariates must be balanced (no covariate can compensate for
    poor balance in another).

  - `"product"`: Very sensitive to any poorly balanced covariate.

- balance_type:

  A character string specifying what metric to calculate. Valid options
  are:

  - `"p_value"` (default): Uses p-values from statistical tests. Tests
    whether covariate distributions differ significantly between
    batches.

  - `"effect_size"`: Uses effect sizes measuring magnitude of
    associations. Similar to correlation-based approaches but using
    valid statistical measures.

## Value

A numeric value representing the overall balance score. For all
combinations, HIGHER values indicate better covariate balance.

## Details

The function transforms effect sizes as (1 - effect_size) so that lower
effect sizes (better balance) produce higher scores, matching the
direction of p-values. This ensures all metrics can be maximised by the
optimisation algorithm.

## Examples

``` r
# Statistical significance with harmonic mean (default)
p_vals <- c(0.8, 0.9, 0.85)
calculate_balance_score(p_values = p_vals)
#> [1] 0.848037

# Effect size with harmonic mean
effects <- c(0.05, 0.03, 0.04)  # Small effect sizes = good balance
calculate_balance_score(effect_sizes = effects, balance_type = "effect_size")
#> [1] 0.9599306

# Statistical significance with product
calculate_balance_score(p_values = p_vals, balance_metric = "product")
#> [1] 0.612

# Handling NA values
p_vals_with_na <- c(0.8, NA, 0.85)
calculate_balance_score(p_values = p_vals_with_na, na.rm = TRUE)
#> [1] 0.8242424
```
