# Calculate Effect Sizes Between Covariates and Batch Allocation

Calculate appropriate effect size measures for associations between
covariates and batch allocation. Uses Cramér's V for
categorical-categorical, eta (from ANOVA) for continuous-categorical
combinations.

## Usage

``` r
calculate_effect_sizes(layout, blocking_variable = "block_id")
```

## Arguments

- layout:

  Data frame with batch_allocation column

- blocking_variable:

  Optional blocking variable to exclude from calculations

## Value

Data frame with columns: covariate, effect_size

## Details

Effect sizes are scaled to be comparable to correlations (0-1 range):

- Categorical covariates: Uses Cramér's V with corrected formula

- Continuous covariates: Uses eta (square root of eta-squared from
  ANOVA)

Lower effect sizes indicate better balance (weaker association between
covariate and batch assignment).
