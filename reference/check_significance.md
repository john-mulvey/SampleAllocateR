# Check for Significant Covariates in Output

This function evaluates the significance of covariates based on adjusted
p-values. It applies the Bonferroni correction to the p-values
associated with each covariate and identifies those that are
statistically significant. We *do not* recommend using this function to
conduct hypothesis testing, but instead only as a sanity check to
identify which covariates may potentially be problematic.

## Usage

``` r
check_significance(output)
```

## Arguments

- output:

  A list or similar object expected to contain a `results` data frame.
  The `results` data frame should have at least two columns: `covariate`
  for the name of the covariate and `p_value` for the associated p-value
  of some statistical test.

## Value

A data frame listing the covariates that have been found to be
statistically significant after adjusting their p-values for multiple
testing. If no covariates are found to be significant, the function will
return an empty data frame. The return data frame includes columns for
the covariate name and the count of significant instances, although the
count will typically be one for each listed covariate unless the input
`output` structure allows for duplicate covariate names.

## Details

The function first retrieves the `results` data frame from the provided
`output` object. It then calculates adjusted p-values using the
Bonferroni correction method. Covariates with adjusted p-values less
than 0.05 are considered statistically significant. The function outputs
the names of significant covariates and their counts.

## Examples

``` r
my_data = simulate_data(n_samples = 100)

allocated_data <- allocate_samples(data = my_data,
                                   id_column = "sample_id",
                                   method = "simulated_annealing",
                                   covariates = c("age_at_baseline", "bmi_at_baseline", "sex"),
                                   batch_size = 13)
#> No blocking variable specified. 
#> Covariate: age_at_baseline - continuous
#> Covariate: bmi_at_baseline - continuous
#> Covariate: sex - categorical
#> Number of samples: 100 

#> Balance Score of final layout: 1 
#> Joining with `by = join_by(age_at_baseline, bmi_at_baseline, sex, sample_id)`

significant_covariates <- check_significance(allocated_data)
#> No significant covariates
```
