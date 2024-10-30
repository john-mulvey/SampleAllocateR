[![R-CMD-check.yaml](https://github.com/john-mulvey/SampleAllocateR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/john-mulvey/SampleAllocateR/actions/workflows/check-standard.yaml)

This is a pre-release version of the SampleAllocateR package: designed to optimally allocate samples with known covariates to experimetnal batches.

To install the latest version:
```r
# if required, install remotes package
install.packages("remotes")

# install package from github
remotes::install_github("https://github.com/john-mulvey/SampleAllocateR", build_vignettes = TRUE)
```
Note that building vignettes is currently very slow, but that it is not possible to supply them pre-built when installing the pacakge from github. If you would rather not wait, the vignette can be viewed [on the package website](https://john-mulvey.github.io/SampleAllocateR/)


# Quick Start Guide
In order to generate a layout with the optimal balance of covariates between experimental batches:
```r
optimal_layout_blocked = allocate_samples(toy_data, 
                                 batch_size = 13, 
                                 covariates = c("covariate1", "covariate2", "covariate3"),
                                 blocking_variable = "block_id",
                                 iterations = 1000,
                                 method = "simulated_annealing")
```

The resulting object contains 
- the original input data supplied the allocated batch appended
- results of testing the balance of each covariate between batches
- data for the optimisation process, if the method used was the default "simulated_annealing"

As a simple sanity check, the values of the covariates can be visualised using the `plot_layout` function:
```r
plot_layout(optimal_layout_blocked, covariates = c("covariate1", "covariate2", "covariate3"))
```
