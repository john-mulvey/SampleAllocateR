# Allocate Samples to Batches with Random Allocation (Internal Function)

This function randomly allocates samples to batches, with support for
blocking variables including unequal block sizes (using a bin-packing
algorithm).

## Usage

``` r
allocate_single_random(data, batch_size, blocking_variable = NA)
```

## Arguments

- data:

  A data.frame containing the dataset to be allocated. Must include a
  sample_id column.

- batch_size:

  Integer specifying the size of each batch.

- blocking_variable:

  Optional string specifying the blocking variable column name. If NA or
  missing, no blocking is applied.

## Value

A list with two elements:

- layout: data.frame with batch_allocation column added (may include
  padding rows)

- results: data.frame with covariate balance test results

## Details

Handles three scenarios:

**Case 1: No blocking variable** Samples are randomly allocated to
batches without constraints.

**Case 2a: Blocking variable with equal block size** Whole blocks are
randomly assigned to batches, keeping samples within blocks together.

**Case 2b: Blocking variable with unequal block size** Uses first-fit
bin-packing algorithm to assign blocks to batches while keeping blocks
intact. Blocks are randomized before packing. This siutation requires a
trade-off between bin packing efficiency and randomness in generating
the layout, and so in some cases can use more batches than the minimum
possible number.
