# Expression for calculating import infection probability

A quoted R expression that calculates the probability of importing an
infected animal from an infected herd, taking into account testing
procedures and accuracy.

## Usage

``` r
imports_exp
```

## Format

A quoted R expression containing the following variables:

- w_prev:

  Within-herd prevalence

- test_origin:

  Probability of testing at origin

- test_sensi:

  Test sensitivity

- inf_a:

  Probability of animal being infected

- false_neg_a:

  Probability of false negative test result

- no_test_a:

  Probability of no testing

- no_detect_a:

  Overall probability of non-detection
