# Example mcmodule object containing Monte Carlo simulation results Animal Imports Risk Assessment

A list containing simulation results for pathogen testing of animal
imports from different origins, including:

- Within-herd prevalence (w_prev)

- Test sensitivity (test_sensi)

- Test origin probability (test_origin)

- Infection probability (inf_a)

- False negative probability (false_neg_a)

- No test probability (no_test_a)

- Non-detection probability (no_detect_a)

## Usage

``` r
imports_mcmodule
```

## Format

An mcmodule object with the following components:

- data:

  Input data frame with 6 rows and 13 variables

- exp:

  Model expressions for calculating probabilities

- node_list:

  List of Monte Carlo nodes with simulation results

- modules:

  Character vector of module names

## Source

Simulated data for demonstration purposes
