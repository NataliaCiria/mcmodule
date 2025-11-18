# Example Monte Carlo Input Table for Import Risk Assessment

A configured table of Monte Carlo nodes used for modeling import risk
scenarios, particularly focused on animal disease transmission pathways.

## Usage

``` r
imports_mctable
```

## Format

### imports_mctable

A data frame with 7 rows and 6 columns:

- mcnode:

  Node identifier used in Monte Carlo simulations

- description:

  Human-readable description of what the node represents

- mc_func:

  R function used for random number generation (e.g., runif, rnorm,
  rpert)

- from_variable:

  Dependency reference to other variables if applicable

- transformation:

  Mathematical transformations applied to the node values

- sensi_analysis:

  Logical flag indicating if node is included in sensitivity analysis

## Source

Simulated data for demonstration purposes
