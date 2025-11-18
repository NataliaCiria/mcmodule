# Example Data Keys for Animal Imports Risk Assessment

A hierarchical data structure containing test sensitivity, animal
import, and regional prevalence information, each with defined columns
and keys.

## Usage

``` r
imports_data_keys
```

## Format

A list with three components:

- test_sensitivity:

  List containing column names for test sensitivity data and "pathogen"
  as key

- animal_imports:

  List containing column names for animal import data and "origin" as
  key

- prevalence_region:

  List containing column names for prevalence data with "pathogen" and
  "origin" as keys

## Source

Simulated data for demonstration purposes
