# Merged Import Data for Risk Assessment

A dataset combining information about animal imports, pathogen
prevalence, and test sensitivity across regions

## Usage

``` r
imports_data
```

## Format

### imports_data

A data frame with 6 rows and 12 columns:

- pathogen:

  Pathogen identifier (a or b)

- origin:

  Region of origin (nord, south, east)

- h_prev_min:

  Minimum herd prevalence value

- h_prev_max:

  Maximum herd prevalence value

- w_prev_min:

  Minimum within-herd prevalence value

- w_prev_max:

  Maximum within-herd prevalence value

- farms_n:

  Number of farms exporting animals

- animals_n_mean:

  Mean number of animals exported per farm

- animals_n_sd:

  Standard deviation of animals exported per farm

- test_origin:

  Test used to detect infected animals at origin

- test_sensi_min:

  Minimum test sensitivity value

- test_sensi_mode:

  Most likely test sensitivity value

- test_sensi_max:

  Maximum test sensitivity value

## Source

Simulated data for demonstration purposes
