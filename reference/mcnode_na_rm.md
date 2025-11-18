# Replace NA and Infinite Values in mcnode Objects

Replaces NA and infinite values in mcnode objects with a specified
value.

## Usage

``` r
mcnode_na_rm(mcnode, na_value = 0)
```

## Arguments

- mcnode:

  An mcnode object containing NA or infinite values

- na_value:

  Numeric value to replace NA and infinite values (default = 0)

## Value

An mcnode object with NA and infinite values replaced by na_value

## See also

[`is.na.mcnode`](https://rdrr.io/pkg/mc2d/man/NA.mcnode.html)

## Examples

``` r
sample_mcnode <- mcstoc(runif,
               min = mcdata(c(NA, 0.2, -Inf), type = "0", nvariates = 3),
               max = mcdata(c(NA, 0.3, Inf), type = "0", nvariates = 3),
               nvariates = 3
)
#> Warning: NAs produced
#> Warning: NAs produced
# Replace NA and Inf with 0
clean_mcnode <- mcnode_na_rm(sample_mcnode)
```
