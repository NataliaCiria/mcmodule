# Match Datasets With Differing Scenarios

Matches datasets by group and preserves baseline scenarios
(scenario_id=0) when scenarios differ between them.

## Usage

``` r
wif_match(x, y, by = NULL)
```

## Arguments

- x:

  First dataset to match

- y:

  Second dataset to match

- by:

  Grouping variable(s) to match on, defaults to NULL

## Value

List containing matched datasets with aligned scenario IDs:

- First element: matched version of dataset x

- Second element: matched version of dataset y

## Examples

``` r
x <- data.frame(
  category = c("a", "b", "a", "b"),
  scenario_id = c(0, 0, 1, 1),
  value = 1:4
)

y <- data.frame(
  category = c("a", "b", "a", "b"),
  scenario_id = c(0, 0, 2, 2),
  value = 5:8
)

# Automatic matching
result <- wif_match(x, y)
#> Group by: category
#> From 4 rows (2 groups, 2 scenarios) and 4 rows (2 groups, 2 scenarios), to 6 rows (2 groups, 3 scenarios)
```
