# Changelog

## mcmodule 1.1.1

### New features

- [`eval_module()`](https://nataliaciria.github.io/mcmodule/reference/eval_module.md)
  gains `keys` and `overwrite_keys` arguments to add keys that aren’t in
  `data_keys` or replace existing keys
  ([\#23](https://github.com/NataliaCiria/mcmodule/issues/23)).

- Core functions
  ([`eval_module()`](https://nataliaciria.github.io/mcmodule/reference/eval_module.md),
  [`trial_totals()`](https://nataliaciria.github.io/mcmodule/reference/trial_totals.md),
  `dim_match()`,
  [`at_least_one()`](https://nataliaciria.github.io/mcmodule/reference/at_least_one.md),
  [`mc_match()`](https://nataliaciria.github.io/mcmodule/reference/mc_match.md),
  [`create_mcnodes()`](https://nataliaciria.github.io/mcmodule/reference/create_mcnodes.md),
  [`get_node_list()`](https://nataliaciria.github.io/mcmodule/reference/get_node_list.md))
  now support mcnodes with multiple data names, with clear messages
  indicating defaults
  ([\#19](https://github.com/NataliaCiria/mcmodule/issues/19)).

### Minor improvements and bug fixes

- [`keys_match()`](https://nataliaciria.github.io/mcmodule/reference/keys_match.md)
  now returns early when keys already match, improving performance and
  fixing ocasional bugs
  [\#28](https://github.com/NataliaCiria/mcmodule/issues/28).

- [`create_mcnodes()`](https://nataliaciria.github.io/mcmodule/reference/create_mcnodes.md)
  and
  [`eval_module()`](https://nataliaciria.github.io/mcmodule/reference/eval_module.md)
  provide clearer error messages for invalid or missing data
  ([\#18](https://github.com/NataliaCiria/mcmodule/issues/18)).

- [`mc_match()`](https://nataliaciria.github.io/mcmodule/reference/mc_match.md)
  and
  [`mc_match_data()`](https://nataliaciria.github.io/mcmodule/reference/mc_match_data.md)
  include improved scenario baseline checks and error messages.

## mcmodule 1.1.0

CRAN release: 2025-10-22

- **Re-submission to CRAN**: Removed unexported function examples.

- **Bug Fixes:** Fixed missing “agg_suffix” handling, improved dimension
  matching for aggregated nodes, resolved prefix issues in combined
  probability nodes.

- **Feature Enhancements:** Added “match_keys” parameter to
  [`eval_module()`](https://nataliaciria.github.io/mcmodule/reference/eval_module.md)
  for flexible data-mcnode matching, implemented multiple
  “prev_mcmodule” data names support in
  [`eval_module()`](https://nataliaciria.github.io/mcmodule/reference/eval_module.md),
  and improved R function handling within mcmodule expressions. Enhanced
  [`trial_totals()`](https://nataliaciria.github.io/mcmodule/reference/trial_totals.md)
  to support “agg_suffix” skipping, improved agg_keys combination with
  [`at_least_one()`](https://nataliaciria.github.io/mcmodule/reference/at_least_one.md),
  and added NA removal in key columns

- **Documentation:** Updated documentation and improved error messages
  for missing prev_nodes and mc_keys handling

- **Testing**: Added tests for totals custom names and various dimension
  matching scenarios

## mcmodule 1.0.1

- **Re-submission to CRAN**: Removed unexported function examples,
  replaced “dontrun” with “donttest”, added vignette link to
  DESCRIPTION, and included citation file.

## mcmodule 1.0.0

- Initial CRAN submission.
