# Animal-imports example data
#
# This script creates the source datasets, merged model data, data keys,
# Monte Carlo node specifications, and model expression used in the
# illustrative mcmodule animal-import example.
#
# The example estimates the probability that an imported animal is infected
# but is not detected before movement. It includes two hypothetical pathogens
# and three regions of origin.
#
# All values are simulated for teaching and software demonstration. They do
# not represent surveillance data, disease estimates, or movement patterns
# from a real population.

# Data

# Numbers of exporting farms and animals exported from each region.
animal_imports <- data.frame(
  origin = c("nord", "south", "east"),
  farms_n = c(5, 10, 7),
  animals_n_mean = c(100, 130, 140),
  animals_n_sd = c(6, 10, 12),
  stringsAsFactors = FALSE
)

# Herd prevalence, within-herd prevalence, and testing practices by
# hypothetical pathogen and region of origin.
prevalence_region <- data.frame(
  pathogen = c("a", "a", "a", "b", "b", "b"),
  origin = c("nord", "south", "east", "nord", "south", "east"),
  h_prev_min = c(0.08, 0.02, 0.10, 0.50, 0.25, 0.30),
  h_prev_max = c(0.10, 0.05, 0.15, 0.70, 0.30, 0.50),
  w_prev_min = c(0.15, 0.15, 0.15, 0.45, 0.37, 0.45),
  w_prev_max = c(0.20, 0.20, 0.20, 0.60, 0.40, 0.60),
  test_origin = c(
    "sometimes",
    "sometimes",
    "never",
    "always",
    "sometimes",
    "unknown"
  ),
  stringsAsFactors = FALSE
)

# Diagnostic-test sensitivity by hypothetical pathogen.
test_sensitivity <- data.frame(
  pathogen = c("a", "b"),
  test_sensi_min = c(0.89, 0.80),
  test_sensi_mode = c(0.90, 0.85),
  test_sensi_max = c(0.91, 0.90),
  stringsAsFactors = FALSE
)

# Merged model data
imports_data <- prevalence_region |>
  dplyr::left_join(animal_imports, by = "origin") |>
  dplyr::left_join(test_sensitivity, by = "pathogen") |>
  dplyr::relocate(pathogen, origin, test_origin)

# Data keys
#
# The keys describe the level at which each source dataset provides
# information. The merged model contains one variate for every combination
# of pathogen and region of origin.
imports_data_keys <- list(
  animal_imports = list(
    cols = names(animal_imports),
    keys = "origin"
  ),
  prevalence_region = list(
    cols = names(prevalence_region),
    keys = c("pathogen", "origin")
  ),
  test_sensitivity = list(
    cols = names(test_sensitivity),
    keys = "pathogen"
  )
)


# Monte Carlo node specifications
#
# h_prev and w_prev use uniform distributions. test_sensi uses a PERT
# distribution, and animals_n uses a normal distribution. farms_n is
# deterministic. The two test_origin nodes demonstrate how categorical
# source data can be transformed into numeric model inputs.
#
# sample_space defines illustrative ranges for sample-design sensitivity
# analyses. sensi_variation defines changes for one-at-a-time analyses.
imports_mctable <- data.frame(
  mcnode = c(
    "h_prev",
    "w_prev",
    "test_sensi",
    "farms_n",
    "animals_n",
    "test_origin_unk",
    "test_origin"
  ),
  description = c(
    "Herd prevalence",
    "Within herd prevalence",
    "Test sensitivity",
    "Number of farms exporting animals",
    "Number of animals exported per farm",
    paste(
      "Unknown probability of the animals being tested in origin",
      "(true = unknown)"
    ),
    "Probability of the animals being tested in origin"
  ),
  mc_func = c(
    "runif",
    "runif",
    "rpert",
    NA,
    "rnorm",
    NA,
    NA
  ),
  from_variable = c(
    NA,
    NA,
    NA,
    NA,
    NA,
    "test_origin",
    NA
  ),
  transformation = c(
    NA,
    NA,
    NA,
    NA,
    NA,
    "value == 'unknown'",
    paste0(
      "ifelse(value == 'always', 1, ",
      "ifelse(value == 'sometimes', 0.5, ",
      "ifelse(value == 'never', 0, NA)))"
    )
  ),
  sensi_variation = c(
    "pmin(1, pmax(0, value * 1.5))",
    "pmin(1, pmax(0, value * 1.5))",
    "pmin(1, pmax(0, value * 1.5))",
    "value * 1.5",
    "value * 1.5",
    "ifelse(value == 'unknown', 'always', value)",
    "pmin(1, pmax(0, value * 1.5))"
  ),
  sample_space = c(
    "min = 0.02, max = 0.7",
    "min = 0.15, max = 0.6",
    "min = 0.8, mode = 0.875, max = 0.91",
    "min = 5, max = 10",
    "min = 82, max = 176",
    NA,
    "min = 0, max = 1"
  ),
  stringsAsFactors = FALSE
)


# Model expression
#
# The expression calculates the probability that an animal from an infected
# herd is infected and is not detected before import. Test specificity is
# assumed to be 100%.
imports_exp <- quote({
  # Probability that an animal from an infected herd is infected
  infected <- w_prev

  # Probability that an infected animal is tested but returns a false negative
  false_neg <- infected * test_origin * (1 - test_sensi)

  # Probability that an infected animal is not tested
  no_test <- infected * (1 - test_origin)

  # Overall probability that an infected animal is not detected
  no_detect <- false_neg + no_test
})

# Save package data
usethis::use_data(animal_imports, overwrite = TRUE)
usethis::use_data(prevalence_region, overwrite = TRUE)
usethis::use_data(test_sensitivity, overwrite = TRUE)
usethis::use_data(imports_data, overwrite = TRUE)
usethis::use_data(imports_data_keys, overwrite = TRUE)
usethis::use_data(imports_mctable, overwrite = TRUE)
usethis::use_data(imports_exp, overwrite = TRUE)
