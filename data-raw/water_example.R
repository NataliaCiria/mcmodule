# Water-supply example data
#
# This script creates the data, keys, Monte Carlo node specifications, and
# model expressions used in the illustrative mcmodule water-supply example.
#
# The example estimates the probability of Cryptosporidium infection through
# two pathways:
#
# 1. Oocysts remaining after routine water treatment.
# 2. Contamination introduced through an intrusion into the distribution
#    system.
#
# The parameter values are intended for teaching and software demonstration.
# They do not describe a particular water supply. Some values are broadly
# informed by published drinking-water QMRA guidance. The populations,
# intrusion probabilities, intrusion concentrations, and maintenance effect
# are hypothetical.
#
# References:
#
# World Health Organization (2016). Quantitative microbial risk assessment:
# application for water safety management.
# https://www.who.int/publications/i/item/9789241565370
#
# Health Canada. Guidance on the use of quantitative microbial risk assessment
# in drinking water.
# https://www.canada.ca/en/health-canada/services/environmental-workplace-health/reports-publications/water-quality/guidance-qmra-drinking-water.html
#
# Messner MJ, Chappell CL, and Okhuysen PC (2001). Risk assessment for
# Cryptosporidium: a hierarchical Bayesian analysis of human dose-response
# data. Water Research, 35:3934-3940.
# https://doi.org/10.1016/S0043-1354(01)00119-1
#
# Besner MC, Prevost M, and Regli S (2011). Assessing the public health risk
# of microbial intrusion events in distribution systems. Water Research,
# 45:961-979.
# https://doi.org/10.1016/j.watres.2010.10.035


# Data
#
# Each row represents one distribution zone under one management scenario.
# Scenario "0" is the baseline. The hypothetical network-maintenance scenario
# reduces the daily intrusion probability by 80%.
water_data <- data.frame(
  zone = c("Zone A", "Zone B", "Zone A", "Zone B"),
  scenario_id = c(
    "0", "0", "Network maintenance", "Network maintenance"
  ),
  population = c(500, 1500, 500, 1500),

  # Cryptosporidium concentration in source water (oocysts/L)
  source_conc_min = rep(0.5, 4),
  source_conc_mode = rep(1.0, 4),
  source_conc_max = rep(1.5, 4),

  # Treatment performance expressed as log10 reduction
  treatment_lrv_min = rep(2.5, 4),
  treatment_lrv_mode = rep(3.0, 4),
  treatment_lrv_max = rep(3.5, 4),

  # Daily consumption of unboiled tap water (L/person/day)
  water_volume = rep(1.5, 4),

  # Daily probability that an intrusion affects the distribution zone
  intrusion_prob = c(0.001, 0.005, 0.0002, 0.0010),

  # Concentration at the tap conditional on an intrusion (oocysts/L)
  intrusion_conc_min = rep(0.01, 4),
  intrusion_conc_max = rep(0.05, 4),

  # Exponential Cryptosporidium dose-response parameter
  dose_response_r = rep(0.018, 4),

  # Number of daily exposure events in the assessment period
  exposure_days = rep(30, 4),

  stringsAsFactors = FALSE
)


# Consumption-group data
#
# This expanded dataset illustrates multiple-group multilevel trials. Each
# zone-scenario row is divided into low-, medium-, and high-consumption groups.
# Group populations sum to the original zone population. The population shares
# and water-consumption values are hypothetical.
consumption_groups <- data.frame(
  consumption_group = c("Low", "Medium", "High"),
  population_share = c(0.25, 0.50, 0.25),
  water_volume = c(1.0, 1.5, 2.0),
  stringsAsFactors = FALSE
)

water_row <- rep(
  seq_len(nrow(water_data)),
  each = nrow(consumption_groups)
)

group_row <- rep(
  seq_len(nrow(consumption_groups)),
  times = nrow(water_data)
)

water_group_data <- water_data[water_row, , drop = FALSE]

water_group_data$consumption_group <-
  consumption_groups$consumption_group[group_row]

water_group_data$population <- as.integer(
  water_group_data$population *
    consumption_groups$population_share[group_row]
)

water_group_data$water_volume <-
  consumption_groups$water_volume[group_row]

# Place the consumption-group identifier beside the other variate keys
water_group_data <- water_group_data[
  c(
    "zone",
    "consumption_group",
    "scenario_id",
    setdiff(
      names(water_group_data),
      c("zone", "consumption_group", "scenario_id")
    )
  )
]

rownames(water_group_data) <- NULL


# Data keys

# zone and scenario_id jointly identify the four main model variates.
water_data_keys <- list(
  water_data = list(
    cols = names(water_data),
    keys = c("zone", "scenario_id")
  )
)

# zone, consumption_group, and scenario_id jointly identify the twelve
# consumption-group variates.
water_group_data_keys <- list(
  water_group_data = list(
    cols = names(water_group_data),
    keys = c("zone", "consumption_group", "scenario_id")
  )
)


# Monte Carlo node specifications
#
# source_conc and treatment_lrv use PERT distributions.
# intrusion_conc uses a uniform distribution.
# The remaining inputs are deterministic in the main Monte Carlo model.
#
# sample_space defines parameter ranges for optional sample-design sensitivity
# analyses. These ranges are illustrative.
water_mctable <- data.frame(
  mcnode = c(
    "source_conc",
    "treatment_lrv",
    "water_volume",
    "intrusion_prob",
    "intrusion_conc",
    "dose_response_r",
    "population",
    "exposure_days"
  ),
  description = c(
    "Cryptosporidium concentration in source water (oocysts/L)",
    "Cryptosporidium log10 reduction during treatment",
    "Daily consumption of unboiled tap water (L/person/day)",
    "Daily probability that a distribution zone is affected by intrusion",
    "Cryptosporidium concentration if intrusion occurs (oocysts/L)",
    "Exponential Cryptosporidium dose-response parameter",
    "Population exposed in the distribution zone",
    "Number of daily exposure events"
  ),
  mc_func = c(
    "rpert",
    "rpert",
    NA,
    NA,
    "runif",
    NA,
    NA,
    NA
  ),
  from_variable = rep(NA_character_, 8),
  transformation = rep(NA_character_, 8),
  sensi_variation = rep(NA_character_, 8),
  sample_space = c(
    "min = 0.5, mode = 1.0, max = 1.5",
    "min = 2.5, mode = 3.0, max = 3.5",
    "min = 1.0, max = 2.0",
    "min = 0.0002, max = 0.005",
    "min = 0.01, max = 0.05",
    "c(0.018, 0.018)",
    NA,
    "c(30, 30)"
  ),
  stringsAsFactors = FALSE
)


# Model expressions

# The treatment pathway estimates infection probability from the
# concentration remaining after treatment.
treatment_exp <- quote({
  treated_conc <- source_conc * 10^(-treatment_lrv)
  treatment_dose <- treated_conc * water_volume

  p_inf_treatment <- 1 - exp(-dose_response_r * treatment_dose)

  # Independent person-day exposure trials in each variate
  person_days <- population * exposure_days
})


# The intrusion pathway estimates infection probability conditional on an
# intrusion event.
intrusion_exp <- quote({
  intrusion_dose <- intrusion_conc * water_volume

  p_inf_given_intrusion <-
    1 - exp(-dose_response_r * intrusion_dose)
})


# Save package data
usethis::use_data(water_data, overwrite = TRUE)
usethis::use_data(water_group_data, overwrite = TRUE)
usethis::use_data(water_data_keys, overwrite = TRUE)
usethis::use_data(water_group_data_keys, overwrite = TRUE)
usethis::use_data(water_mctable, overwrite = TRUE)
usethis::use_data(treatment_exp, overwrite = TRUE)
usethis::use_data(intrusion_exp, overwrite = TRUE)
