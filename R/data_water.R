#' Water Supply Example Data
#'
#' Simulated data for an illustrative quantitative microbial risk assessment
#' of \emph{Cryptosporidium} infection in a water distribution system. Each row
#' represents one distribution zone under one management scenario.
#'
#' The example includes a baseline scenario, identified by `"0"`, and a
#' hypothetical network-maintenance scenario that reduces the daily intrusion
#' probability by 80 percent.
#'
#' @format A data frame with 4 rows and 15 columns:
#' \describe{
#'   \item{zone}{Distribution-zone identifier.}
#'   \item{scenario_id}{Management-scenario identifier. `"0"` indicates the
#'   baseline scenario.}
#'   \item{population}{Number of people exposed in the distribution zone.}
#'   \item{source_conc_min}{Minimum \emph{Cryptosporidium} concentration in source
#'   water, in oocysts per litre.}
#'   \item{source_conc_mode}{Most likely \emph{Cryptosporidium} concentration in
#'   source water, in oocysts per litre.}
#'   \item{source_conc_max}{Maximum \emph{Cryptosporidium} concentration in source
#'   water, in oocysts per litre.}
#'   \item{treatment_lrv_min}{Minimum treatment performance, expressed as a
#'   log10 reduction value.}
#'   \item{treatment_lrv_mode}{Most likely treatment performance, expressed as
#'   a log10 reduction value.}
#'   \item{treatment_lrv_max}{Maximum treatment performance, expressed as a
#'   log10 reduction value.}
#'   \item{water_volume}{Daily consumption of unboiled tap water, in litres per
#'   person per day.}
#'   \item{intrusion_prob}{Daily probability that an intrusion affects the
#'   distribution zone.}
#'   \item{intrusion_conc_min}{Minimum \emph{Cryptosporidium} concentration at the tap
#'   conditional on an intrusion, in oocysts per litre.}
#'   \item{intrusion_conc_max}{Maximum \emph{Cryptosporidium} concentration at the tap
#'   conditional on an intrusion, in oocysts per litre.}
#'   \item{dose_response_r}{Parameter of the exponential \emph{Cryptosporidium}
#'   dose-response model.}
#'   \item{exposure_days}{Number of daily exposure events in the assessment
#'   period.}
#' }
#'
#' @references
#' World Health Organization (2016). Quantitative microbial risk assessment:
#' application for water safety management.
#' \url{https://www.who.int/publications/i/item/9789241565370}
#'
#' Messner MJ, Chappell CL, and Okhuysen PC (2001). Risk assessment for
#' \emph{Cryptosporidium}: a hierarchical Bayesian analysis of human dose-response
#' data. Water Research, 35, 3934-3940.
#' \doi{10.1016/S0043-1354(01)00119-1}
#'
#' Besner MC, Prevost M, and Regli S (2011). Assessing the public health risk
#' of microbial intrusion events in distribution systems. Water Research, 45,
#' 961-979. \doi{10.1016/j.watres.2010.10.035}
#'
#' @source Simulated data for demonstration purposes.
#' @seealso [water_group_data], [water_data_keys], [water_group_data_keys],
#'   [water_mctable], [treatment_exp], [intrusion_exp]
#' @examples
#' water_data
#' @docType data
#' @keywords datasets
"water_data"


#' Water Supply Example Data by Consumption Group
#'
#' An expanded version of [water_data] for demonstrating multiple-group
#' multilevel trials. Each distribution-zone and scenario combination is
#' divided into groups with low, medium, and high daily water consumption.
#'
#' The consumption groups contain 25, 50, and 25 percent of the corresponding
#' zone population and consume 1.0, 1.5, and 2.0 litres of unboiled tap water
#' per person per day, respectively.
#'
#' @format A data frame with 12 rows and 16 columns:
#' \describe{
#'   \item{zone}{Distribution-zone identifier.}
#'   \item{consumption_group}{Water-consumption group: `"Low"`, `"Medium"`, or
#'   `"High"`.}
#'   \item{scenario_id}{Management-scenario identifier. `"0"` denotes the
#'   baseline scenario.}
#'   \item{population}{Number of people in the consumption group. Group
#'   populations sum to the corresponding population in [water_data].}
#'   \item{source_conc_min}{Minimum \emph{Cryptosporidium} concentration in source
#'   water, in oocysts per litre.}
#'   \item{source_conc_mode}{Most likely \emph{Cryptosporidium} concentration in
#'   source water, in oocysts per litre.}
#'   \item{source_conc_max}{Maximum \emph{Cryptosporidium} concentration in source
#'   water, in oocysts per litre.}
#'   \item{treatment_lrv_min}{Minimum treatment performance, expressed as a
#'   log10 reduction value.}
#'   \item{treatment_lrv_mode}{Most likely treatment performance, expressed as
#'   a log10 reduction value.}
#'   \item{treatment_lrv_max}{Maximum treatment performance, expressed as a
#'   log10 reduction value.}
#'   \item{water_volume}{Daily consumption of unboiled tap water for the
#'   consumption group, in litres per person per day.}
#'   \item{intrusion_prob}{Daily probability that an intrusion affects the
#'   distribution zone.}
#'   \item{intrusion_conc_min}{Minimum \emph{Cryptosporidium} concentration at the tap
#'   conditional on an intrusion, in oocysts per litre.}
#'   \item{intrusion_conc_max}{Maximum \emph{Cryptosporidium} concentration at the tap
#'   conditional on an intrusion, in oocysts per litre.}
#'   \item{dose_response_r}{Parameter of the exponential \emph{Cryptosporidium}
#'   dose-response model.}
#'   \item{exposure_days}{Number of daily exposure events in the assessment
#'   period.}
#' }
#'
#' @details
#' The consumption groups in the same distribution zone share the occurrence
#' or absence of a daily intrusion event. Their conditional infection
#' probabilities differ because `water_volume` differs between groups. This
#' structure can be evaluated with [trial_totals()] by using `population` as
#' the number of trials and aggregating by `zone` and `scenario_id`.
#'
#' @source Derived from [water_data] using hypothetical population shares and
#' water-consumption values.
#' @seealso [water_data], [water_group_data_keys], [water_mctable],
#'   [intrusion_exp], [trial_totals()]
#' @examples
#' water_group_data
#'
#' # Population is preserved within each zone and scenario.
#' aggregate(
#'   population ~ zone + scenario_id,
#'   data = water_group_data,
#'   FUN = sum
#' )
#' @docType data
#' @keywords datasets
"water_group_data"


#' Data Keys for the Water Supply Example
#'
#' A data-key specification describing the columns and identifying variables
#' in [water_data]. The combination of `zone` and `scenario_id` uniquely
#' identifies each model variate.
#'
#' @format A named list with one component:
#' \describe{
#'   \item{water_data}{A list containing `cols`, the column names available in
#'   [water_data], and `keys`, the columns `zone` and `scenario_id` that jointly
#'   identify its rows.}
#' }
#'
#' @source Created for the illustrative water-supply example.
#' @seealso [water_data], [water_group_data_keys], [water_mctable],
#'   [eval_module()]
#' @examples
#' water_data_keys
#' water_data_keys$water_data$keys
#' @docType data
#' @keywords datasets
"water_data_keys"


#' Data Keys for the Consumption-Group Water Example
#'
#' A data-key specification describing the columns and identifying variables
#' in [water_group_data]. The combination of `zone`, `consumption_group`, and
#' `scenario_id` uniquely identifies each model variate.
#'
#' @format A named list with one component:
#' \describe{
#'   \item{water_group_data}{A list containing `cols`, the column names
#'   available in [water_group_data], and `keys`, the columns `zone`,
#'   `consumption_group`, and `scenario_id` that jointly identify its rows.}
#' }
#'
#' @source Created for the illustrative water-supply example.
#' @seealso [water_group_data], [water_data_keys], [water_mctable],
#'   [eval_module()], [trial_totals()]
#' @examples
#' water_group_data_keys
#' water_group_data_keys$water_group_data$keys
#' @docType data
#' @keywords datasets
"water_group_data_keys"


#' Monte Carlo Node Specifications for the Water Supply Example
#'
#' A configured Monte Carlo node table for the illustrative water-supply risk
#' assessment. It defines the input nodes, probability distributions, and
#' sampling ranges used to evaluate the treatment and intrusion pathways.
#'
#' `source_conc` and `treatment_lrv` use PERT distributions, while
#' `intrusion_conc` uses a uniform distribution. The remaining inputs are
#' deterministic in the main Monte Carlo model. The `sample_space` values
#' provide illustrative ranges for optional sample-design sensitivity
#' analyses.
#'
#' @format A data frame with 8 rows and 7 columns:
#' \describe{
#'   \item{mcnode}{Name of the Monte Carlo input node.}
#'   \item{description}{Description of the represented parameter.}
#'   \item{mc_func}{Random-number generation function. `NA` identifies a
#'   deterministic input.}
#'   \item{from_variable}{Alternative source column used to create the node,
#'   where applicable.}
#'   \item{transformation}{Transformation applied to the source values, where
#'   applicable.}
#'   \item{sensi_variation}{Expression defining variation for one-at-a-time
#'   sensitivity analysis.}
#'   \item{sample_space}{Sampling range or distribution arguments used for
#'   sample-design sensitivity analysis.}
#' }
#'
#' @source Parameter ranges are illustrative. They do not represent a fitted model
#' for a specific water system.
#' @seealso [water_data], [water_group_data], [water_data_keys],
#'   [water_group_data_keys], [treatment_exp], [intrusion_exp], [eval_module()]
#' @examples
#' water_mctable
#' @docType data
#' @keywords datasets
"water_mctable"


#' Treatment-Pathway Expression for the Water Supply Example
#'
#' A quoted R expression representing the routine water-treatment pathway in
#' the illustrative water-supply risk assessment. It calculates the treated
#' \emph{Cryptosporidium} concentration, ingested dose, individual infection
#' probability, and number of person-day exposure trials.
#'
#' @format A quoted R expression that creates the following model nodes:
#' \describe{
#'   \item{treated_conc}{\emph{Cryptosporidium} concentration remaining after
#'   treatment.}
#'   \item{treatment_dose}{Daily ingested dose from treated water.}
#'   \item{p_inf_treatment}{Individual daily probability of infection through
#'   the treatment pathway.}
#'   \item{person_days}{Number of person-day exposure trials, calculated as
#'   population multiplied by exposure days.}
#' }
#'
#' @source Created for the illustrative water-supply example.
#' @seealso [water_data], [water_group_data], [water_data_keys],
#'   [water_group_data_keys], [water_mctable], [intrusion_exp], [eval_module()]
#' @examples
#' treatment_exp
#'
#' treatment_module <- eval_module(
#'   exp = list(treatment = treatment_exp),
#'   data = water_data,
#'   mctable = water_mctable,
#'   data_keys = water_data_keys
#' )
#' @docType data
#' @keywords datasets
"treatment_exp"


#' Intrusion-Pathway Expression for the Water Supply Example
#'
#' A quoted R expression representing contamination introduced through an
#' intrusion into the water distribution system. It calculates the ingested
#' dose and individual infection probability conditional on an intrusion.
#'
#' The probability and frequency of intrusion events are not applied inside
#' this expression. They can subsequently be incorporated with
#' [trial_totals()] using `intrusion_prob` and `exposure_days` from
#' [water_data] or [water_group_data].
#'
#' @format A quoted R expression that creates the following model nodes:
#' \describe{
#'   \item{intrusion_dose}{Daily ingested dose conditional on an intrusion.}
#'   \item{p_inf_given_intrusion}{Individual daily probability of infection
#'   conditional on an intrusion.}
#' }
#'
#' @source Created for the illustrative water-supply example.
#' @seealso [water_data], [water_group_data], [water_data_keys],
#'   [water_group_data_keys], [water_mctable], [treatment_exp], [eval_module()],
#'   [trial_totals()]
#' @examples
#' intrusion_exp
#'
#' intrusion_module <- eval_module(
#'   exp = list(intrusion = intrusion_exp),
#'   data = water_data,
#'   mctable = water_mctable,
#'   data_keys = water_data_keys
#' )
#'
#' intrusion_group_module <- eval_module(
#'   exp = list(intrusion = intrusion_exp),
#'   data = water_group_data,
#'   mctable = water_mctable,
#'   data_keys = water_group_data_keys
#' )
#' @docType data
#' @keywords datasets
"intrusion_exp"
