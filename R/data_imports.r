#' Animal Import Data
#'
#' Simulated data describing the number of farms exporting animals and the
#' number of animals exported per farm from three hypothetical regions.
#'
#' @format A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{origin}{Region of origin: `"nord"`, `"south"`, or `"east"`.}
#'   \item{farms_n}{Number of farms exporting animals.}
#'   \item{animals_n_mean}{Mean number of animals exported per farm.}
#'   \item{animals_n_sd}{Standard deviation of the number of animals exported
#'   per farm.}
#' }
#'
#' @source Simulated data for demonstration purposes.
#' @seealso [prevalence_region], [test_sensitivity], [imports_data]
#' @examples
#' animal_imports
#' @docType data
#' @keywords datasets
"animal_imports"


#' Regional Pathogen Prevalence Data
#'
#' Simulated estimates of herd prevalence, within-herd prevalence, and testing
#' practices for two hypothetical pathogens in three regions of origin.
#'
#' @format A data frame with 6 rows and 7 columns:
#' \describe{
#'   \item{pathogen}{Hypothetical pathogen identifier: `"a"` or `"b"`.}
#'   \item{origin}{Region of origin: `"nord"`, `"south"`, or `"east"`.}
#'   \item{h_prev_min}{Minimum herd prevalence.}
#'   \item{h_prev_max}{Maximum herd prevalence.}
#'   \item{w_prev_min}{Minimum within-herd prevalence.}
#'   \item{w_prev_max}{Maximum within-herd prevalence.}
#'   \item{test_origin}{Frequency with which animals are tested at origin:
#'   `"always"`, `"sometimes"`, `"never"`, or `"unknown"`.}
#' }
#'
#' @source Simulated data for demonstration purposes.
#' @seealso [animal_imports], [test_sensitivity], [imports_data]
#' @examples
#' prevalence_region
#' @docType data
#' @keywords datasets
"prevalence_region"


#' Diagnostic Test Sensitivity Data
#'
#' Simulated minimum, most likely, and maximum diagnostic-test sensitivity
#' values for two hypothetical pathogens.
#'
#' @format A data frame with 2 rows and 4 columns:
#' \describe{
#'   \item{pathogen}{Hypothetical pathogen identifier: `"a"` or `"b"`.}
#'   \item{test_sensi_min}{Minimum diagnostic-test sensitivity.}
#'   \item{test_sensi_mode}{Most likely diagnostic-test sensitivity.}
#'   \item{test_sensi_max}{Maximum diagnostic-test sensitivity.}
#' }
#'
#' @source Simulated data for demonstration purposes.
#' @seealso [animal_imports], [prevalence_region], [imports_data]
#' @examples
#' test_sensitivity
#' @docType data
#' @keywords datasets
"test_sensitivity"


#' Merged Animal Import Data
#'
#' A model-ready dataset combining animal movements, pathogen prevalence, and
#' diagnostic-test sensitivity. Each row represents one combination of a
#' hypothetical pathogen and region of origin.
#'
#' @format A data frame with 6 rows and 13 columns:
#' \describe{
#'   \item{pathogen}{Hypothetical pathogen identifier: `"a"` or `"b"`.}
#'   \item{origin}{Region of origin: `"nord"`, `"south"`, or `"east"`.}
#'   \item{test_origin}{Frequency with which animals are tested at origin.}
#'   \item{h_prev_min}{Minimum herd prevalence.}
#'   \item{h_prev_max}{Maximum herd prevalence.}
#'   \item{w_prev_min}{Minimum within-herd prevalence.}
#'   \item{w_prev_max}{Maximum within-herd prevalence.}
#'   \item{farms_n}{Number of farms exporting animals.}
#'   \item{animals_n_mean}{Mean number of animals exported per farm.}
#'   \item{animals_n_sd}{Standard deviation of the number of animals exported
#'   per farm.}
#'   \item{test_sensi_min}{Minimum diagnostic-test sensitivity.}
#'   \item{test_sensi_mode}{Most likely diagnostic-test sensitivity.}
#'   \item{test_sensi_max}{Maximum diagnostic-test sensitivity.}
#' }
#'
#' @source Simulated data for demonstration purposes. The dataset is created
#' by joining [prevalence_region], [animal_imports], and [test_sensitivity].
#' @seealso [imports_data_keys], [imports_mctable], [imports_exp]
#' @examples
#' imports_data
#' @docType data
#' @keywords datasets
"imports_data"


#' Data Keys for the Animal Import Example
#'
#' A data-key specification describing the columns and identifying variables
#' associated with each source dataset in the animal-import example. These
#' keys allow `mcmodule` to associate input parameters with the appropriate
#' model variates.
#'
#' @format A named list with three components:
#' \describe{
#'   \item{animal_imports}{A list containing `cols`, the columns provided by
#'   [animal_imports], and `keys`, with `origin` as the identifying variable.}
#'   \item{prevalence_region}{A list containing `cols`, the columns provided by
#'   [prevalence_region], and `keys`, with `pathogen` and `origin` as the
#'   identifying variables.}
#'   \item{test_sensitivity}{A list containing `cols`, the columns provided by
#'   [test_sensitivity], and `keys`, with `pathogen` as the identifying
#'   variable.}
#' }
#'
#' @source Created for the illustrative animal-import example.
#' @seealso [imports_data], [imports_mctable], [imports_exp], [eval_module()]
#' @examples
#' imports_data_keys
#' imports_data_keys$prevalence_region$keys
#' @docType data
#' @keywords datasets
"imports_data_keys"


#' Monte Carlo Node Specifications for the Animal Import Example
#'
#' A configured Monte Carlo node table for the illustrative animal-import risk
#' assessment. It defines input nodes, probability distributions, source
#' columns, transformations, and sensitivity-analysis ranges.
#'
#' `h_prev` and `w_prev` use uniform distributions, `test_sensi` uses a PERT
#' distribution, and `animals_n` uses a normal distribution. `farms_n` is
#' deterministic. The `test_origin_unk` and `test_origin` nodes demonstrate
#' how categorical source data can be transformed into model inputs.
#'
#' @format A data frame with 7 rows and 7 columns:
#' \describe{
#'   \item{mcnode}{Name of the Monte Carlo input node.}
#'   \item{description}{Description of the represented parameter.}
#'   \item{mc_func}{Random-number generation function. `NA` identifies a
#'   deterministic input.}
#'   \item{from_variable}{Alternative source column used to create the node,
#'   where applicable.}
#'   \item{transformation}{Transformation applied to source values, where
#'   applicable.}
#'   \item{sensi_variation}{Expression defining variation for one-at-a-time
#'   sensitivity analysis.}
#'   \item{sample_space}{Sampling range or distribution arguments used for
#'   sample-design sensitivity analysis.}
#' }
#'
#' @source Simulated specifications for demonstration purposes.
#' @seealso [imports_data], [imports_data_keys], [imports_exp], [eval_module()]
#' @examples
#' imports_mctable
#' @docType data
#' @keywords datasets
"imports_mctable"


#' Model Expression for the Animal Import Example
#'
#' A quoted R expression that calculates the probability that an animal from
#' an infected herd is infected but is not detected before import. Diagnostic
#' test specificity is assumed to be 100 percent.
#'
#' @format A quoted R expression that creates the following model nodes:
#' \describe{
#'   \item{infected}{Probability that an animal from an infected herd is
#'   infected.}
#'   \item{false_neg}{Probability that an infected animal is tested but
#'   returns a false-negative result.}
#'   \item{no_test}{Probability that an infected animal is not tested.}
#'   \item{no_detect}{Overall probability that an infected animal is not
#'   detected before import.}
#' }
#'
#' @source Created for the illustrative animal-import example.
#' @seealso [imports_data], [imports_data_keys], [imports_mctable],
#'   [eval_module()]
#' @examples
#' imports_exp
#'
#' imports_module <- eval_module(
#'   exp = list(imports = imports_exp),
#'   data = imports_data,
#'   mctable = imports_mctable,
#'   data_keys = imports_data_keys
#' )
#' @docType data
#' @keywords datasets
"imports_exp"


#' Example Monte Carlo Module for Animal Imports
#'
#' A pre-evaluated [mcmodule][eval_module] object containing simulation results
#' for the illustrative animal-import risk assessment. The module represents
#' two hypothetical pathogens imported from three regions of origin and
#' includes input nodes, intermediate calculations, and the probability that
#' an infected animal is not detected before import.
#'
#' The object is generated from [imports_data], [imports_data_keys],
#' [imports_mctable], and [imports_exp]. It is provided so that package
#' functions can be demonstrated without rebuilding the model each time.
#'
#' @format An object of class `mcmodule` with the following components:
#' \describe{
#'   \item{data}{A named list containing the model input data.}
#'   \item{exp}{A named list containing the evaluated model expression.}
#'   \item{node_list}{A named list of Monte Carlo input and output nodes with
#'   their associated metadata. This includes `w_prev`, `test_sensi`,
#'   `test_origin`, `infected`, `false_neg`, `no_test`, and `no_detect`.}
#'   \item{modules}{Character vector identifying the modules represented in
#'   the object.}
#' }
#'
#' @source Generated from simulated data for demonstration purposes.
#' @seealso [imports_data], [imports_data_keys], [imports_mctable],
#'   [imports_exp], [eval_module()]
#' @examples
#' imports_mcmodule
#' class(imports_mcmodule)
#' names(imports_mcmodule$node_list)
#' mc_summary(imports_mcmodule, "no_detect")
#' @docType data
#' @keywords datasets
"imports_mcmodule"
