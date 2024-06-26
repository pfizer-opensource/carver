# Copyright 2024 Pfizer Inc
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#' ADSL
#'
#' Subject Level Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adsl.xpt>,
#' downloaded 2023-03-17
#' @format Data frame with 254 features and 58 fields
"adsl"

#' ADLB
#'
#' Laboratory Results Chemistry Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adlb.xpt>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 74264 features and 46 fields
"adlb"

#' ADAE
#'
#' Adverse Events Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adae.xpt>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 1191 features and 55 fields
"adae"

#' FMQ Consolidated List
#'
#' Consolidated list of FDA Medical Queries.
#'
#' @source
#' <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/FMQ_Consolidated_List.csv>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 11712 features and 7 fields
"FMQ_Consolidated_List"

#' ae_pre
#'
#' Pre Processed `adae` data from `ae_pre_processor()`
#'
#' @format List of data frames of length `3`
"ae_pre"

#' ae_risk
#'
#' Output from `risk_stat()`
#'
#' @format Data frame with `46` rows and `17` variables
"ae_risk"

#' event_df
#'
#' Output from `risk_stat()` required for `event_analysis()`
#'
#' @format List containing 2 data frames `dsin` and `dout` (to be passed in `event_analysis`) and
#' `bigN` value
"event_df"

#' CM
#'
#' Concomitant Medications Dataset
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/cm.xpt>,
#' downloaded 2023-08-24
#'
#' @format Data frame with 7510 rows and 21 variables
"cm"
