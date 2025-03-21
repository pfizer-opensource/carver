% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tornado_plot.R
\name{process_tornado_data}
\alias{process_tornado_data}
\title{Pre-process data for tornado plot}
\usage{
process_tornado_data(
  dataset_adsl,
  dataset_analysis,
  adsl_subset = NA_character_,
  analysis_subset = NA_character_,
  obs_residual = NA_real_,
  ae_filter = "Any Event",
  pop_fil = NA_character_,
  fmq_data = NULL,
  split_by = NA_character_,
  ae_catvar,
  trtvar,
  trt_left,
  trt_right,
  trtsort = NA_character_,
  subset = NA_character_,
  pctdisp = "TRT",
  denom_subset = NA_character_,
  legendbign = "N",
  yvar = "AESOC"
)
}
\arguments{
\item{dataset_adsl}{(\code{data.frame}) ADSL dataset.}

\item{dataset_analysis}{(\code{data.frame}) ADAE dataset.}

\item{adsl_subset}{(\code{string}) Subset condition to be applied on \code{dataset_adsl}.}

\item{analysis_subset}{Subset conditions for \code{dataset_analysis}}

\item{obs_residual}{If not NA, use this argument to pass a period (numeric) to extend the
observation period. If passed as NA, overall study duration is considered for analysis.
eg. if 5, only events occurring upto 5 days past the TRTEDT are considered.}

\item{ae_filter}{Vector of adverse event types to be used as filter conditions.
Permissible Values: "ANY", "ANY EVENT", "TREATMENT EMERGENT", "SERIOUS",
"DRUG-RELATED", "RELATED", "MILD", "MODERATE", "SEVERE", "RECOVERED/RESOLVED",
"RECOVERING/RESOLVING", "NOT RECOVERING/NOT RESOLVING", "FATAL", "GRADE N"}

\item{pop_fil}{Population Filter for data set: Name of flag variable.
eg: \code{"SAFFL"}, \code{"EFFFL"} or \code{NA} for Overall Population.}

\item{fmq_data}{FMQ table dataframe, if passed, will be merged to adae date by PT.}

\item{split_by}{(\code{string}) By variable for stratification.}

\item{ae_catvar}{Categorical variable for severity analysis and order variable. eg; "ASEV/ASEVN"}

\item{trtvar}{(\code{string}) Treatment Variable to be created for analysis.}

\item{trt_left}{A Treatment value for displaying left hand side plot.}

\item{trt_right}{A Treatment value for displaying right hand side plot.}

\item{trtsort}{(\code{string}) Variable to sort treatment variable by.}

\item{subset}{Overall subset for data set. eg: "EFFFL == 'Y'"
eg: \code{"SAFFL"}, \code{"EFFFL"} or \code{NA} for Overall Population.}

\item{pctdisp}{Method to calculate denominator (for \%) by
Possible values: "TRT","VAR","COL","SUBGRP","CAT","NONE","NO","DPTVAR"}

\item{denom_subset}{Subset condition to be applied to data set for
calculating denominator.}

\item{legendbign}{(\code{string}) Display BIGN in Legend (\code{Y/N}).}

\item{yvar}{Categorical Analysis variable for Y axis}
}
\value{
mcatstat dataset as data frame.
}
\description{
Pre-process data for tornado plot
}
\details{
\itemize{
\item ae_catvar grouping variable for severity like AESEV(MILD, MODERATE,
SEVERE). It must be passed "/" separated with its numeric variable.
eg: ASEV/ASEVN; ATOXGR/ATOXGRN
\item yvar(dptvar) Adverse Event category, derived term from AE.
Possible Values: AEBODSYS, AEDECOD, AEHLT, AEHLGT.
}
}
\examples{
data(tornado_plot_data)

process_tornado_data(
  dataset_adsl = adsl,
  dataset_analysis = adae,
  adsl_subset = "SAFFL == 'Y'",
  analysis_subset = "TRTEMFL == 'Y'",
  obs_residual = "30",
  fmq_data = NA,
  ae_catvar = "AESEV/AESEVN",
  trtvar = "ARM",
  trt_left = "Xanomeline High Dose",
  trt_right = "Xanomeline Low Dose",
  pctdisp = "TRT",
  denom_subset = NA_character_,
  legendbign = "N",
  yvar = "AESOC"
)

}
