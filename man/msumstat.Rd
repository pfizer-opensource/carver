% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msumstat.R
\name{msumstat}
\alias{msumstat}
\title{Summary statistics for numeric data variable}
\usage{
msumstat(
  datain = NULL,
  a_subset = NA_character_,
  dptvar = NULL,
  statvar = "",
  sigdec = "",
  dptvarn = 1,
  sparsebyvalyn = "N",
  figyn = "N"
)
}
\arguments{
\item{datain}{Input dataset from \code{mentry()}}

\item{a_subset}{Analysis subset condition specific to this function.}

\item{dptvar}{Numerical variable for analysis. eg: \code{"AGE"}, \code{"HEIGHTBL"}}

\item{statvar}{\code{Tilde} (\code{~})-separated list of statistics to be computed. eg: \code{"mean~median"}}

\item{sigdec}{Number of base decimal places to retain in output
Applies to mean, min, max, sd etc}

\item{dptvarn}{Number to assign as \code{'DPTVARN'}, used for block sorting when
multiple blocks are created to be combined.}

\item{sparsebyvalyn}{Sparse missing categories within by groups. \code{"Y"/"N"}}

\item{figyn}{Determine if output is for figure or not \code{"Y"/"N"}}
}
\value{
a list containing 2 elements
\itemize{
\item \code{tsum} - Dataset with statistics as rows, for table output
\item \code{gsum} - Dataset with statistics as columns, for plot output
}
}
\description{
Summary statistics for numeric data variable
}
\details{
Current available statistics (values for \code{statvar}) :
n (count per group), mean, median, sd (standard deviation), min, max,
iqr (interquartile range), var (variance), sum, range ("min, max")
mean(sd), median(minmax), q25/q1 (25 \% quantile), q75/q3 (75 \% quantile) , p10 (10\% quantile),
p5, p1, p90, p95, p99, q1q3 ("q25, q75"), whiskerlow, whiskerup (box lower/upper
whiskers), outliers (boxplot outliers, tilde-separated output), geometric mean/sd/CI
box = median~q25~q75~whiskerlow~whiskerup~outliers (Tukey's method)
}
\examples{
data(adsl)

adsl_entry <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = "SEX",
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  pop_fil = NA
)

adsl_sum <- adsl_entry |>
  msumstat(
    dptvar = "AGE",
    a_subset = "SEX == 'F'",
    statvar = "mean(sd)~median(minmaxc)~q3",
    sigdec = "3(2)~2(0)~1",
    sparsebyvalyn = "N"
  )

adsl_sum$tsum
adsl_sum$gsum

}
