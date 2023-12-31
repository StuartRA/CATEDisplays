% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/displayCATE.R
\name{displayCATE}
\alias{displayCATE}
\title{displayCATE}
\usage{
displayCATE(
  dfList,
  outCol,
  txCol,
  covList,
  blpredList,
  combine,
  verbose,
  ci,
  nTrees,
  seedN
)
}
\arguments{
\item{dfList}{list; a list of dataframes, one per trial}

\item{outCol}{character; name of outcome column}

\item{txCol}{character; name of treatment status column}

\item{covList}{vector; column names of covariates to estimate forest}

\item{blpredList}{vector; number of units to be initially randomly selected}

\item{combine}{logical; combine data from multiple studies}

\item{ci}{numeric; confidence interval \%}

\item{nTrees}{numeric; column names of unit level variables  to sample units on}

\item{seedN}{numeric; seed number to be used for sampling. If NA, calls set.seed(); default = NA}
}
\value{
list with:
\enumerate{
\item full output from getCATE(), 2) tauHat figures, 3) sub-group CATE figures, 4) best linear projection figures
}
}
\description{
Estimates CATE using causal forests with data from multiple trials
and displays multiple treatment effect heterogeneity diagnostics by subgroup.
}
