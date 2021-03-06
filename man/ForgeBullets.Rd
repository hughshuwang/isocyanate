% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bullets.R
\name{ForgeBullets}
\alias{ForgeBullets}
\title{Forge a dataframe of bullets with proper colnames}
\usage{
ForgeBullets(core, name.core, funcs, periods)
}
\arguments{
\item{core}{xts series, core time series for forging}

\item{name.core}{string, at the beginning of colnames}

\item{funcs}{list of functions with all lower names}

\item{periods}{list of rolling period applied with all lower names}
}
\value{
dataframe without NA, properly named
}
\description{
Forge a dataframe of bullets with proper colnames
}
