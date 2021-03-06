% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_hydroMet_CR2.R
\docType{class}
\name{hydroMet_CR2-class}
\alias{hydroMet_CR2-class}
\alias{hydroMet_CR2}
\title{\code{hydroMet} subclass for CR2 (Explorador Climático) data}
\value{
A hydroMet_CR2 class object.
}
\description{
A suitable object for store hydro-meteorological data from CR2.
}
\section{Slots}{

\describe{
\item{\code{precip}}{data.frame from \link{read_CR2} containing daily precipitation [mm].}

\item{\code{tmean}}{data.frame from \link{read_CR2} containing daily mean air temperature [ºC].}

\item{\code{tmax}}{data.frame from \link{read_CR2} containing daily maximum air temperature [ºC].}

\item{\code{tmin}}{data.frame from \link{read_CR2} containing daily minimum air temperature [ºC].}
}}

