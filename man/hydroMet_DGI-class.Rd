% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_hydroMet_DGI.R
\docType{class}
\name{hydroMet_DGI-class}
\alias{hydroMet_DGI-class}
\alias{hydroMet_DGI}
\title{\code{hydroMet} subclass for DGI (Departamento General de Irrigación) data}
\value{
A hydroMet_DGI class object.
}
\description{
A suitable object for store hydro-meteorological data from DGI.
}
\section{Slots}{

\describe{
\item{\code{hsnow}}{data.frame from \link{read_DGI} containing daily snow height [m].}

\item{\code{swe}}{data.frame from \link{read_DGI} containing daily snow water equivalent [mm].}

\item{\code{tmean}}{data.frame from \link{read_DGI} containing daily mean air temperature [ºC].}

\item{\code{tmax}}{data.frame from \link{read_DGI} containing daily max. air temperature [ºC].}

\item{\code{tmin}}{data.frame from \link{read_DGI} containing daily min. air temperature [ºC].}

\item{\code{hr}}{data.frame from \link{read_DGI} containing daily mean relative humidity [\%].}

\item{\code{patm}}{data.frame from \link{read_DGI} containing daily mean atmospheric pressure [hPa].}
}}

