% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swe_to_precip.R
\name{swe_to_precip}
\alias{swe_to_precip}
\title{Snow water equivalent to snowfall}
\usage{
swe_to_precip(df)
}
\arguments{
\item{df}{data frame with 'swe' series in the second column. See \code{'read_XXX'} functions.}
}
\value{
Data frame containing the numeric vector with inferred snowfall.
}
\description{
Converts a snow water equivalent series (from snow pillow) to a snowfall series.
}
\examples{
# Relative path to raw data
full_path <- system.file('extdata', package = "hydroToolkit")

# Read swe sheet
toscas_swe <- read_DGI(file = 'Toscas.xlsx', sheet = 'swe',
                colName = 'swe(mm)', path = full_path)

# swe to snowfall
toscas_snfall <- swe_to_precip(df = toscas_swe)    

}
