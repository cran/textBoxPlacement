% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textBoxPlacement.R
\name{permInd}
\alias{permInd}
\title{permInd}
\usage{
permInd(yrange)
}
\arguments{
\item{yrange}{numeric vector (max-min) for vector of y values}
}
\value{
returns the integer vector indices of sorting yrange in decreasing order
}
\description{
compute the indices of sorting yrange in decreasing order
}
\examples{
permInd(yrange(yList1))

}
