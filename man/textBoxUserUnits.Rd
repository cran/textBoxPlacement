% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textBoxPlacement.R
\name{textBoxUserUnits}
\alias{textBoxUserUnits}
\title{textBoxUserUnits}
\usage{
textBoxUserUnits(tList, yrange, verbose)
}
\arguments{
\item{tList}{a list of character strings for text boxes to label the curves}

\item{yrange}{numeric vector (max-min) for vector of y values}

\item{verbose}{Boolean if TRUE print informative or diagnostic messages to console}
}
\value{
numeric vector of the heights of the text boxes in user units
}
\description{
compute the heights of the text boxes in user units
}
\examples{
textBoxUserUnits(textList,yrange(yList1),verbose=TRUE)
}
