% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textBoxPlacement.R
\name{positionTextBox}
\alias{positionTextBox}
\title{positionTextBox}
\usage{
positionTextBox(text, x, y, xPos, adj, nApprox = 10, reallyText)
}
\arguments{
\item{text}{character string text to insert in plot}

\item{x}{numeric vector of x values}

\item{y}{numeric vector of y values}

\item{xPos}{numeric x position for text box}

\item{adj}{numeric vector param passed to text()}

\item{nApprox}{integer number of intervals to interpolate between x data points}

\item{reallyText}{Boolean if TRUE then execute text() command}
}
\value{
returns a list c(ymin,ymax,strheight(text),xPos1)
}
\description{
compute x and y coordinates for placement of text box
based upon the values of the function
to avoid running into the graph line
}
\examples{
x<-1:10
y<-1:10
plot(x,y,type="l")
positionTextBox(text="TEXT ME",x=x,y=y,xPos=1,
  adj=c(0,0),nApprox=10,reallyText=TRUE)

}
