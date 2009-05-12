\name{display}
\alias{display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Displays the EMM }
\description{
 A useful visualization tool that displays the EMM as a graph
}
\usage{
display(JEMM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{JEMM}{ The EMM object that should be displayed }
}
\author{ Benjamin Koopferstock }
\examples{
library(RJEMM)
emm <- create_EMM(measure = "Dice", threshold = 0.5, )
data(anomaly)
build( emm, anomaly)
displayStates(emm)
displayLinks(emm)
display(emm)
}
