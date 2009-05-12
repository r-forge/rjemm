\name{displayStates}
\alias{displayStates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Displays the EMM States }
\description{
  Prints out the states and number of times in state of an EMM
}
\usage{
displayStates(JEMM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{JEMM}{ The EMM object to get states for }
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
