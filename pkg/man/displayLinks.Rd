\name{displayLinks}
\alias{displayLinks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Displays EMM Links }
\description{
  Prints out a table of the links (transitions between states) in an EMM and how often they are traversed.
}
\usage{
displayLinks(JEMM)
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
