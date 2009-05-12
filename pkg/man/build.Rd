\name{build}
\alias{build}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Build an EMM Model }
\description{
  Builds an EMM model using provided data.
}
\usage{
build(JEMM, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{JEMM}{ The EMM object that you want to add data to }
  \item{newdata}{ The data that you want to add }
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
