\name{detectEvent}
\alias{detectEvent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Detect Rare Events }
\description{
  Adds new data to a model and detects rare events
}
\usage{
detectEvent(JEMM, newdata, measure = "Cosine", threshold = 0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{JEMM}{ The EMM model to detect rare events for }
  \item{newdata}{ The data that you want to add }
  \item{measure}{ The clustering algorithm }
  \item{threshold}{ The threshold for the clustering algorithm }
}
\value{
  Prints out the rare states
}
\author{ Benjamin Koopferstock }
\examples{
library(RJEMM)
data(anomaly)
emm <- create_EMM(measure = "Dice", threshold = 0.5, )
build( emm, anomaly)
data(anomalyRare)
detectEvent(emm, anomalyRare, "Dice", 0.9)
}
