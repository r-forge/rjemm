\name{save}
\alias{save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Save an EMM model }
\description{
  Saves an EMM model in the same format that JEMM does, so any saved model in RJEMM can be loaded in EMM
}
\usage{
save(location = "/EMMModel.bin", JEMM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location}{ The location and name to save the model }
  \item{JEMM}{ The EMM model to save }
}
\author{ Benjamin Koopferstock }

\examples{
emm <- create_EMM(measure = "Dice", threshold = 0.5, )
data(anomaly)
build( emm, anomaly)
save("/home/ben/Documents/EMM/EMMModel.bin", emm)
}

