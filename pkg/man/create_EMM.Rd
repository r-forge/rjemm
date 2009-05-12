\name{create_EMM}
\alias{create_EMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create an EMM Object }
\description{
  create_EMM creates an EMM model using the supplied threshold and clustering algorithm.  Choices for the clustering algorithm are "Cosine", "Dice", "Euclidean", "Jaccard", and "Overlap".  It can also be used to load an existing EMM model if the location variable is set to the location of the saved model.
}
\usage{
create_EMM(measure = "Cosine", threshold = 0.2, location = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measure}{ measure for clustering }
  \item{threshold}{ threshold for clustering }
  \item{location}{ if a saved model should be loaded, put the location here }
}
\value{
  Returns a JREMM object
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
