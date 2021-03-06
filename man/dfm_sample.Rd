% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfm_sample.R
\name{dfm_sample}
\alias{dfm_sample}
\title{Randomly sample documents or features from a dfm}
\usage{
dfm_sample(x, size = ndoc(x), replace = FALSE, prob = NULL,
  margin = c("documents", "features"))
}
\arguments{
\item{x}{the \link{dfm} object whose documents or features will be sampled}

\item{size}{a positive number, the number of documents or features to select}

\item{replace}{logical; should sampling be with replacement?}

\item{prob}{a vector of probability weights for obtaining the elements of the
vector being sampled.}

\item{margin}{dimension (of a \link{dfm}) to sample: can be \code{documents} or
\code{features}}
}
\value{
A dfm object with number of documents or features equal to \code{size}, drawn 
  from the dfm \code{x}.
}
\description{
Sample randomly from a dfm object, from documents or features.
}
\examples{
set.seed(10)
dfmat <- dfm(data_corpus_inaugural[1:10])
head(dfmat)
head(dfm_sample(dfmat))
head(dfm_sample(dfmat, replace = TRUE))
head(dfm_sample(dfmat, margin = "features"))
}
\seealso{
\link{sample}
}
\keyword{dfm}
