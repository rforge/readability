\name{readability_features}
\alias{readability_features}
\title{Create Readability Features}
\usage{
readability_features(a, id = NULL, counts = FALSE)
}
\arguments{
\item{a}{an object inheriting from class \code{"AnnotatedPlainTextDocument"}.}
\item{id}{id an optional identifier.}
\item{counts}{a boolean selecting if the count variables should be returned.}
}
\value{
Returns an object inheriting from \code{'data.frame'}.
}
\description{
A convenience function for creating features for readability prediction.
}
