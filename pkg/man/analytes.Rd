\name{analytes}
\alias{analytes}
\title{Accessor for the analytes}
\arguments{
  \item{object}{an \code{ExpressionSet}, \code{matrix} or
  \code{data.frame}}

  \item{standards}{a logical vector indicating which rows
  are internal analytes}

  \item{...}{not used}
}
\value{
  subsetted dataset
}
\description{
  Subset an data set to only contain the analytes.
}
\examples{
data(mix)
analytes(mix)
analytes(exprs(mix), fData(mix)$tag == 'IS')
}
\author{
  Henning Redestig
}

