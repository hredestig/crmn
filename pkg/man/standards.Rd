\name{standards}
\alias{standards}
\title{Accessor for the Internal Standards}
\arguments{
  \item{object}{an \code{ExpressionSet}, \code{matrix} or
  \code{data.frame}}

  \item{standards}{a logical vector indicating which rows
  are internal standards}

  \item{...}{not used}
}
\value{
  subsetted dataset
}
\description{
  Subset an data set to only contain the labeled internal
  standards.
}
\examples{
data(mix)
standards(mix)
standards(exprs(mix), fData(mix)$tag == 'IS')
}
\author{
  Henning Redestig
}

