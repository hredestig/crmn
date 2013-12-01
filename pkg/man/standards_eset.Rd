\name{standards_eset}
\alias{standards,ExpressionSet,missing-method}
\alias{standards_eset}
\title{Accessor for the Internal Standards}
\usage{
  standards_eset(object, where = "tag", what = "IS", ...)
}
\arguments{
  \item{object}{an \code{ExpressionSet}}

  \item{where}{Column index or name in fData which equals
  \code{what} for the ISs}

  \item{what}{What the column \code{where} equals for ISs}

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
fData(mix)$test <- fData(mix)$tag
standards(mix, where="test")
}
\author{
  Henning Redestig
}

