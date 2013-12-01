\name{analytes_eset}
\alias{analytes,ExpressionSet,missing-method}
\alias{analytes_eset}
\title{Accessor for the analytes}
\usage{
  analytes_eset(object, where = "tag", what = "IS", ...)
}
\arguments{
  \item{object}{an \code{ExpressionSet}}

  \item{where}{Column index or name of fData which equals
  \code{what} for the ISs (and something else for the
  analytes)}

  \item{what}{What the column \code{where} does not equal
  for analytes. Can be vector values too.}

  \item{...}{not used}
}
\value{
  \code{ExpressionSet}
}
\description{
  Subset an expression set to remove the internal standards
}
\examples{
data(mix)
analytes(mix)
fData(mix)$test <- fData(mix)$tag
analytes(mix, where="test")
}
\author{
  Henning Redestig
}

