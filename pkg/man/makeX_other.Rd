\name{makeX_other}
\alias{makeX,ANY,matrix-method}
\alias{makeX_other}
\title{Make X}
\usage{
  makeX_other(object, factors, ...)
}
\arguments{
  \item{object,}{not used}

  \item{factors}{a design matrix}

  \item{...}{not used}
}
\value{
  the same design matrix
}
\description{
  Construct a design matrix
}
\details{
  Convenience function that just return the given design
  matrix.
}
\examples{
data(mix)
makeX(mix, model.matrix(~pData(mix)[,"runorder"]))
}
\author{
  Henning Redestig
}

