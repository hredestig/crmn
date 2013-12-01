\name{makeX}
\alias{makeX}
\title{Make X}
\arguments{
  \item{object}{an \code{ExpressionSet}}

  \item{factors}{column names from the pheno data of
  \code{object} or a design matrix}
}
\value{
  a design matrix
}
\description{
  Construct a design matrix
}
\details{
  Make a design matrix from the pheno data slot of an
  expression set, taking care that factors and numerical
  are handled properly. No interactions are included and
  formula is the most simple possible, i.e.
  \code{y~-1+term1+term2+...}. Can also be given anything
  as object in which case \code{factor} must be a design
  matrix. It that case the same design matrix is returned.
}
\examples{
data(mix)
makeX(mix, "runorder")
runorder <- mix$runorder
makeX(mix, model.matrix(~-1+runorder))
}
\author{
  Henning Redestig
}

