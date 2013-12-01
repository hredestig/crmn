\name{makeX_eset}
\alias{makeX,ExpressionSet,character-method}
\alias{makeX_eset}
\title{Make X}
\usage{
  makeX_eset(object, factors, ...)
}
\arguments{
  \item{object}{an \code{ExpressionSet}}

  \item{factors}{column names from the pheno data of
  \code{object}}

  \item{...}{not used}
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
  the formula is the most simple possible, i.e.
  \code{y~-1+term1+term2+...}
}
\examples{
data(mix)
makeX(mix, "runorder")
}
\author{
  Henning Redestig
}

