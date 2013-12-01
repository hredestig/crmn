\name{mexprs}
\alias{mexprs}
\alias{mexprs,ExpressionSet-method}
\alias{mexprs,matrix-method}
\title{Matrix safe accessor of expression slot}
\arguments{
  \item{object}{an \code{ExpressionSet} or \code{matrix}}
}
\value{
  the expression data
}
\description{
  Get the expression data from an \code{ExpressionSet} or
  just return the given matrix
}
\examples{
data(mix)
head(mexprs(mix))
head(mexprs(exprs(mix)))
}
\author{
  Henning Redestig
}

