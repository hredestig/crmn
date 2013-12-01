\name{set_mexprs}
\alias{mexprs<-}
\alias{mexprs<-,ExpressionSet,matrix-method}
\alias{mexprs<-,matrix,matrix-method}
\alias{set_mexprs}
\title{Accessor}
\arguments{
  \item{object}{an \code{ExpressionSet} or \code{matrix}}

  \item{value}{the value to assign}
}
\value{
  the expression data
}
\description{
  Matrix safe setter of expression slot
}
\details{
  Set the expression data in an \code{ExpressionSet} or
  just return the given matrix
}
\examples{
data(mix)
test <- mix
mexprs(test) <- exprs(mix) * 0
head(mexprs(test))
test <- exprs(mix)
mexprs(test) <- test * 0
head(mexprs(test))
}
\author{
  Henning Redestig
}

