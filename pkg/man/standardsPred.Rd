\name{standardsPred}
\alias{standardsPred}
\title{Predict effect for new data (or get fitted data)}
\usage{
  standardsPred(model, newdata, factors, lg = TRUE, ...)
}
\arguments{
  \item{model}{result from \code{standardsFit}}

  \item{newdata}{an \code{ExpressionSet} or \code{matrix}
  with new data (or the data used to fit the model to get
  the fitted data)}

  \item{factors}{the biological factors described in the
  pheno data slot if \code{object} is an
  \code{ExpressionSet} or a design matrix if \code{object}
  is a \code{matrix}.}

  \item{lg}{logical indicating that the data should be log
  transformed}

  \item{...}{passed on to \code{standards} and
  \code{analytes}}
}
\value{
  the corrected data
}
\description{
  Predicted values for the standards
}
\details{
  There is often unwanted variation in among the labeled
  internal standards which is related to the experimental
  factors due to overlapping peaks etc. This predicts this
  effect given a model of the overlapping variance. The
  prediction is given by
  \eqn{\hat{X}_{IS}=X_{IS}-X_{IS}B}{XhatIS=XIS-XIS*B}
}
\examples{
data(mix)
fullFit <- standardsFit(mix, "type", ncomp=3)
sfit <- standardsFit(mix[,-1], "type", ncomp=3)
pred <- standardsPred(sfit, mix[,1], "type")
cor(scores(sfit$fit$pc)[1,], scores(fullFit$fit$pc)[1,])
## could just as well have been done as
Y <- exprs(mix)
G <- model.matrix(~-1+mix$type)
isIS <- fData(mix)$tag == 'IS'
fullFit <- standardsFit(Y, G, ncomp=3, standards=isIS)
sfit    <- standardsFit(Y[,-1], G[-1,], ncomp=3,
                        standards=isIS)
pred <- standardsPred(sfit, Y[,1,drop=FALSE], G[1,,drop=FALSE], standards=isIS)
cor(scores(sfit$fit$pc)[1,], scores(fullFit$fit$pc)[1,])
}
\author{
  Henning Redestig
}
\seealso{
  \code{makeX}, \code{standardsFit}
}

