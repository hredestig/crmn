\name{normPred}
\alias{normPred}
\title{Predict for normalization}
\usage{
  normPred(normObj, newdata, factors = NULL, lg = TRUE,
    predfunc = predict, ...)
}
\arguments{
  \item{normObj}{the result from \code{normFit}}

  \item{newdata}{an \code{ExpressionSet} or a \code{matrix}
  (in which case the \code{standards} must be passed on via
  \code{...}), possibly the same as used to fit the
  normalization model in order to get the fitted data.}

  \item{factors}{column names in the pheno data slot
  describing the biological factors. Or a design matrix.}

  \item{lg}{logical indicating that the data should be log
  transformed}

  \item{predfunc}{the function to use to get predicted
  values from the fitted object (only for crmn)}

  \item{...}{passed on to \code{standardsPred},
  \code{standardsFit}, ode{standards}, \code{analytes}}
}
\value{
  the normalized data
}
\description{
  Predict the normalized data using a previously fitted
  normalization model.
}
\details{
  Apply fitted normalization parameters to new data to get
  normalized data. Current can not only handle matrices as
  input for methods 'RI' and 'one'.
}
\examples{
data(mix)
nfit <- normFit(mix, "crmn", factor="type", ncomp=3)
normedData <- normPred(nfit, mix, "type")
slplot(pca(t(log2(exprs(normedData)))), scol=as.integer(mix$type))
## same thing
Y <- exprs(mix)
G <- with(pData(mix), model.matrix(~-1+type))
isIS <- fData(mix)$tag == 'IS'
nfit <- normFit(Y, "crmn", factors=G, ncomp=3, standards=isIS)
normedData <- normPred(nfit, Y, G, standards=isIS)
slplot(pca(t(log2(normedData))), scol=as.integer(mix$type))
}
\author{
  Henning Redestig
}
\seealso{
  \code{normFit}
}

