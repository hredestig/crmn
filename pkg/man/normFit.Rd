\name{normFit}
\alias{normFit}
\title{Fit a normalization model}
\usage{
  normFit(object, method, one = "Succinate_d4",
    factors = NULL, lg = TRUE, fitfunc = lm,
    formula = TRUE, ...)
}
\arguments{
  \item{object}{an \code{ExpressionSet} or a \code{matrix}
  (with samples as columns) in which case the
  \code{standards} must be passed on via \code{...}}

  \item{method}{chosen normalization method}

  \item{one}{single internal standard to use for
  normalization}

  \item{factors}{column names in the pheno data slot
  describing the biological factors. Or a design matrix
  directly.}

  \item{lg}{logical indicating that the data should be log
  transformed}

  \item{fitfunc}{the function that creates the model fit
  for normalization, must use the same interfaces as
  \code{lm}.}

  \item{formula}{if fitfunc has formula interface or not}

  \item{...}{passed on to \code{standardsFit},
  \code{standards}, {analytes}}
}
\value{
  a normalization model
}
\description{
  Fit the parameters for normalization of a metabolomics
  data set.
}
\details{
  Normalization is first done by fitting a model and then
  applying that model either to new data or the same data
  using \code{normPred}. Five different methods are
  implemented. \describe{ \item{t1}{divide by row-means of
  the \eqn{L_2} scaled internal standards}
  \item{one}{divide by value of a single, user defined,
  internal standard} \item{totL2}{divide by the square of
  sums of the full dataset} \item{nomis}{See Sysi-Aho et
  al.} \item{crmn}{See Redestig et al.} }
}
\examples{
data(mix)
nfit <- normFit(mix, "crmn", factors="type", ncomp=3)
slplot(sFit(nfit)$fit$pc, scol=as.integer(mix$runorder))
## same thing
Y <- exprs(mix)
G <- model.matrix(~-1+mix$type)
isIS <- fData(mix)$tag == 'IS'
nfit <- normFit(Y, "crmn", factors=G, ncomp=3, standards=isIS)
slplot(sFit(nfit)$fit$pc, scol=as.integer(mix$runorder))
}
\author{
  Henning Redestig
}
\references{
  Sysi-Aho, M.; Katajamaa, M.; Yetukuri, L. & Oresic, M.
  Normalization method for metabolomics data using optimal
  selection of multiple internal standards. BMC
  Bioinformatics, 2007, 8, 93

  Redestig, H.; Fukushima, A.; Stenlund, H.; Moritz, T.;
  Arita, M.; Saito, K. & Kusano, M.  Compensation for
  systematic cross-contribution improves normalization of
  mass spectrometry based metabolomics data Anal Chem,
  2009, 81, 7974-7980
}
\seealso{
  \code{normPred}, \code{standards}, \code{model.matrix}
}

