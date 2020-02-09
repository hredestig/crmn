# Cross-contribution Robust Multiple standard Normalization

## Dependencies
The R-packages pcaMethods and Biobase must be installed and
loadable. This package was developed and tested on R v2.10.0 but will
probably work with many older versions as well.

## Installation
For Windows, start R and select the Packages menu, then "Install
package from local zip file". Find and highlight the location of the
zip file and click on "open".

For Linux/Unix, use the usual command "R CMD INSTALL" or use
the command install.packages from within an R session.

## Help
Start R and issue::

```
  library(crmn)
  openVignette("crmn")
```

for a tutorial and brief description of the package. Use the normal
R-help for details/examples on the individual functions.

If you have questions/comments/bug-reports/beers you are very welcome
to send them to the maintainer of the package.

## Licence
This package is free open source software, see file `COPYING` for
details. In brief, you may do with as you please as long as you
provide proper citations and publish all derivative work under the
same licence.

For citation, see `citation("crmn")`

>  Redestig, H.; Fukushima, A.; Stenlund, H.; Moritz, T.; Arita, M.;
>  Saito, K. & Kusano, M. Compensation for systematic cross-contribution
>  improves normalization of mass spectrometry based metabolomics data
>  Anal Chem, 2009, 81, 7974-7980
