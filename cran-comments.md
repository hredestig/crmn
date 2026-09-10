## Test environments

* local macOS (Darwin 25.6.0), R 4.6.1, `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings* | 0-1 notes*

\* Two items appear only on this local machine and are not expected on
CRAN's build infrastructure:

  - `checking sizes of PDF files under 'inst/doc' ... WARNING`
    ('qpdf' not installed locally)
  - `checking HTML version of manual ... NOTE`
    (local 'tidy' binary too old to validate; 'V8' unavailable for math
    rendering)

One further NOTE may be borderline on CRAN's reference machine:

  - `checking examples ... NOTE` — the `normalize` example ran in ~5.4s
    locally (just over the 5s threshold); timing is machine-dependent.

## Downstream dependencies

None (checked via CRAN reverse dependency listing).
