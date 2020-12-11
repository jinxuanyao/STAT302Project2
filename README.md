  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/jinxuanyao/STAT302Project2.svg?branch=master)](https://travis-ci.com/jinxuanyao/STAT302Project2)
  [![Codecov test coverage](https://codecov.io/gh/jinxuanyao/STAT302Project2/branch/master/graph/badge.svg)](https://codecov.io/gh/jinxuanyao/STAT302Project2?branch=master)
  <!-- badges: end -->

## Use

The vignette demonstrates example usage of all main functions. Please [file an issue](https://github.com/jinxuanyao/STAT302Project2/issues) if you have a request for a tutorial that is not currently included. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("jinxuanyao/STAT302Project2", build_vignette = TRUE, build_opts = c())
library(STAT302Project2)
# Use this to view the vignette in the STAT302Project2 HTML help
help(package = "STAt302Project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302Project2")
```
