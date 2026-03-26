## condvis: Conditional Visualisation for Statistical Models

[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](https://interoperable-europe.ec.europa.eu/licence/gnu-general-public-license-v20-or-later)
[![CRAN](https://www.r-pkg.org/badges/version/condvis)](https://cran.r-project.org/package=condvis)
[![Downloads](https://cranlogs.r-pkg.org/badges/condvis?color=brightgreen)](https://www.r-pkg.org/pkg/condvis)

### Interactively exploring fitted models

Interactively take 2-D and 3-D sections in data space, showing where fitted
models intersect the section, and observed data near the section according to
a distance measure. See package [website](https://markajoc.github.io/condvis/)
for examples.

Works on Windows, Mac OS and Linux.

Requirements:
  * Windows: the standard graphics device is sufficient.
  * Mac OS: XQuartz device, [website](https://www.xquartz.org/)
  * Linux: X11, included in some distributions.

Installation:
```r
install.packages("condvis")
```

Example to get started:
```r
library(condvis)
data(mtcars)
m <- lm(mpg ~ wt + hp, data = mtcars)
ceplot(data = mtcars, model = m, sectionvars = "hp")
```
