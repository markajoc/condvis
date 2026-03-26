## condvis: Conditional Visualisation for Statistical Models

[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](https://www.r-pkg.org:443/pkg/condvis)](https://cran.r-project.org/package=condvis)
[![Downloads](http://cranlogs.r-pkg.org/badges/condvis?color=brightgreen)](https://www.r-pkg.org/pkg/condvis)

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
