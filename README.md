## condvis: Conditional Visualisation for Statistical Models  

[![BuildStatus](https://travis-ci.org/markajoc/condvis.svg?branch=devel)](https://travis-ci.org/markajoc/condvis)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/condvis)](http://cran.r-project.org/package=condvis)
[![Downloads](http://cranlogs.r-pkg.org/badges/condvis?color=brightgreen)](http://www.r-pkg.org/pkg/condvis)  

### Interactively exploring fitted model structures

Interactively take 2-D and 3-D sections in data space, showing where fitted  
models intersect the section, and observed data near the section according to  
a distance measure.

Works on Windows, Mac OS and Linux.

Requirements:  
  * Windows: the standard graphics device is sufficient.  
  * Mac OS: XQuartz device, [website](http://www.xquartz.org/)  
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
