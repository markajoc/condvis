condvis: Conditional Visualisation for Statistical Models
=========================================================

Interactively exploring fitted model structures
-----------------------------------------------

Interactively take 2-D and 3-D sections in data space, showing where fitted  
models intersect the section, and observed data near the section according to  
a distance measure.

Works on Windows, Mac OS and Linux.

Requirements:  
  * Windows: the standard graphics device is sufficient.  
  * Mac OS: Xquartz device, [website](http://www.xquartz.org/)  
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
ceplot(data = mtcars, model = m, S = "hp", type = "default")
```
