ggBrain
========
[![Build Status](https://travis-ci.org/aaronjfisher/ggBrain.png?branch=master)](https://travis-ci.org/aaronjfisher/ggBrain)
  [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/aaronjfisher/ggBrain?branch=master&svg=true)](https://ci.appveyor.com/project/aaronjfisher/ggBrain)
  
`muschellij2` badges:
[![Build Status](https://travis-ci.org/muschellij2/ggBrain.png?branch=master)](https://travis-ci.org/muschellij2/ggBrain)
  [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/ggBrain?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/ggBrain)
  
An R package of simple helper functions for creating brain image figures with ggplot. The primary workhorse function is `ggBrain`, which is documented in the vignette. Some of the images generated in the vignette are shown below.


### To install
```r
## if needed
install.packages("devtools")

## main package
library(devtools)
install_github('aaronjfisher/ggBrain',build_vignettes=TRUE)

## to access help pages
library(ggBrain)
help(package=ggBrain)
``` 


### Sample images from vignette

#### Structural images
<img src="vignettes/figure/line-key-str.png" border="5" />

#### Comparing seed maps across two subjects
<img src="vignettes/figure/2brain_compare.png" border="5" />

#### Tri-planar cross-hairs
<img src="vignettes/figure/tri-panel2.png" border="5" />

#### Basic, single panel figures
<img src="vignettes/figure/single-plots-abs-val.png" border="5" />




<br/><br/>
