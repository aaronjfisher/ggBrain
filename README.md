ggBrain
=======

An R package of simple helper functions for creating brain image figures with ggplot. The primary workhorse function is `ggBrain`, which is documented in the vignette. Some of the images generated in the vignette are shown below.


### To install
```S
## if needed
install.packages("devtools")

## main package
library(devtools)
install_github('ggBrain','aaronjfisher',build_vignettes=TRUE)

## to access help pages
library(ggBrain)
help(package=ggBrain)
``` 


### Sample images from vignette

####Structural images
<img src="vignettes/figure/line-key-str.png" border="5" />

####Comparing seed maps across two subjects
<img src="vignettes/figure/2brain_compare.png" border="5" />

####Tri-planar cross-hairs
<img src="vignettes/figure/tri-panel2.png" border="5" />

####Basic, single panel figures
<img src="vignettes/figure/single-plots-abs-val.png" border="5" />




<br/><br/>