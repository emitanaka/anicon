
anicon
======

Overview
--------

This R-package allows for easy insertion of animated [font awesome](https://fontawesome.com) or [academicons](https://jpswalsh.github.io/academicons/) icons into R markdown or Shiny.

It works with inline code such as <code>r <icon::fa>("pagelines")</code> enclosed with backticks. You can find the vignette [here](https://anicon.netlify.com/).

Installation
------------

You can install this package from github as:

``` r
# install.packages("devtools")
devtools::install_github('emitanaka/anicon')
```

Example
-------

![](https://raw.githubusercontent.com/emitanaka/anicon/master/inst/media/animate.gif)

`anicon` now also has text animation!

![](https://raw.githubusercontent.com/emitanaka/anicon/master/inst/media/anitextshow.gif)

See more in the vignette [here](https://anicon.netlify.com/).

Font awesome animation is brought thanks to [font-awesome-animation](https://l-lin.github.io/font-awesome-animation/).

Still icons
-----------

For still icons you can try either the [`fontawesome`](https://github.com/rstudio/fontawesome) package or [`icon`](https://github.com/ropenscilabs/icon) package .
