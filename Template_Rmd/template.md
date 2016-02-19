TITLE
================
NAME (INSTITUTION) <email@place.country>
19 February, 2016

Lorem ipsum dolor sit amet, debitis dissentiunt mel at, porro nemore sea no, eos an albucius definiebas. Vis minim graece aliquam ne, nam in prompta discere appareat. Ad qui enim assentior, ei dicam quando accusata eam, ea cum delenit nusquam. Mei ei noster eripuit pertinacia, no nec simul delicatissimi, nec eu deleniti accusata. Tibique necessitatibus cu qui, semper suscipiantur eam id, ne nam platonem qualisque.

Required packages
-----------------

To follow this tutorial you should have installed the following packages:

-   CRAN: [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
-   FLR: [FLCore](http://www.flr-project.org/FLCore/), [ggplotFL](http://www.flr-project.org/ggplotFL/)

You can do so as follows,

``` r
install.packages(c("ggplot2"))
install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
```

``` r
# This is an example chunk for a figure
plot(FLQuant(rnorm(200), dim=c(10,20)))
```

<img src="template_files/figure-markdown_github/figA-1.png" title="" alt="" style="display: block; margin: auto;" />

References
==========

<https://creativecommons.org/licenses/by-sa/4.0>

More information
================

-   You can submit bug reports, questions or suggestions on this tutorial at <https://github.com/flr/doc/issues>.
-   Or send a pull request to <https://github.com/flr/doc/>
-   For more information on the FLR Project for Quantitative Fisheries Science in R, visit the FLR webpage, <http://flr-project.org>.

Software Versions
-----------------

-   R version 3.2.3 (2015-12-10)
-   FLCore: 2.5.20160107
-   **Compiled**: Fri Feb 19 10:20:38 2016
-   **Git Hash**: 61d8c24

Author information
------------------

**Iago MOSQUEIRA**. European Commission Joint Research Centre (JRC), Institute for the Protection and Security of the Citizen (IPSC), Maritime Affairs Unit, Via E. Fermi 2749, 21027 Ispra VA, Italy. <https://ec.europa.eu/jrc/>
