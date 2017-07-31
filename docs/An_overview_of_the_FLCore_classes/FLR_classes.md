An overview of the FLCore classes
================
06 March, 2016

Lorem ipsum dolor sit amet, debitis dissentiunt mel at, porro nemore sea no, eos an albucius definiebas. Vis minim graece aliquam ne, nam in prompta discere appareat. Ad qui enim assentior, ei dicam quando accusata eam, ea cum delenit nusquam. Mei ei noster eripuit pertinacia, no nec simul delicatissimi, nec eu deleniti accusata. Tibique necessitatibus cu qui, semper suscipiantur eam id, ne nam platonem qualisque.

Required packages
-----------------

To follow this tutorial you should have installed the following packages:

-   FLR: [FLCore](http://www.flr-project.org/FLCore/)

You can do so as follows,

``` {.r}
install.packages(c("FLCore"), repos="http://flr-project.org/R")
```

``` {.r}
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
```

SECTION
=======

Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?

``` {.r}
# This is an example chunk for a figure
plot(FLQuant(rnorm(200), dim=c(2,20)))
```

<img src="FLR_classes_files/figure-markdown_github/figA-1.png" title="" alt="" style="display: block; margin: auto;" />

SubSECTION
----------

References
==========

More information
================

-   You can submit bug reports, questions or suggestions on this tutorial at <https://github.com/flr/doc/issues>.
-   Or send a pull request to <https://github.com/flr/doc/>
-   For more information on the FLR Project for Quantitative Fisheries Science in R, visit the FLR webpage, <http://flr-project.org>.

Software Versions
-----------------

-   R version 3.2.3 (2015-12-10)
-   FLCore: 2.5.20160120.1
-   **Compiled**: Sun Mar 6 18:38:29 2016
-   **Git Hash**: 828d0ba

License
-------

This document is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-sa/4.0) license.

Author information
------------------

**Iago MOSQUEIRA**. European Commission Joint Research Centre (JRC), Institute for the Protection and Security of the Citizen (IPSC), Maritime Affairs Unit, Via E. Fermi 2749, 21027 Ispra VA, Italy. <https://ec.europa.eu/jrc/>