
# GUIDELINES for authors of FLR documentation

The tutorials should walk the user step by step on how to perform a given analysis using one or more FLR packages, while giving the necessary background information on the methods and formulations employed in FLR.

## Requirements

Please install [knitr](https://cran.r-project.org/web/packages/knitr/index.html) and [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/index.html) to be able to `knit` the R Markdown documents in your machine.

You will need to call `rmarkdown::render`, as in

```r
library(rmarkdown)
render("TUTORIALNAME.Rmd")
```

or use the included Makefile.

## Writing style

A good set of guidelines in style can be found at the [OpenStack project](http://docs.openstack.org/contributor-guide/writing-style/general-writing-guidelines.html).

Mention of other R and FLR packages should include a link to the FLR website, as in

  [FLCore](http://www.flr-project.org/FLCore/)

Mention of FLR methoda and classes should use a link to the relevant html version of the help page, as in

  [bubbles](http://www.flr-project.org/FLCore/bubbles.html)

## Tutorial structure

A possible general structure would be as follows:

- Introduction, what the tutorial will cover, mention relevant fisheries concepts the user should be familiar with if necessary.

- Requirements, a lis of the required FLR packages, for example

The main body of the tutorial should consist oif combinations of these three types of sections:

- Concept, explains a functionality, e.g. The Ricker stock-recruit model in FLCore
- Task, steps to carry out some task, e.g. How to load a CSV file into an FLQuant
- Example - show a complete nalaysis using example data, e.g. Running FLXSA on ple4 and ple4.indices

The final section will contain:

- References, with url links if possible.
- More information, as in the template file. Please update author information and list all relecvant packages so that the version used is reported.

## Formatting conventions

- Tutorials are written in R Markdown, to be processed with `knitr`

- Default options for knitr chunks are to show both the command and the result. If output is too long, use some summary function or set `echo=FALSE` in that particular chunk.

# GETTING RECOGNITION

If the tutorial is based on existing material, we should list both old and new author(s), and maybe ask the previous author for help or feedback. 

We could get a DOI for each tutorial, from <https://guides.github.com/activities/citable-code/>

# FURTHER INFORMATION

- [knitr](http://yihui.name/knitr/)

- [Markdown](http://rmarkdown.rstudio.com/markdown_document_format.html)

