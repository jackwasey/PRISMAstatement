<!-- README.md is generated from README.Rmd. Please edit that file, then:
rmarkdown::render("README.Rmd")
-->

# PRISMAstatement

[![CRAN
status](https://www.r-pkg.org/badges/version/PRISMAstatement)](https://cran.r-project.org/package=PRISMAstatement)
[![Travis build
status](https://travis-ci.org/jackwasey/PRISMAstatement.svg?branch=master)](https://travis-ci.org/jackwasey/PRISMAstatement)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jackwasey/PRISMAstatement?branch=master&svg=true)](https://ci.appveyor.com/project/jackwasey/PRISMAstatement)
[![Coverage
status](https://codecov.io/gh/jackwasey/PRISMAstatement/branch/master/graph/badge.svg)](https://codecov.io/github/jackwasey/PRISMAstatement?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)

PRISMAstatement is a simple package which does one thing: generate
publication quality flow charts of study inclusions and exclusions
following recommendations of the PRISMA statement. See the vignettes
(`vignette("PRISMA")`, `vignette("exclusionflowcharts")`) for more
details.

## Installation

You can install the released version of PRISMAstatement from
[CRAN](https://CRAN.R-project.org/package=PRISMAstatement) with:

``` r
install.packages("PRISMAstatement")
```

## Example

``` r
library(PRISMAstatement)
prisma(found = 750,
       found_other = 123,
       no_dupes = 776, 
       screened = 776, 
       screen_exclusions = 13, 
       full_text = 763,
       full_text_exclusions = 17, 
       qualitative = 746, 
       quantitative = 319)
```

<!--html_preserve-->

<div id="htmlwidget-e9627b9d67ff96545c30" class="grViz html-widget" style="width:100%;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-e9627b9d67ff96545c30">{"x":{"diagram":"digraph prisma {\n    node [shape=\"box\", fontsize = 10];\n    graph [splines=ortho, nodesep=1, dpi = 72]\n    a -> nodups;\n    b -> nodups;\n    a [label=\"Records identified through\ndatabase searching\n(n = 750)\"];\n    b [label=\"Additional records identified\nthrough other sources\n(n = 123)\"]\n    nodups -> incex;\n    nodups [label=\"Records after duplicates removed\n(n = 776)\"];\n    incex -> {ex; ft}\n    incex [label=\"Records screened\n(n = 776)\"];\n    ex [label=\"Records excluded\n(n = 13)\"];\n    {rank=same; incex ex}\n    ft -> {qual; ftex};\n    ft [label=\"Full-text articles assessed\nfor eligibility\n(n = 763)\"];\n    {rank=same; ft ftex}\n    ftex [label=\"Full-text articles excluded,\nwith reasons\n(n = 17)\"];\n    qual -> quant\n    qual [label=\"Studies included in qualitative synthesis\n(n = 746)\"];\n    quant [label=\"Studies included in\nquantitative synthesis\n(meta-analysis)\n(n = 319)\"];\n  }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
