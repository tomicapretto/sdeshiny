
# sdeshiny

Read this in other languages: [Español](README.es.md)

This package contains a [Shiny](https://shiny.rstudio.com/) application
that works as an interface to the packages
[deSolve](https://CRAN.R-project.org/package=deSolve) and
[phaseR](https://CRAN.R-project.org/package=phaseR). This app was
developed as a final proejct for a course on mathematical modelling
taught by [Marco
Scavino](https://scholar.google.com/citations?user=woT0slUAAAAJ) on
February 2020 in Rosario.

The goal of this app is to make it easier to solve system of
differential equations without having to write R code or knowing how to
use packages deSolve/phaseR.

One of the nice features of this app is that the user can write
autonomous system of differential equations using a `mathInput()` from
the package [shinymath](https://github.com/tomicapretto/shinymath) with
arbitrary state and parameter names. Under the hood, the LaTeX
representation is translated to R code via
[latex2r](https://github.com/tomicapretto/latex2r).

The app automatically recognizes states, parameters, and the independent
variable of the system. Then, the app shows the inputs that correspond
to these components. What’s more, the user not only gets graphics to
analyze the ODE system but also the R code required to reproduce the
analysis. Thus, this app also serves as a starting point to those who
don’t know how to use packages deSolve and phaseR.

Finally, the app is now multi-lingual. It supports both Spanish and
English.

## Installation

This package is not published on CRAN and it is not likely to happen in
the near future. The development version can be installed from
[GitHub](https://github.com/) via:

``` r
# install.packages("devtools")
devtools::install_github("tomicapretto/sdeshiny")
```

## Running the app

You just need to call `launch_app()`

``` r
sdeshiny::launch_app()
```

## Notes

It is important to remark some characteristis and limitations of the
system.

### `latex2r`

This app comes with all the limitations and characteristics from the
parser implemented in `latex2r`. Please have a look at [these
notes](https://github.com/tomicapretto/latex2r#supported-latex) before
using the app.

### Leibniz notation

The differential equations must be expressed using Leibniz notation.

Good:

  - `dX/dt = -\lambda * X`

Bad:

`X' = -\lambda * X`

## Introductory video (in Spanish)

  - [Como usar sdeshiny](https://www.youtube.com/watch?v=CZP9TaTwRlI)
