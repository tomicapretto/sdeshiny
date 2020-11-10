
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdeshiny

Para leer en otros lenguajes: [English](README.md).

Este paquete contiene una aplicacion escrita en
[Shiny](https://shiny.rstudio.com/) que funciona como interface para los
paquetes [deSolve](https://CRAN.R-project.org/package=deSolve) y
[phaseR](https://CRAN.R-project.org/package=phaseR). Esta aplicacion fue
desarrollada como trabajo final para el curso de doctorado Modelos
Matematicos Continuos dictado en Febrero del 2020 en Rosario por [Marco
Scavino](https://scholar.google.com/citations?user=woT0slUAAAAJ).

El objetivo de esta aplicacion es facilitar el analisis de sistemas de
ecuaciones diferenciales sin necesidad de que el usuario sepa escribir
codigo en R o utilizar los paquetes deSolve/phaseR.

La principal caracteristica de esta aplicacion es que el usuario puede
escribir cualquier sistema de ecuaciones diferenciales autonomas
utilizando `mathInput()` del paquete
[`shinymath`](https://github.com/tomicapretto/shinymath) y no esta
restringido a una cantidad de variables o parametros. Internamente, la
representacion en LaTeX que devuelve el campo de entrada basado en
Mathquill es convertida a R mediante el paquete
[latex2r](https://github.com/tomicapretto/latex2r).

Esta aplicacion reconoce automaticamente estados, parametros y variable
independiente del sistema para que luego el usuario indique valores para
los mismos.

El usuario no solo puede analizar un sistema de ecuaciones diferenciales
mediante graficos de estados vs tiempo o graficos en el plano de fases,
sino que tambien puede descargar el codigo necesario para reproducir el
analisis en otro momento. Por lo tanto, esta aplicacion no solo facilita
el analisis, sino que tambien sirve como punto de partida para quien
quiera utilizar los paquetes deSolve/phaseR.

## Instalacion

Este paquete aun no fue publicado en CRAN y es muy poco probable que eso
suceda. La version en desarrollo se puede instalar desde
[GitHub](https://github.com/) mediante:

``` r
# install.packages("devtools")
devtools::install_github("tomicapretto/sdeshiny")
```

## Corriendo la app

Para correr la aplicacion solo hace falta llamar a la funcion
`launch_app()`

``` r
sdeshiny::launch_app()
```

## Notas

Es importante resaltar algunas caractersiticas y limitaciones del
sistema.

### `latex2r`

Esta aplicacion hereda todas las limitaciones y caracteristicas del
parser implementado en `latex2r`. Es recomendable echarle un vistazo a
[estas notas](https://github.com/tomicapretto/latex2r#supported-latex)
en `latex2r` antes de utilizar la app.

### Notacion de Leibniz

Las ecuaciones diferenciales deben ser indicadas utilizando la notacion
de Leibniz.  
Bien:

  - `dX/dt = -\lambda * X`

Mal:

`X' = -\lambda * X`

## Video introductorio

  - [Como usar sdeshiny](https://www.youtube.com/watch?v=CZP9TaTwRlI)
