app_sidebar = function() {
  sidebarPanel(
    sliderInput(
      inputId = "n_eq",
      label = "Cantidad de ecuaciones",
      value = 2,
      min = 1,
      max = 4,
      step = 1
    ),

    latexInput2(
      inputId = "eq1",
      label = "Ecuacion 1"
    ),

    latexInput2(
      inputId = "eq2",
      label = "Ecuacion 2"
    ),

    latexInput2(
      inputId = "eq3",
      label = "Ecuacion 3",
      hidden = TRUE
    ),

    latexInput2(
      inputId = "eq4",
      label = "Ecuacion 4",
      hidden = TRUE
    ),

    actionButton(
      inputId = "set_eqs",
      label = "Fijar ecuaciones",
      width = "100%",
      style = "padding-right:7.5px; padding-left:7.5px;"
    ),

    hidden(disabled(actionButton(
      inputId = "unlock_eqs",
      label = "Desbloquear ecuaciones",
      width = "100%",
      style = "padding-right:7.5px; padding-left:7.5px;"
    ))),


    uiOutput("paramsInputUI"),

    hidden(
      sliderInput(
        inputId = "ind_var",
        label = "",
        value = c(0, 20),
        min = 0,
        max = 100,
        step = 1,
        round = 1
      )
    ),

    hidden(actionButton(
      inputId = "set_params",
      label = "Fijar parametros",
      width = "100%",
      style = "padding-right:7.5px; padding-left:7.5px;"
    )),

    hidden(disabled(actionButton(
      inputId = "unlock_params",
      label = "Desbloquear parametros",
      width = "100%",
      style = "padding-right:7.5px; padding-left:7.5px;"
    )))
  )
}


# Notas:
# Los diferentes analisis se van a obtener en un tabbed box.
# La configuracion va a ser el ultimo menu del sidebar.
# Los parametros y las ecuaciones se van a mostrar dinamicamente en el sidebar.


