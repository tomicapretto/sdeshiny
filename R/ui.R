app_sidebar = function() {
  tagList(
    fluidRow(
      column(
        width = 2,
        offset = 10,
        selectInput(
          inputId = "language",
          label = "Lenguaje",
          choices = c("ES", "EN"),
          selected = "ES",
          width = "95%",
        ),
        div(style = "margin-top:-20px")
      )
    ),
    sidebarPanel(
      sliderInput(
        inputId = "equation_n",
        label = "Cantidad de ecuaciones",
        value = 2,
        min = 1,
        max = 4,
        step = 1
      ),
      latexInput2(
        inputId = "eq1",
        label = "Eq 1"
      ),
      latexInput2(
        inputId = "eq2",
        label = "Eq 2"
      ),
      latexInput2(
        inputId = "eq3",
        label = "Eq 3",
        hidden = TRUE
      ),
      latexInput2(
        inputId = "eq4",
        label = "Eq 4",
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

      fluidRow(
        column(
          width = 6,
          hidden(
            numericInput(
              inputId = "independent_min",
              label = "",
              value = 0,
              step = 1
            )
          )
        ),
        column(
          width = 6,
          hidden(
            numericInput(
              inputId = "independent_max",
              label = "",
              value = 20,
              step = 1
            )
          )
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
  )
}

app_mainpanel = function() {
  mainPanel(
    tabsetPanel(
      id = "tabs",
      timeDependentUI("td"),
      phasePlaneUI("pp")
    )
  )
}

app_ui = function() {
  fluidPage(
    useShinyjs(),
    shinybusy::use_busy_spinner(spin = "fading-circle"),
    shinypop::use_notiflix_notify(position = "right-bottom", timeout = 8000),
    tags$head(tags$style(HTML('.form-group {margin-bottom: 10px;}'))),
    sidebarLayout(
      app_sidebar(),
      app_mainpanel()
    )
  )
}
