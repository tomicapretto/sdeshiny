app_mainpanel = function() {
  mainPanel(
    tabsetPanel(
      id = "tabs",
      panel1(),
      panel2()
    )
  )
}

panel1 = function() {
  tabPanel(
    "Time depenent plots",
    wellPanel(
      style = "height: 90vh; overflow-y: auto;
              background: transparent; border: transparent; box-shadow: none;",
      h3("Graficos"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = "get_graph_1",
              label = "Obtener graficos",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = "dwnld_plot_panel_1",
              label = "Descargar grafico",
              style = "width:100%;"
            )
          )
        )
      ),
      hr(),
      hidden(
        selectInput(
          inputId = "select_plot_panel_1",
          label = "Selecciona un estado",
          choices = "",
          width = "40%"
        )
      ),

      plotOutput("plot_panel1", height = "420px", width = "560px"),

      h3("Codigo"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = "get_code_1",
              label = "Obtener codigo",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = "dwnld_code_panel_1",
              label = "Descargar codigo",
              style = "width:100%;"
            )
          )
        )
      ),
      hr(),
      verbatimTextOutput("code_panel_1")
    )

  )
}

panel2 = function() {
  tabPanel(
    "Phase-plane analysis",
    wellPanel(
      style = "height: 90vh; overflow-y: auto;
              background: transparent; border: transparent; box-shadow: none;",
      h3("Graficos"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = "get_graph_2",
              label = "Obtener grafico",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = "dwnld_plot_panel_2",
              label = "Descargar grafico",
              style = "width:100%;"
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          hidden(
              selectInput(
                inputId = "state1_panel_2",
                label = "Estado 1",
                choices = "",
                width = "100%"
              )
            ),
            hidden(
              sliderInput(
                inputId = "state1_range",
                label = "Rango del estado 1",
                min = -100, max = 100, value = c(0, 10)
              )
            )
          ),
        column(
          width = 6,
          hidden(
            selectInput(
              inputId = "state2_panel_2",
              label = "Estado 2",
              choices = "",
              width = "100%"
            )
          ),
          hidden(
            sliderInput(
              inputId = "state2_range",
              label = "Rango del estado 2",
              min = -100, max = 100, value = c(0, 10)
            )
          )
        )
      ),

      plotOutput("plot_panel2", height = "420px", width = "560px"),

      h3("Codigo"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = "get_code_2",
              label = "Obtener codigo",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = "dwnld_code_panel2",
              label = "Descargar codigo",
              style = "width:100%;"
            )
          )
        )
      ),
      hr(),
      verbatimTextOutput("code_panel_2")
    )
  )
}
