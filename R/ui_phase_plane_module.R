phasePlaneUI = function(id) {
  tabPanel(
    "Phase-plane analysis",
    wellPanel(
      style =  paste0(
        "height: 90vh; overflow-y: auto;",
        "background: transparent; border: transparent;",
        "box-shadow: none;"
      ),
      h3("Graficos"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = NS(id, "get_plot"),
              label = "Obtener grafico",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = NS(id, "dwnld_plot"),
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
              inputId = NS(id, "state1"),
              label = "Estado 1",
              choices = "",
              width = "100%"
            )
          ),
          fluidRow(
            column(
              width = 6,
              hidden(
                numericInput(
                  inputId = NS(id, "state1_min"),
                  label = "Limite inferior",
                  value = 0,
                  step = 1
                )
              )
            ),
            column(
              width = 6,
              hidden(
                numericInput(
                  inputId = NS(id, "state1_max"),
                  label = "Limite superior",
                  value = 20,
                  step = 1
                )
              )
            )
          )
        ),
        column(
          width = 6,
          hidden(
            selectInput(
              inputId = NS(id, "state2"),
              label = "Estado 2",
              choices = "",
              width = "100%"
            )
          ),
          fluidRow(
            column(
              width = 6,
              hidden(
                numericInput(
                  inputId = NS(id, "state2_min"),
                  label = "Limite inferior",
                  value = 0,
                  step = 1
                )
              )
            ),
            column(
              width = 6,
              hidden(
                numericInput(
                  inputId = NS(id, "state2_max"),
                  label = "Limite superior",
                  value = 20,
                  step = 1
                )
              )
            )
          )
        )
      ),

      imageOutput(NS(id, "plot"), width = "100%"),

      h3("Codigo"),
      fluidRow(
        column(
          width = 6,
          disabled(
            actionButton(
              inputId = NS(id, "get_code"),
              label = "Obtener codigo",
              width = "100%"
            )
          )
        ),
        column(
          width = 6,
          disabled(
            downloadButton(
              outputId = NS(id, "dwnld_code"),
              label = "Descargar codigo",
              style = "width:100%;"
            )
          )
        )
      ),
      hr(),
      verbatimTextOutput(NS(id, "code"))
    )
  )
}
