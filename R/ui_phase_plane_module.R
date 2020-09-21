phasePlaneUI = function(id) {
  tabPanel(
    "Phase-plane analysis",
    wellPanel(
      style =  paste0(
        "height: 90vh; overflow-y: auto;",
        "background: transparent; border: transparent;",
        "box-shadow: none;"
      ),
      uiOutput(NS(id, "graphics_header_ui")),
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
          uiOutput(NS(id, "dwnld_plot_ui"))
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
      uiOutput(NS(id, "code_header_ui")),
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
          uiOutput(NS(id, "dwnld_code_ui"))
        )
      ),
      hr(),
      verbatimTextOutput(NS(id, "code"))
    )
  )
}
