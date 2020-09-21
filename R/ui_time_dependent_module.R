timeDependentUI = function(id) {
  tabPanel(
    "Time depenent plots",
    wellPanel(
      style = paste0(
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
      hidden(
        selectInput(
          inputId = NS(id, "selected_states"),
          label = "Estados",
          choices = "",
          width = "40%",
          multiple = TRUE
        )
      ),
      plotOutput(NS(id, "plot"), height = "420px", width = "560px"),
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
