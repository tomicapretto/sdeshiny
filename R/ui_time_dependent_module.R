timeDependentUI = function(id) {
  tabPanel(
    "Time depenent plots",
    wellPanel(
      style = paste0(
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
              label = "Obtener graficos",
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
