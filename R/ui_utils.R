latexInput2 = function(inputId, label, hidden = FALSE) {
  if (hidden) {
    shinyjs::hidden(
      shiny::div(
        id = paste0(inputId, "_div"),
        latex2r::latexInput(
          inputId = inputId,
          label = label
        )
      )
    )
  } else {
    shiny::div(
      id = paste0(inputId, "_div"),
      latex2r::latexInput(
        inputId = inputId,
        label = label
      )
    )
  }
}
