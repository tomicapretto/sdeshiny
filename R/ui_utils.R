latexInput2 = function(inputId, label, hidden = FALSE) {
  if (hidden) {
    shinyjs::hidden(
      shiny::div(
        id = paste0(inputId, "_div"),
        shinymath::mathInput(
          inputId = inputId,
          label = label
        )
      )
    )
  } else {
    shiny::div(
      id = paste0(inputId, "_div"),
      shinymath::mathInput(
        inputId = inputId,
        label = label
      )
    )
  }
}

paramInput = function(param) {
  numericInput(
    inputId = paste0("param_", param),
    label = param,
    value = 1
  )
}

stateInput = function(state) {
  lang = getOption("sdeshiny.lang")
  textInput(
    inputId = paste0("state_", state),
    label = state,
    value = "",
    placeholder = LANG[[lang]][["state_placeholder"]]
  )
}
