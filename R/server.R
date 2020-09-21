app_server = function(input, output, session) {

  session$onSessionEnded(shiny::stopApp)
  session$onSessionEnded(function() {
    dirs = isolate(store$temp_dirs)
    if (length(dirs) == 0) return(NULL)
    walk(dirs, delete_dir)
  })

  store = reactiveValues()
  store$independent = list(n = 500, min = 0, max = 20)
  store$temp_dirs = c()

  # This are shared variables that are accessed by other modules
  # to trigger actions.
  store$params_set = FALSE

  observeEvent(input$language, {
    options("sdeshiny.lang" = input$language)
  })

  observeEvent(input$language, {
    store$lang = input$language
    updateSelectInput(
      session = session,
      inputId = "language",
      label = LANG[[store$lang]][["language"]]
    )
    updateSliderInput(
      session = session,
      inputId = "equation_n",
      label = LANG[[store$lang]][["equation_n"]]
    )
    updateActionButton(
      session = session,
      inputId = "set_eqs",
      label = LANG[[store$lang]][["set_eqs"]]
    )
    updateActionButton(
      session = session,
      inputId = "unlock_eqs",
      label = LANG[[store$lang]][["unlock_eqs"]]
    )
    updateActionButton(
      session = session,
      inputId = "set_params",
      label = LANG[[store$lang]][["set_params"]]
    )
    updateActionButton(
      session = session,
      inputId = "unlock_params",
      label = LANG[[store$lang]][["unlock_params"]]
    )

    req(store$equation_components$state)
    for (state in store$equation_components$state) {
      updateTextInput(
        session = session,
        inputId = paste0("state_", state),
        placeholder = LANG[[store$lang]][["state_placeholder"]]
      )
    }
  })

  update_equation_n = update_equation_n_gen()

  observe({
    update_equation_n(input, output, session, input$equation_n)
  })

  observeEvent(input$set_eqs, {
    withCustomHandler({
      equation_ids = paste0(c(paste0("eq", seq_len(input$equation_n))), "_latex")
      equations = purrr::map(equation_ids, function(x) input[[x]])

      if (any(sapply(equations, is.null))) {
        stop(LANG_MSG[[store$lang]][["empty_equation"]])
      }

      equation = as.character(equations)
      store$equation_components = process_equations(equations)

      updateNumericInput(
        session = session,
        inputId = "independent_min",
        label = paste(store$equation_components$independent, "min.")
      )
      updateNumericInput(
        session = session,
        inputId = "independent_max",
        label = paste(store$equation_components$independent, "max.")
      )

      walk(c(paste0("eq", seq_len(input$equation_n))), disable)
      walk(c("set_eqs", "equation_n"), hide)
      walk(c("unlock_eqs", "set_params"), enable)
      walk(c("paramsInputUI", "independent_min", "independent_max", "set_params", "unlock_eqs"), show)
    })
  })

  observeEvent(input$unlock_eqs, {
    equation_ids = c(paste0("eq", seq_len(input$equation_n)))
    walk(c(equation_ids, "set_eqs"), enable)
    walk(c("unlock_eqs", "set_params", "unlock_params"), disable)
    walk(c("equation_n", "set_eqs"), show)
    walk(c("paramsInputUI", "independent_min", "independent_max", "set_params", "unlock_eqs"), hide)
  })

  observeEvent(input$set_params, {
    withCustomHandler({
      if (input$independent_min >= input$independent_max) {
        stop(LANG_MSG[[store$lang]][["bad_limits1"]], call. = FALSE)
      }

      store$multiple_states = FALSE
      store$param_values = NULL
      store$state_values = NULL

      store$param_values = purrr::map_dbl(
        paste0("param_", store$equation_components$params),
        function(x) process_param(input[[x]])
      )

      store$state_values = purrr::map(
        paste0("state_", store$equation_components$state),
        function(x) process_states(input[[x]])
      )

      names(store$state_values) = store$equation_components$state

      if (any(sapply(store$state_values, length) != 1)) {
        store$state_values = recycle(store$state_values)
        store$multiple_states = TRUE
      }

      param_inputs = paste0("param_", store$equation_components$params)
      state_inputs = paste0("state_", store$equation_components$state)

      store$independent$min = input$independent_min
      store$independent$max = input$independent_max

      walk(c("set_eqs", "unlock_eqs", "set_params",
             "independent_min", "independent_max", param_inputs, state_inputs),
           disable)
      walk("set_params", hide)
      walk("unlock_params", enable)
      walk("unlock_params", show)

      store$params_set = TRUE

      # It creates a tempdir for each combination of model/parameters/states.
      # In other words, every time we click on set params, a new temp dir is created.
      # They are all deleted when leaving the app.
      store$temp_dir = create_tempdir()
      store$temp_dirs = c(store$temp_dirs, store$temp_dir)
    })
  })

  observeEvent(input$unlock_params, {
    param_inputs = paste0("param_", store$equation_components$params)
    state_inputs = paste0("state_", store$equation_components$state)
    walk(c("set_eqs", "unlock_eqs", "set_params",
           "independent_min", "independent_max", param_inputs, state_inputs),
                enable)
    walk(c("unlock_params"), disable)
    walk(c("unlock_params"), hide)
    walk(c("set_params"), show)
    store$params_set = FALSE
  })

  output$paramsInputUI = renderUI({
    req(store$equation_components)
    tagList(
      tags$h5(tags$strong(LANG_HEADERS[[store$lang]][["initial_states"]])),
      purrr::map(store$equation_components$state, stateInput),
      tags$h5(tags$strong(LANG_HEADERS[[store$lang]][["parameters"]])),
      purrr::map(store$equation_components$params, paramInput),
      tags$h5(tags$strong(LANG_HEADERS[[store$lang]][["ind_var"]]))
    )
  })

  # Panel 1
  timeDependentSever("td", store)

  # Panel 2
  phasePlaneServer("pp", store)

}


