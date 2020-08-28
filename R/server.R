app_server = function(input, output, session) {

  session$onSessionEnded(shiny::stopApp)
  store = reactiveValues()
  store$ind_n = 500

  update_eq_n = update_eq_n_generator()

  observe({
    update_eq_n(input, output, session, input$n_eq)
  })

  observeEvent(input$set_eqs, {
    withCustomHandler({

      equation_ids = c(paste0("eq", seq_len(input$n_eq)))

      equation_ids = paste0(equation_ids, "_latex")
      equations = purrr::map(equation_ids, function(x) input[[x]])

      if (any(sapply(equations, is.null))) {
        stop("Hay al menos una ecuacion vacia.")
      }

      equation = as.character(equations)
      store$equation_components = process_equations(equations)

      updateSliderInput(
        session = session,
        inputId = "ind_var",
        label = store$equation_components$independent
      )

      purrr::walk(c(paste0("eq", seq_len(input$n_eq))), disable)
      purrr::walk(c("set_eqs", "n_eq"), hide)
      purrr::walk(c("unlock_eqs", "set_params"), enable)
      purrr::walk(c("paramsInputUI", "ind_var", "set_params", "unlock_eqs"), show)
    })
  })

  observeEvent(input$unlock_eqs, {
    equation_ids = c(paste0("eq", seq_len(input$n_eq)))
    purrr::walk(c(equation_ids, "set_eqs"), enable)
    purrr::walk(c("unlock_eqs", "set_params", "unlock_params"), disable)
    purrr::walk(c("n_eq", "set_eqs"), show)
    purrr::walk(c("paramsInputUI", "ind_var", "set_params", "unlock_params", "unlock_eqs"), hide)
  })



  observeEvent(input$unlock_params, {
    param_inputs = paste0("param_", store$equation_components$params)
    state_inputs = paste0("state_", store$equation_components$state)
    purrr::walk(c("unlock_eqs", "set_params", param_inputs, state_inputs), enable)
    purrr::walk(c("unlock_params"), disable)
    purrr::walk(c("unlock_params"), hide)
    purrr::walk(c("set_params"), show)

    purrr::walk(c("get_graph_1", "dwnld_plot_panel_1", "get_code_1", "dwnld_code_panel_1"), disable)
  })

  output$paramsInputUI = renderUI({
    req(store$equation_components)
    tagList(
      tags$h5(tags$strong("Estados inciales")),
      purrr::map(store$equation_components$state, stateInput),
      tags$h5(tags$strong("Parametros")),
      purrr::map(store$equation_components$params, paramInput),
      tags$h5(tags$strong("Variable independiente"))
    )
  })


  # Main panel ----------------------------------------------------------------------
  observeEvent(input$set_params, {
    withCustomHandler({
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

      if (any(sapply(store$state_values, length) != 1)) {
        store$state_values = recycle(store$state_values)
        store$multiple_states = TRUE
      }

      param_inputs = paste0("param_", store$equation_components$params)
      state_inputs = paste0("state_", store$equation_components$state)
      purrr::walk(c("set_eqs", "unlock_eqs", "set_params", param_inputs, state_inputs), disable)
      purrr::walk(c("set_params"), hide)
      purrr::walk(c("unlock_params"), enable)
      purrr::walk(c("unlock_params"), show)

      # Primer panel
      purrr::walk(c("get_graph_1", "get_code_1", "dwnld_code_panel_1"), enable)

      # Segundo panel
      if (length(store$state_values) == 1) {
        purrr::walk(c("state1_panel_2", "state1_range"), show)
        updateSelectInput(
          session = session,
          inputId = "state1_panel_2",
          choices = store$equation_components$state
        )
      }

      if (length(store$state_values) > 1) {
        purrr::walk(c("state1_panel_2", "state1_range", "state2_panel_2", "state2_range"), show)
        updateSelectInput(
          session = session,
          inputId = "state1_panel_2",
          choices = store$equation_components$state
        )
        updateSelectInput(
          session = session,
          inputId = "state2_panel_2",
          choices = store$equation_components$state
        )
      }

      purrr::walk(c("get_graph_2", "get_code_2"), enable)
    })
  })


  # Panel 1 -------------------------------------------------------------------------
  observeEvent(input$get_graph_1, {
    tryCatch({
      withCustomHandler({
        store$graphs_panel_1 = get_graphs_panel_1(
          store$equation_components,
          store$param_values,
          store$state_values,
          list("interval" = input$ind_var, "ind_n" = 500),
          store$multiple_states
        )
      })
      updateSelectInput(
        session = session,
        inputId = "select_plot_panel_1",
        choices = names(store$graphs_panel_1)
      )
      show("select_plot_panel_1")
      enable("dwnld_plot_panel_1")
    },
    warning = function(cnd) {
      shinypop::nx_notify_warning(
        paste("El solver informa un warning:", cnd$message)
      )
    })
  })

  output$plot_panel1 = renderPlot({
    store$graphs_panel_1[[input$select_plot_panel_1]]
  })

  store$code_panel_1 = eventReactive(input$get_code_1, {
    withCustomHandler({
      req(store$param_values, store$state_values)
      get_code_panel_1(
        store$equation_components,
        store$param_values,
        store$state_values,
        list("interval" = input$ind_var, "ind_n" = store$ind_n),
        store$multiple_states
      )
    })
  })

  output$code_panel_1 = renderText({
    store$code_panel_1()
  })


  output$dwnld_plot_panel_1 = downloadHandler(
    filename = function() {
      "time_dependent_plot.png"
    },
    content = function(file) {
      withCustomHandler({
        plt = store$graphs_panel_1[[input$select_plot_panel_1]]
        ggsave(file, plt, device = "png", width = 9.33, height = 7)
      })
    }
  )

  output$dwnld_code_panel_1 = downloadHandler(
    filename = function() {
      "code.R"
    },
    content = function(file) {
      withCustomHandler({
        file_connection <- file(file)
        writeLines(
          get_code_panel_1(
            store$equation_components,
            store$param_values,
            store$state_values,
            list("interval" = input$ind_var, "ind_n" = store$ind_n),
            store$multiple_states
          ),
          file_connection
        )
        close(file_connection)
      })
    }
  )

  # Panel 2 -------------------------------------------------------------------------

  store$code_panel_2 = eventReactive(input$get_code_2, {
    withCustomHandler({

      if (length(store$state_values) == 1) {
        req(store$param_values, store$state_values, input$state1_panel_2)
        list1 = list(
          name = input$state1_panel_2,
          value = input[[paste0("state_", input$state1_panel_2)]],
          range = input$state1_range
        )
        list2 = NULL
      }
      if (length(store$state_values) > 1) {
        req(store$param_values, store$state_values,
            input$state1_panel_2, input$state2_panel_2)

        list1 = list(
          name = input$state1_panel_2,
          value = input[[paste0("state_", input$state1_panel_2)]],
          range = input$state1_range
        )
        list2 = list(
          name = input$state2_panel_2,
          value = input[[paste0("state_", input$state2_panel_2)]],
          range = input$state2_range
        )
      }
      get_code_panel_2(
        store$equation_components,
        store$param_values,
        list("interval" = input$ind_var, "ind_n" = store$ind_n),
        state1_list = list1,
        state2_list = list2,
        return_type = 'all'
      )
    })
  })

  output$code_panel_2 = renderText({
    store$code_panel_2()
  })

}

