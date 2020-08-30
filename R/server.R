app_server = function(input, output, session) {

  session$onSessionEnded(shiny::stopApp)
  session$onSessionEnded(function() {
    dirs = isolate(store$temp_dirs)
    if (length(dirs) == 0) return(NULL)
    purrr::walk(dirs, delete_dir)
  })

  store = reactiveValues()
  store$ind_n = 500
  store$temp_dirs = c()

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

      purrr::walk(c(paste0("eq", seq_len(input$n_eq))), disable)
      purrr::walk(c("set_eqs", "n_eq"), hide)
      purrr::walk(c("unlock_eqs", "set_params"), enable)
      purrr::walk(c("paramsInputUI", "independent_min", "independent_max",
                    "set_params", "unlock_eqs"), show)
    })
  })

  observeEvent(input$unlock_eqs, {
    equation_ids = c(paste0("eq", seq_len(input$n_eq)))
    purrr::walk(c(equation_ids, "set_eqs"), enable)
    purrr::walk(c("unlock_eqs", "set_params", "unlock_params"), disable)
    purrr::walk(c("n_eq", "set_eqs"), show)
    purrr::walk(c("paramsInputUI", "independent_min", "independent_max",
                  "set_params", "unlock_eqs"), hide)
  })


  observeEvent(input$unlock_params, {
    param_inputs = paste0("param_", store$equation_components$params)
    state_inputs = paste0("state_", store$equation_components$state)
    purrr::walk(c("set_eqs", "unlock_eqs", "set_params",
                  "independent_min", "independent_max", param_inputs, state_inputs),
                enable)
    purrr::walk(c("unlock_params"), disable)
    purrr::walk(c("unlock_params"), hide)
    purrr::walk(c("set_params"), show)
    purrr::walk(c("get_graph_1", "dwnld_plot_panel_1", "get_code_1", "dwnld_code_panel_1"), disable)
    purrr::walk(c("get_graph_2", "dwnld_plot_panel_2", "get_code_2", "dwnld_code_panel_2"), disable)

    hide("plot_states_panel_1")
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

      shinybusy::show_spinner()
      if (input$independent_min >= input$independent_max) {
        stop("El limite inferior de la variable independiente debe ser menor al limite superior.",
             call. = FALSE)
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

      if (any(sapply(store$state_values, length) != 1)) {
        store$state_values = recycle(store$state_values)
        store$multiple_states = TRUE
      }

      param_inputs = paste0("param_", store$equation_components$params)
      state_inputs = paste0("state_", store$equation_components$state)
      purrr::walk(c("set_eqs", "unlock_eqs", "set_params",
                    "independent_min", "independent_max", param_inputs, state_inputs),
                  disable)
      purrr::walk(c("set_params"), hide)
      purrr::walk(c("unlock_params"), enable)
      purrr::walk(c("unlock_params"), show)

      # Primer panel
      purrr::walk(c("get_graph_1", "get_code_1", "dwnld_code_panel_1"), enable)

      # Segundo panel
      if (length(store$state_values) == 1) {
        purrr::walk(c("state1_panel_2", "state1_min", "state1_max"), show)
        updateSelectInput(
          session = session,
          inputId = "state1_panel_2",
          choices = store$equation_components$state
        )
      }

      if (length(store$state_values) > 1) {
        purrr::walk(c("state1_panel_2", "state1_min", "state1_max"), show)
        purrr::walk(c("state2_panel_2", "state2_min", "state2_max"), show)

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

      purrr::walk(c("get_graph_2", "get_code_2", "dwnld_code_panel_2"), enable)

      show("plot_states_panel_1")
      enable("dwnld_plot_panel_1")

      updateSelectInput(
        session = session,
        inputId = "plot_states_panel_1",
        choices = store$equation_components$state
      )
    })

    # Corro el solver!
    tryCatch({
      withCustomHandler({
        store$df_panel_1 = get_df_panel_1(
          store$equation_components,
          store$param_values,
          store$state_values,
          list(
            "interval" = c(input$independent_min, input$independent_max),
            "ind_n" = store$ind_n
          ),
          store$multiple_states
        )

        shinybusy::hide_spinner()
      })
    },
    warning = function(cnd) {
      shinypop::nx_notify_warning(
        paste("El solver informa un warning:", cnd$message)
      )
    })

    store$counter = make_counter()
    # Se crea un directorio temporal para cada combinacion de modelo/parametros/estados
    store$model_temp_dir = create_tempdir()
    store$temp_dirs = c(store$temp_dirs, store$model_temp_dir)
  })


  # Panel 1 -------------------------------------------------------------------------
  store$graph_panel_1 = eventReactive(input$get_graph_1, {
    withCustomHandler({
      if (length(input$plot_states_panel_1) == 0 || input$plot_states_panel_1 == "") {
        stop("Selecciona al menos un estado.", call. = FALSE)
      }
      get_graph_panel_1(store, input$plot_states_panel_1)
    })
  })

  output$plot_panel1 = renderPlot({
    store$graph_panel_1()
  })

  store$code_panel_1 = eventReactive(input$get_code_1, {
    withCustomHandler({
      req(store$param_values, store$state_values)

      if (length(input$plot_states_panel_1) == 0 || input$plot_states_panel_1 == "") {
        stop("Selecciona al menos un estado.", call. = FALSE)
      }

      get_code_panel_1(
        store$equation_components,
        store$param_values,
        store$state_values,
        input$plot_states_panel_1,
        list(
          "interval" = c(input$independent_min, input$independent_max),
          "ind_n" = store$ind_n
        ),
        store$multiple_states
      )
    })
  })

  output$code_panel_1 = renderText({
    store$code_panel_1()
  })

  output$dwnld_plot_panel_1 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$plot_states_panel_1, collapse = "_"),
        "_time_dependent_plot.png"
      )
    },
    content = function(file) {
      withCustomHandler({
        if (length(input$plot_states_panel_1) == 0 || input$plot_states_panel_1 == "") {
          stop("Selecciona al menos un estado.", call. = FALSE)
        }
        plt = get_graph_panel_1(store, input$plot_states_panel_1)
        ggsave(file, plt, device = "png", width = 9.33, height = 7)
      })
    }
  )

  output$dwnld_code_panel_1 = downloadHandler(
    filename = function() {
      "code_1.R"
    },
    content = function(file) {
      withCustomHandler({
        req(store$param_values, store$state_values)
        if (length(input$plot_states_panel_1) == 0 || input$plot_states_panel_1 == "") {
          stop("Selecciona al menos un estado.", call. = FALSE)
        }

        code = get_code_panel_1(
          store$equation_components,
          store$param_values,
          store$state_values,
          input$plot_states_panel_1,
          list(
            "interval" = c(input$independent_min, input$independent_max),
            "ind_n" = store$ind_n
          ),
          store$multiple_states
        )

        file_connection <- file(file)
        writeLines(
          code,
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
          value = process_states(input[[paste0("state_", input$state1_panel_2)]]),
          range = c(input$state1_min, input$state1_max)
        )
        list2 = NULL
      }
      if (length(store$state_values) > 1) {
        req(store$param_values, store$state_values,
            input$state1_panel_2, input$state2_panel_2)

        list1 = list(
          name = input$state1_panel_2,
          value = process_states(input[[paste0("state_", input$state1_panel_2)]]),
          range = c(input$state1_min, input$state1_max)
        )
        list2 = list(
          name = input$state2_panel_2,
          value = process_states(input[[paste0("state_", input$state2_panel_2)]]),
          range = c(input$state2_min, input$state2_max)
        )

        if (list1$name == list2$name) {
          stop("Debes elegir dos estados distintos.", call. = FALSE)
        }
      }

      get_code_panel_2(
        store$equation_components,
        store$param_values,
        list(
          "interval" = c(input$independent_min, input$independent_max),
          "ind_n" = store$ind_n
        ),
        state1_list = list1,
        state2_list = list2,
        return_type = 'all'
      )
    })
  })

  output$code_panel_2 = renderText({
    store$code_panel_2()
  })


  store$graph_panel_2_path = eventReactive(input$get_graph_2, {
    withCustomHandler({

      shinybusy::show_spinner()

      if (length(store$state_values) == 1) {
        req(store$param_values, store$state_values, input$state1_panel_2)
        list1 = list(
          name = input$state1_panel_2,
          value = process_states(input[[paste0("state_", input$state1_panel_2)]]),
          range = c(input$state1_min, input$state1_max)
        )
        list2 = NULL
      }
      if (length(store$state_values) > 1) {
        req(store$param_values, store$state_values,
            input$state1_panel_2, input$state2_panel_2)

        list1 = list(
          name = input$state1_panel_2,
          value = process_states(input[[paste0("state_", input$state1_panel_2)]]),
          range = c(input$state1_min, input$state1_max)
        )
        list2 = list(
          name = input$state2_panel_2,
          value = process_states(input[[paste0("state_", input$state2_panel_2)]]),
          range = c(input$state2_min, input$state2_max)
        )

        if (list1$name == list2$name) {
          stop("Debes elegir dos estados distintos.", call. = FALSE)
        }
      }

      settings_expr = get_code_panel_2(
        store$equation_components,
        store$param_values,
        list(
          "interval" = c(input$independent_min, input$independent_max),
          "ind_n" = store$ind_n
        ),
        state1_list = list1,
        state2_list = list2,
        return_type = 'settings'
      )

      plt_expr = get_code_panel_2(
        store$equation_components,
        store$param_values,
        list(
          "interval" = c(input$independent_min, input$independent_max),
          "ind_n" = store$ind_n
        ),
        state1_list = list1,
        state2_list = list2,
        return_type = 'plot'
      )

      n = store$counter()
      plt_name = paste0("plot_", n)

      create_and_save_plot(
        store$model_temp_dir,
        settings_expr,
        plt_expr,
        plt_name
      )
      # El grafico solo puede ser descaragdo una vez que fue generado.
      # Ademas, solo descarga el ultimo grafico generado.
      # Si generas un grafico para el estado X y luego cambias al estado Y,
      # si intentas descargar el grafico para Y sin haberlo creado, te va a descargar
      # el grafico para X (es decir, el que esta en pantalla al momento de
      # clickear en el boton de descarga).
      enable("dwnld_plot_panel_2")

      shinybusy::hide_spinner()
      return(normalizePath(file.path(store$model_temp_dir, paste0(plt_name, ".png"))))
    })
  })

  output$plot_panel2 = renderImage({
      filename = store$graph_panel_2_path()
      list(src = filename,
           contentType = "image/png",
           width = 560,
           height = 420
      )
  }, deleteFile = FALSE)


  output$dwnld_plot_panel_2 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$state1_panel_2, input$input$state2_panel_2, collapse = "_"),
        "_phase_plane_plot.png"
      )
    },

    content = function(file) {
      withCustomHandler({
        file.copy(store$graph_panel_2_path(), file)
      })
    }
  )

  output$dwnld_code_panel_2 = downloadHandler(
    filename = function() {
      "code_2.R"
    },
    content = function(file) {
      withCustomHandler({
        if (length(store$state_values) == 1) {
          req(store$param_values, store$state_values, input$state1_panel_2)
          list1 = list(
            name = input$state1_panel_2,
            value = input[[paste0("state_", input$state1_panel_2)]],
            range = c(input$state1_min, input$state1_max)
          )
          list2 = NULL
        }
        if (length(store$state_values) > 1) {
          req(store$param_values, store$state_values,
              input$state1_panel_2, input$state2_panel_2)

          list1 = list(
            name = input$state1_panel_2,
            value = input[[paste0("state_", input$state1_panel_2)]],
            range = c(input$state1_min, input$state1_max)
          )
          list2 = list(
            name = input$state2_panel_2,
            value = input[[paste0("state_", input$state2_panel_2)]],
            range = c(input$state2_min, input$state2_max)
          )
          if (list1$name == list2$name) {
            stop("Debes elegir dos estados distintos.", call. = FALSE)
          }
        }

        code = get_code_panel_2(
          store$equation_components,
          store$param_values,
          list(
            "interval" = c(input$independent_min, input$independent_max),
            "ind_n" = store$ind_n
          ),
          state1_list = list1,
          state2_list = list2,
          return_type = 'all'
        )

        file_connection <- file(file)
        writeLines(
          code,
          file_connection
        )
        close(file_connection)
      })
    }
  )

}


