phasePlaneServer <- function(id, store) {
  moduleServer(id, function(input, output, session) {

    isolate({
      module_store = reactiveValues()
      store$enable_dwnld_plot_btn = FALSE
    })

    observeEvent(store$lang, {
      updateSelectInput(
        session = session,
        inputId = "state1",
        label = LANG[[store$lang]][["state1"]]
      )
      updateSelectInput(
        session = session,
        inputId = "state2",
        label = LANG[[store$lang]][["state2"]]
      )
      updateNumericInput(
        session = session,
        inputId = "state1_min",
        label = LANG[[store$lang]][["state1_min"]]
      )
      updateNumericInput(
        session = session,
        inputId = "state1_max",
        label = LANG[[store$lang]][["state1_max"]]
      )
      updateNumericInput(
        session = session,
        inputId = "state2_min",
        label = LANG[[store$lang]][["state2_min"]]
      )
      updateNumericInput(
        session = session,
        inputId = "state2_max",
        label = LANG[[store$lang]][["state2_max"]]
      )
      updateActionButton(
        session = session,
        inputId = "get_plot",
        label = LANG[[store$lang]][["get_plot"]]
      )
      updateActionButton(
        session = session,
        inputId = "get_code",
        label = LANG[[store$lang]][["get_code"]]
      )
    })

    output$graphics_header_ui = renderUI({
      h3(LANG_HEADERS[[store$lang]][["graphics"]])
    })

    output$code_header_ui = renderUI({
      h3(LANG_HEADERS[[store$lang]][["code"]])
    })

    output$dwnld_code_ui = renderUI({
      func = if (store$params_set) `(` else disabled
      func(
        downloadButton(
          outputId = NS(id, "dwnld_code"),
          label = LANG[[store$lang]][["dwnld_code"]],
          style = "width:100%;"
        )
      )
    })

    output$dwnld_plot_ui = renderUI({
      func = if (store$enable_dwnld_plot_btn) `(` else disabled
      func(
        downloadButton(
          outputId = NS(id, "dwnld_plot"),
          label = LANG[[store$lang]][["dwnld_plot"]],
          style = "width:100%;"
        )
      )
    })

    observeEvent(store$params_set, {
      if (store$params_set) {
        walk(c("state1", "state1_min", "state1_max"), show)

        updateSelectInput(
          session = session,
          inputId = "state1",
          choices = store$equation_components$state
        )

        if (length(store$state_values) > 1) {
          walk(c("state2", "state2_min", "state2_max"), show)
          updateSelectInput(
            session = session,
            inputId = "state2",
            choices = store$equation_components$state
          )
        }
        walk(c("get_plot", "get_code"), enable)
        module_store$counter = make_counter()
      } else {
        walk(c("get_plot", "get_code"), disable)
        walk(c("state1", "state1_min", "state1_max"), hide)
        walk(c("state2", "state2_min", "state2_max"), hide)
        store$enable_dwnld_plot_btn = FALSE
      }
    })

    module_store$code = eventReactive(input$get_code, {
      withCustomHandler({
        req(store$param_values, store$state_values, input$state1)

        list1 = list(
          name = input$state1,
          value = store$state_values[[input$state1]],
          range = c(input$state1_min, input$state1_max)
        )
        list2 = NULL

        if (length(store$state_values) > 1) {
          req(input$state2)
          list2 = list(
            name = input$state2,
            value = store$state_values[[input$state2]],
            range = c(input$state2_min, input$state2_max)
          )
          if (list1$name == list2$name) {
            stop(
              LANG_MSG[[store$lang]][["choose_two_different_states"]],
              call. = FALSE
            )
          }
        }
        get_code_pp(
          store$equation_components,
          store$param_values,
          store$independent,
          state1_list = list1,
          state2_list = list2,
          return_type = 'all'
        )
      })
    })

    output$code = renderText({
      module_store$code()
    })


    module_store$plot_path = eventReactive(input$get_plot, {
      withCustomHandler({
        req(store$param_values, store$state_values, input$state1)

        shinybusy::show_spinner()
        list1 = list(
          name = input$state1,
          value = store$state_values[[input$state1]],
          range = c(input$state1_min, input$state1_max)
        )
        list2 = NULL

        if (length(store$state_values) > 1) {
          req(input$state2)
          list2 = list(
            name = input$state2,
            value = store$state_values[[input$state2]],
            range = c(input$state2_min, input$state2_max)
          )
          if (list1$name == list2$name) {
            shinybusy::hide_spinner()
            stop(
              LANG_MSG[[store$lang]][["choose_two_different_states"]],
              call. = FALSE
            )
          }
        }

        settings_expr = get_code_pp(
          store$equation_components,
          store$param_values,
          store$independent,
          state1_list = list1,
          state2_list = list2,
          return_type = 'settings'
        )

        plt_expr = get_code_pp(
          store$equation_components,
          store$param_values,
          store$independent,
          state1_list = list1,
          state2_list = list2,
          return_type = 'plot'
        )

        n = module_store$counter()
        plt_name = paste0("plot_", n)
        create_and_save_plot(store$temp_dir, settings_expr, plt_expr, plt_name)

        # The plot can be downloaded only after it was generated.
        # The downloader can only download the last plot created.
        store$enable_dwnld_plot_btn = TRUE

        shinybusy::hide_spinner()
        return(normalizePath(file.path(store$temp_dir, paste0(plt_name, ".png"))))
      })
    })

    output$plot = renderImage({
      req(module_store$plot_path())
      filename = module_store$plot_path()
      list(
        src = filename,
        contentType = "image/png",
        width = 560,
        height = 420
      )
    }, deleteFile = FALSE)

    output$dwnld_plot = downloadHandler(
      filename = function() {
        if (length(store$state_values) > 1) {
          name = (paste0(input$state1, "_phase_plane.png"))
        } else {
          name = paste0(
            paste(input$state1, input$state2, collapse = "_"),
            "_phase_plane.png"
          )
        }
        return(name)
      },
      content = function(file) {
        withCustomHandler({
          file.copy(module_store$plot_path(), file)
        })
      }
    )

    output$dwnld_code = downloadHandler(
      filename = function() {
        "code_2.R"
      },
      content = function(file) {
        withCustomHandler({
          req(store$param_values, store$state_values, input$state1)

          list1 = list(
            name = input$state1,
            value = store$state_values[[input$state1]],
            range = c(input$state1_min, input$state1_max)
          )
          list2 = NULL

          if (length(store$state_values) > 1) {
            req(input$state2)
            list2 = list(
              name = input$state2,
              value = store$state_values[[input$state2]],
              range = c(input$state2_min, input$state2_max)
            )
            if (list1$name == list2$name) {
              stop(
                LANG_MSG[[store$lang]][["choose_two_different_states"]],
                call. = FALSE
              )
            }
          }

          code = get_code_pp(
            store$equation_components,
            store$param_values,
            store$independent,
            state1_list = list1,
            state2_list = list2,
            return_type = 'all'
          )

          file_connection = file(file)
          writeLines(code, file_connection)
          close(file_connection)
        })
      }
    )
  })
}
