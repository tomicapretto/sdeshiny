timeDependentSever <- function(id, store) {
  moduleServer(id, function(input, output, session) {

    isolate({
      module_store = reactiveValues()
    })

    observeEvent(store$lang, {
      lang = store$lang
      updateActionButton(
        session = session,
        inputId = "get_plot",
        label = LANG[[lang]][["get_plot"]]
      )
      updateActionButton(
        session = session,
        inputId = "get_code",
        label = LANG[[lang]][["get_code"]]
      )
      updateSelectInput(
        session = session,
        inputId = "selected_states",
        label = LANG[[lang]][["selected_states"]]
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
      func = if (store$params_set) `(` else disabled
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
        tryCatch({
          shinybusy::show_spinner()
          withCustomHandler({
            module_store$df = get_df_td(
              store$equation_components,
              store$param_values,
              store$state_values,
              store$independent,
              store$multiple_states
            )
          })
          shinybusy::hide_spinner()
        },
        warning = function(cnd) {
          shinypop::nx_notify_warning(
            paste(LANG_MSG[[store$lang]][["solver_warning"]], cnd$message)
          )
          req(FALSE)
        },
        finally = function() {
          shinybusy::hide_spinner()
        })

        walk(c("get_plot", "get_code"), enable)
        show("selected_states")
        updateSelectInput(
          session = session,
          inputId = "selected_states",
          choices = store$equation_components$state
        )
      } else {
        walk(c("get_plot", "get_code"), disable)
        hide("selected_states")
      }
    })

    module_store$plot = eventReactive(input$get_plot, {
      withCustomHandler({
        selected_states = input$selected_states
        if (length(selected_states) == 0 || selected_states == "") {
          stop(
            LANG_MSG[[store$lang]][["choose_at_least_one_state"]],
            call. = FALSE
          )
        }
        get_plot_td(
          df = module_store$df,
          independent = store$equation_components$independent,
          selected_states = input$selected_states,
          multiple_states = store$multiple_states
        )
      })
    })

    output$plot = renderPlot({
      module_store$plot()
    })

    module_store$code = eventReactive(input$get_code, {
      withCustomHandler({
        req(store$param_values, store$state_values)
        selected_states = input$selected_states
        if (length(selected_states) == 0 || selected_states == "") {
          stop(
            LANG_MSG[[store$lang]][["choose_at_least_one_state"]],
            call. = FALSE
          )
        }
        get_code_td(
          store$equation_components,
          store$param_values,
          store$state_values,
          selected_states,
          store$independent,
          store$multiple_states
        )
      })
    })

    output$code = renderText({
      module_store$code()
    })

    output$dwnld_plot = downloadHandler(
      filename = function() {
        paste0(
          paste(input$selected_states, collapse = "_"),
          "_vs_", store$equation_components$independent, ".png"
        )
      },
      content = function(file) {
        withCustomHandler({
          selected_states = input$selected_states
          if (length(selected_states) == 0 || selected_states == "") {
            stop(
              LANG_MSG[[store$lang]][["choose_at_least_one_state"]],
              call. = FALSE
            )
          }
          plt = get_plot_td(
            df = module_store$df,
            independent = store$equation_components$independent,
            selected_states = input$selected_states,
            multiple_states = store$multiple_states
          )
          ggsave(file, plt, device = "png", width = 9.33, height = 7)
        })
      }
    )

    output$dwnld_code = downloadHandler(
      filename = function() {
        "code_1.R"
      },
      content = function(file) {
        withCustomHandler({
          req(store$param_values, store$state_values)

          selected_states = input$selected_states
          if (length(selected_states) == 0 || selected_states == "") {
            stop(
              LANG_MSG[[store$lang]][["choose_at_least_one_state"]],
              call. = FALSE
            )
          }
          code = get_code_td(
            store$equation_components,
            store$param_values,
            store$state_values,
            selected_states,
            store$independent,
            store$multiple_states
          )
          file_connection = file(file)
          writeLines(code, file_connection)
          close(file_connection)
        })
      }
    )
  })
}
