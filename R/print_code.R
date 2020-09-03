print_header = function(ind, ind_min, ind_max, ind_n, param_names, param_values,
                        state_names, state_values, multiple_states) {

  if (multiple_states) {
    state_values = recycle(state_values)
    glue::glue(
      "<<ind>> = seq(<<ind_min>>, <<ind_max>>, length.out = <<ind_n>>)\n",
      "parameters = c(<<paste(param_names, '=', param_values, collapse = ', ')>>)\n",
      "<<paste(state_names, '=',
             sapply(state_values, function(x) paste0('c(' , paste(x, collapse = ', '), ')')),
              collapse = '\n')>>\n",
      .open = "<<", .close = ">>"
    )
  } else {
    glue::glue(
      "<<ind>> = seq(<<ind_min>>, <<ind_max>>, length.out = <<ind_n>>)\n",
      "parameters = c(<<paste(param_names, '=', param_values, collapse = ', ')>>)\n",
      "state = c(<<paste(state_names, '=', state_values, collapse = ', ')>>)\n",
      .open = "<<", .close = ">>"
    )
  }
}

print_odefun = function(ind, state_names, equations) {
  glue::glue(
    "ode_function = function(<<ind>>, state, parameters) {\n",
    "  with(as.list(c(state, parameters)), {\n",
    "<<paste0('    ', paste(paste0('d', state_names), equations, sep = ' = '), collapse = '\n')>>\n",
    "    list(c(<<paste(paste0('d', state_names), collapse = ', ')>>))\n",
    "  })\n",
    "}", .open = "<<", .close = ">>"
  )
}

print_solver = function(ind, ind_n, state_values, state_names, multiple_states) {
  if (multiple_states) {
    max_len = max(vapply(state_values, length, integer(1)))
    col_n = length(state_values) + 2 # Uno de la v.i y otro del grupo
    col_names = paste0("'", state_names, "'", collapse = ", ")
    txt = glue::glue(
      "output_df = data.frame(matrix(nrow = <<ind_n>> * <<max_len>>, ncol = <<col_n>>))\n",
      "names(output_df) = c('time', <<col_names>>, 'param_group')\n\n",
      "for (i in 1:<<max_len>>) {\n",
      "  state = c(<<paste0(state_names, ' = ', state_names, '[[i]]', collapse = ', ')>>)\n",
      "  ode_output = ode(func = ode_function, y = state, times = <<ind>>, parms = parameters)\n",
      "  ode_output = as.data.frame(ode_output)\n",
      "  ode_output$param_group = paste('group', i)\n",
      "  replace_idxs = ((i - 1) * <<ind_n>> + 1):(i * <<ind_n>>)\n",
      "  output_df[replace_idxs, ] = ode_output\n",
      "}\n",
      .open = "<<", .close = ">>"
    )
  } else {
    txt = glue::glue(
      "ode_output = ode(func = ode_function, y = state, times = {ind}, parms = parameters)\n",
      "output_df = as.data.frame(ode_output)"
    )
  }
  return(txt)
}


print_ggplot = function(independent, states, multiple_states) {

  if (length(states) > 1) {
    cols = paste0(paste0("'", states, "'", collapse = ", "))
    data_transform = glue::glue(
      "output_df_long = tidyr::pivot_longer(\n",
      "  data = output_df,\n",
      "  cols = c({cols}),\n",
      "  names_to = 'Estado',\n",
      "  values_to = 'Valor',\n",
      ")"
    )
    if (multiple_states) {
      ggplot_code = glue::glue(
        "ggplot(output_df_long) +\n",
        "  geom_line(aes(x = time, y = Valor, color = param_group, linetype = Estado), size = 1.5) +\n",
        "  scale_color_viridis_d()",
        .trim = FALSE
      )
    } else {
      ggplot_code = glue::glue(
        "ggplot(output_df_long) +\n",
        "  geom_line(aes(x = time, y = Valor, color = Estado), size = 1.5) +\n",
        "  scale_color_viridis_d()",
        .trim = FALSE
      )
    }
    return(paste(data_transform, ggplot_code, sep = "\n\n", collapse = "\n"))
  } else {
    if (multiple_states) {
      ggplot_code = ""
      for (state in states) {
        ggplot_code = paste0(
          ggplot_code,
          glue::glue(
            "ggplot(output_df) +\n",
            "  geom_line(aes(x = time, y = {state}, color = param_group), size = 1.5) +\n",
            "  scale_color_viridis_d() +\n",
            "  labs(title = '{paste('Estado', state)}') +\n",
            "  theme(legend.position = 'none')",
            .trim = FALSE
          ),
          sep = "\n\n"
        )
      }
    } else {
      ggplot_code = ""
      for (state in states) {
        ggplot_code = paste0(
          ggplot_code,
          glue::glue(
            "ggplot(output_df) +\n",
            "  geom_line(aes(x = time, y = {state}), size = 1.5) +\n",
            "  labs(title = '{paste('Estado', state)}')",
            .trim = FALSE
          )
        )
      }
    }
    return(ggplot_code)
  }
}

get_code_td = function(equation_components, param_values, state_values, state_selected,
                       independent, multiple_states) {

  ind = equation_components$independent
  ind_min = independent$min
  ind_max = independent$max
  ind_n = independent$n

  state_names = equation_components$state
  param_names = equation_components$params
  equations = equation_components$eqs

  imports = "#library(deSolve)\n#library(ggplot2)"
  header = print_header(ind, ind_min, ind_max, ind_n, param_names, param_values,
                        state_names, state_values, multiple_states)
  odefun = print_odefun(ind, state_names, equations)
  solver = print_solver(ind, ind_n, state_values, state_names, multiple_states)

  ggplot = print_ggplot(ind, state_selected, multiple_states)
  paste(imports, header, odefun, solver, ggplot, sep = "\n\n", collapse = "\n")
}

get_df_td = function(.equation_components, .param_values, .state_values, independent, .multiple_states) {
  .ind = .equation_components$independent

  .ind_min = independent$min
  .ind_max = independent$max
  .ind_n = independent$n

  .state_names = .equation_components$state
  .param_names = .equation_components$params
  .equations = .equation_components$eqs

  # Evaluate header
  eval(parse(text = print_header(
    .ind, .ind_min, .ind_max, .ind_n, .param_names, .param_values,
    .state_names, .state_values, .multiple_states
  )))

  # Evaluate `ode_function`
  # This defines `ode_function` within the execuition environment of this function.
  eval(parse(text = print_odefun(.ind, .state_names, .equations)))
  # Evaluate solver
  eval(parse(text = print_solver(.ind, .ind_n, .state_values, .state_names, .multiple_states)))
  eval(parse(text = "return(output_df)"))
}

get_plot_td = function(df, independent, selected_states, multiple_states) {
  output_df = df
  eval(parse(text = print_ggplot(independent, selected_states, multiple_states)))
}


print_flowfield = function(state_names, ind_range, state1_range, state2_range = NULL) {
  if (length(state_names) == 2) {
    system = "two.dim"
    state_names = paste0("c(", paste0("'", state_names, "'", collapse = ", "), ")")
    xlim = paste0("c(", paste0(state1_range, collapse = ", "), ")")
    ylim = paste0("c(", paste0(state2_range, collapse = ", "), ")")
  } else {
    system = "one.dim"
    state_names = paste0("'", state_names, "'")
    xlim = paste0("c(", paste0(ind_range, collapse = ", "), ")")
    ylim = paste0("c(", paste0(state1_range, collapse = ", "), ")")
  }
  glue::glue(
    "system_flowField = flowField(\n",
    "  deriv = ode_function,\n",
    "  parameters = parameters,\n",
    "  state.names = {state_names},\n",
    "  system = '{system}',\n",
    "  xlim = {xlim},\n",
    "  ylim = {ylim},\n",
    "  col = '#7f8c8d',\n",
    "  lwd = 2.5,\n",
    "  add = FALSE\n",
    ")"
  )
}

print_nullclines = function(state_names, ind_range, state1_range, state2_range = NULL) {
  colors = c("#c0392b", "#2980b9")
  if (length(state_names) == 2) {
    system = "two.dim"
    state_names = paste0("c(", paste0("'", state_names, "'", collapse = ", "), ")")
    xlim = paste0("c(", paste0(state1_range, collapse = ", "), ")")
    ylim = paste0("c(", paste0(state2_range, collapse = ", "), ")")
    colors = paste0("c(", paste0("'", colors, "'", collapse = ", "), ")")
  } else {
    system = "one.dim"
    state_names = paste0("'", state_names, "'")
    xlim = paste0("c(", paste0(ind_range, collapse = ", "), ")")
    ylim = paste0("c(", paste0(state1_range, collapse = ", "), ")")
    colors = paste0("'", colors[1], "'")
  }
  glue::glue(
    "system_nullclines = nullclines(\n",
    "  deriv = ode_function,\n",
    "  parameters = parameters,\n",
    "  state.names = {state_names},\n",
    "  system = '{system}',\n",
    "  points = 300,\n",
    "  xlim = {xlim},\n",
    "  ylim = {ylim},\n",
    "  col = {colors},\n",
    "  lwd = 3,\n",
    "  add.legend = FALSE\n",
    ")"
  )
}

print_nullclines_legend = function(state_names) {
  colors = c("#c0392b", "#2980b9")
  if (length(state_names) == 2) {
    labels = paste0("c(", paste0("'", state_names, " Nullclines'", collapse = ", "), ")")
    colors = paste0("c(", paste0("'", colors, "'", collapse = ", "), ")")
  } else {
    labels = paste0("'", state_names, " Nullclines'")
    colors = paste0("'", colors[1], "'")
  }
  glue::glue(
    "legend(\n",
    "  x = 'topright',\n",
    "  legend = {labels},\n",
    "  col = {colors},\n",
    "  lty = 1,\n",
    "  lwd = 3,\n",
    "  bg = 'white',\n",
    "  inset = 0.025,\n",
    "  cex = 1.1\n",
    ")"
  )
}

print_trajectory = function(state_names, ind_range, state1_vals, state2_vals = NULL) {
  tlim = paste0("c(", paste0(ind_range, collapse = ", "), ")")
  if (length(state_names) == 2) {
    system = "two.dim"
    state_names = paste0("c(", paste0("'", state_names, "'", collapse = ", "), ")")
    y = paste0(paste0(state1_vals, ", "), state2_vals, collapse = ", ")
    y0 = glue::glue(
      "y0 = matrix(\n",
      "  data = {paste0('c(', y, ')')},\n",
      "  ncol = 2,\n",
      "  byrow = TRUE\n",
      ")"
    )
  } else {
    system = "one.dim"
    state_names = paste0("'", state_names, "'")
    y0 = paste0("y0 = c(", paste0(state1_vals, collapse = ", "), ")")

  }
  glue::glue(
    "{y0}\n\n",
    "system_trajectory = trajectory(\n",
    "  deriv = ode_function,\n",
    "  parameters = parameters,\n",
    "  state.names = {state_names},\n",
    "  system = '{system}',\n",
    "  y0 = y0,\n",
    "  tlim = {tlim},\n",
    "  lwd = 2.5\n",
    ")"
  )
}

print_header_2 = function(ind, ind_min, ind_max, ind_n, param_names, param_values) {
  glue::glue(
    "<<ind>> = seq(<<ind_min>>, <<ind_max>>, length.out = <<ind_n>>)\n",
    "parameters = c(<<paste(param_names, '=', param_values, collapse = ', ')>>)\n",
    .open = "<<", .close = ">>"
  )
}

get_code_pp = function(equation_components, param_values, ind_settings,
                       state1_list, state2_list = NULL,
                       return_type = c('all', 'settings', 'plot')) {

  ind = equation_components$independent
  ind_min = ind_settings$min
  ind_max = ind_settings$max
  ind_n = ind_settings$n

  state_names = equation_components$state
  param_names = equation_components$params
  equations = equation_components$eqs

  if (is.null(state2_list)) {
    state_names_ = state1_list$name
    state1_r = state1_list$range
    state1_v = state1_list$value
    state2_r = NULL
    state2_v = NULL
  } else {
    state_names_ = c(state1_list$name, state2_list$name)
    state1_r = state1_list$range
    state1_v = state1_list$value
    state2_r = state2_list$range
    state2_v = state2_list$value
  }

  imports = "# library(phaseR)"
  header = print_header_2(ind, ind_min, ind_max, ind_n, param_names, param_values)
  odefun = print_odefun(ind, state_names, equations)

  flowfield = print_flowfield(state_names_, c(ind_min, ind_max), state1_r, state2_r)
  nullclins = print_nullclines(state_names_, c(ind_min, ind_max), state1_r, state2_r)
  trajectry = print_trajectory(state_names_, c(ind_min, ind_max), state1_v, state2_v)
  nullclins_lgnd = print_nullclines_legend(state_names_)

  if (return_type == 'all') {
    return(paste(imports, header, odefun, flowfield, nullclins, trajectry, nullclins_lgnd,
                 sep = "\n\n", collapse = "\n"))
  }
  if (return_type == 'settings') {
    return(paste(header, odefun, sep = "\n\n", collapse = "\n"))
  }
  if (return_type == 'plot') {
    return(paste(flowfield, nullclins, trajectry, nullclins_lgnd,
                 sep = "\n\n", collapse = "\n"))
  }
}
