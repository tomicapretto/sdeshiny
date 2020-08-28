# Generate parameter inputs ---------------------------------------------------------
paramInput = function(param) {
  numericInput(
    inputId = paste0("param_", param),
    label = param,
    value = 1
  )
}
stateInput = function(state) {
  textInput(
    inputId = paste0("state_", state),
    label = state,
    value = "",
    placeholder = "Uno o mas valores separados por ','"
  )
}


# Equation number counter -----------------------------------------------------------
update_eq_n_generator = function() {
  n_old = 0

  f = function(input, output, session, n_new) {
    if (n_new > n_old) {
      idxs = setdiff(seq_len(n_new), seq_len(n_old))
      idxs = paste0("eq", idxs, "_div")
      purrr::walk(idxs, shinyjs::show)
    }
    if (n_new < n_old) {
      idxs = setdiff(seq_len(n_old), seq_len(n_new))
      idxs = paste0("eq", idxs, "_div")
      purrr::walk(idxs, shinyjs::hide)
    }
    n_old <<- n_new
  }
}

# Process equations -----------------------------------------------------------------
recycle <- function(num_list) {
  length_max <- max(vapply(num_list, length, integer(1)))
  lapply(num_list, rep_len, length.out = length_max)
}

split_equation = function(equation, split = "=") {
  equations = unlist(strsplit(equation, split))
  if (length(equations) != 2) {
    stop("La expresion debe tener dos componentes, el diferencial y la ecuacion.")
  }
  return(equations)
}

parse_differential = function(equation) {
  slash_count = stringr::str_count(equation, "/")
  if (slash_count != 1) {
    stop(
      paste0("La expresion del diferencial debe tener una sola fraccion, pero hay ", slash_count, ".")
    )
  }
  equations = stringr::str_remove_all(split_equation(equation, "/"), " ")
  if (any(!stringr::str_starts(equations, "d"))) {
    stop(
      paste0("Ambos componentes del diferencial deben empezar con 'd'. Por ejemplo, 'dx/dt'.")
    )
  }

  if (any(stringr::str_remove(equations, "d") == "")) {
    stop(
      paste0("Al menos una variable independiente o dependiente esta vacia.")
    )
  }
  return(list(
    "dep" = stringr::str_remove(equations[1], "d"),
    "ind" = stringr::str_remove(equations[2], "d")
  ))
}

get_ast = function(arg) purrr::map_if(as.list(arg), is.call, get_ast)

get2 = function(x) {
  if (exists(x)) return(get(x))
  return(NULL)
}

is_function2 = function(x) {
  greek_letters = c(
    "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
    "gamma", "varpi", "phi", "delta", "kappa", "rho",
    "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
    "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
    "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
    "Upsilon", "Omega", "Theta", "Pi", "Phi"
  )
  x_chr = as.character(x)
  if (tolower(x_chr) %in% letters) return(FALSE)
  if (tolower(x_chr) %in% greek_letters) return(FALSE)
  is.function(get2(x_chr))
}

get_args = function(expr, sort = TRUE) {
  expr = parse(text = expr)
  ast = unlist(get_ast(expr))
  result = sapply(
    ast,
    function(x) {if (is.symbol(x) && !is_function2(x)) as.character(x) else  NA}
  )
  result = result[!is.na(result)]
  if (sort) {
    return(sort(unique(result)))
  } else {
    return(unique(result))
  }
}

get_equation_comp = function(equation) {
  equation = split_equation(equation, "=")
  differential = parse_differential(equation[1])
  equation = sub("^ ", "", equation[2])
  equation_args = get_args(equation)
  return(list(
    "dep" = differential$dep,
    "ind" = differential$ind,
    "eq" = equation,
    "args" = equation_args
    ))
}

process_equations = function(equations) {
  equations = purrr::map_chr(equations, latex2r::latex2r)
  components = purrr::map(equations, get_equation_comp)
  dependents = unlist(purrr::map(components, function(x) x[[1]]))
  independent = unique(unlist(purrr::map(components, function(x) x[[2]])))
  equations = unique(unlist(purrr::map(components, function(x) x[[3]])))
  parameters = unique(unlist(purrr::map(components, function(x) x[[4]])))

  if (!length(unique(dependents)) == length(dependents)) {
    stop("No puede haber mas una ecuacion para cada variable dependiente.")
  }

  if (!length(independent) == 1) {
    stop("La variable independiente debe ser la misma para todas las ecuaciones.")
  }

  params = sort(setdiff(setdiff(parameters, dependents), independent))
  params = params[order(nchar(params))]

  return(list(
    "eqs" = equations,"state" = dependents,
    "params" = params, "independent" = independent
  ))
}

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
    glue::glue(
      "df = data.frame(matrix(nrow = <<ind_n>> * <<max_len>>, ncol = <<col_n>>))\n",
      "names(df) = c('<<ind>>', <<col_names>>, 'param_group')\n\n",
      "for (i in 1:<<max_len>>) {\n",
      "  state = c(<<paste0(state_names, ' = ', state_names, '[[i]]', collapse = ', ')>>)\n",
      "  ode_output = ode(func = ode_function, y = state, times = <<ind>>, parms = parameters)\n",
      "  ode_output = as.data.frame(ode_output)\n",
      "  ode_output$param_group = paste('group', i)\n",
      "  replace_idxs = ((i - 1) * <<ind_n>> + 1):(i * <<ind_n>>)\n",
      "  df[replace_idxs, ] = ode_output\n",
      "}\n",
      .open = "<<", .close = ">>"
    )
  } else {
    glue::glue(
      "ode_output = ode(func = ode_function, y = state, times = {ind}, parms = parameters)\n",
      "ode_output = as.data.frame(ode_output)"
    )
  }
}

print_ggplot = function(state, ind, multiple_states) {
  if (multiple_states) {
    glue::glue(
      "ggplot(df) +\n",
      "  geom_line(aes(x = {ind}, y = {state}, color = param_group), size = 1.5) +\n",
      "  scale_color_viridis_d() +\n",
      "  labs(title = '{paste('Estado', state)}') +\n",
      "  theme(legend.position = 'none')",
     .trim = FALSE
    )
  } else {
    glue::glue(
      "ggplot(ode_output) +\n",
      "  geom_line(aes(x = {ind}, y = {state}), size = 1.5) +\n",
      "  labs(title = '{paste('Estado', state)}')",
      .trim = FALSE
    )
  }
}

process_states = function(string) {
  string = sub("\\s*^,", "", string)
  string = sub(",\\s*$", "", string)

  if (gsub(" ", "", string) == "") {
    stop("El estado inicial no puede estar vacio.", call. = FALSE)
  }

  x = suppressWarnings(as.numeric(string)) # Si es '' tambien da NA.
  if (!is.na(x)) {
    return(x)
  }

  if (!grepl(",", string)) {
    stop("Utilice ',' para indicar mas de un estado inicial.", call. = FALSE)
  }

  x = tryCatch({
    as.numeric(unlist(strsplit(string, ",")))
  },
  warning = function(cnd) {
    stop("No se pudo reconocer a los estados iniciales.", call. = FALSE)
  },
  error = function(cnd) {
    stop("No se pudo reconocer a los estados iniciales.", call. = FALSE)
  })
  x[!is.na(x)]
}

process_param = function(param) {
  if (is.na(param)) stop("Ningun parametro puede estar vacio", call. = FALSE)
  return(param)
}

withCustomHandler <- function(expr) {
  tryCatch({
    expr
  },
  shiny.silent.error = function(cnd) {
    # This is the error class signalled by `req()`
    NULL
  },
  error = function(cnd) {
    shinypop::nx_notify_error(
      paste("Ha ocurrido un error:", cnd$message)
    )
  })
}

get_code_panel_1 = function(equation_components, param_values, state_values,
                            ind_settings, multiple_states) {

  ind = equation_components$independent
  ind_min = ind_settings$interval[1]
  ind_max = ind_settings$interval[2]
  ind_n = ind_settings$ind_n

  state_names = equation_components$state
  param_names = equation_components$params
  equations = equation_components$eqs

  imports = "#library(deSolve)\n#library(ggplot2)"
  header = print_header(ind, ind_min, ind_max, ind_n, param_names, param_values,
                        state_names, state_values, multiple_states)
  odefun = print_odefun(ind, state_names, equations)
  solver = print_solver(ind, ind_n, state_values, state_names, multiple_states)

  ggplot = paste0(
    purrr::map_chr(state_names, print_ggplot, ind = ind, multiple_states = multiple_states),
    collapse = "\n\n"
  )
  paste(imports, header, odefun, solver, ggplot, sep = "\n\n", collapse = "\n")
}


get_graphs_panel_1 = function(.equation_components, .param_values, .state_values, .ind_settings, .multiple_states) {

  .ind = .equation_components$independent
  .ind_min = .ind_settings$interval[1]
  .ind_max = .ind_settings$interval[2]
  .ind_n = .ind_settings$ind_n

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

  # Generate ggplot2 code
  ggplot_char = purrr::map_chr(.state_names, print_ggplot, ind = .ind, multiple_states = .multiple_states)

  # Format and return output
  output = purrr::map(ggplot_char, function(x) eval(parse(text = x)))
  names(output) = .state_names
  output
}

# eq1 = "\\frac{dS}{dt} = b\\times N - \\beta\\times S \\times I - a \\times S"
# eq2 = "\\frac{dI}{dt} = \\beta\\cdot S\\cdot I-\\gamma\\cdot I-a\\cdot I"
# eq3 = "\\frac{dR}{dt} = \\gamma\\cdot I-a\\cdot R"
# eqs = c(eq1, eq2, eq3)
# process_equations(eqs)

# cat(print_fun1(
#   process_equations(eqs),
#   param_values = 1:6,
#   state_values = list(1, 2, 3),
#   ind_settings = list("interval" = c(0, 20), "ind_n" = 500),
#   multiple_states = FALSE
# ))
#
# cat(print_fun1(
#   process_equations(eqs),
#   param_values = 1:6,
#   state_values = list(1:4, 1:2, 3),
#   ind_settings = list("interval" = c(0, 20), "ind_n" = 500),
#   multiple_states = TRUE
# ))


# Temporary directory utils ---------------------------------------------------------

create_tempdir = function() {
  temp_dir = tempfile()
  dir.create(temp_dir)
  return(temp_dir)
}

create_and_save_plot = function(dir, plt_expr, plt_name) {
  png(
    file.path(dir, paste0(plt_name, ".png")),
    res = 300, units = "in", width = 9.33, height = 7
  )
  plt_expr
  dev.off()
}

# open_dir <- function(dir = getwd()){
#   if (.Platform['OS.type'] == "windows"){
#     shell.exec(dir)
#   } else {
#     system(paste(Sys.getenv("R_BROWSER"), dir))
#   }
# }

delete_dir = function(temp_dir) {
  unlink(temp_dir, recursive = TRUE)
}

# temp_dir = create_tempdir()
# create_and_save_plot(temp_dir, plot(1:10, 1:10, type="l"), "graf")
# open_dir(temp_dir)
# delete_dir(temp_dir)
# dir.exists(temp_dir)

# Second panel utils -----------------------------------------------------------------

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
    "  lwd = 2,\n",
    "  add = FALSE\n",
    ")"
  )
}

# print_flowfield("x", c(0, 10), c(0, 5))
# print_flowfield(c("x", "y"), c(0, 20), c(0, 5), c(0, 10))

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

# print_nullclines("x", c(0, 10), c(0, 5))
# print_nullclines(c("x", "y"), c(0, 20), c(0, 5), c(0, 10))

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
    "  inset = 0.025\n",
    ")"
  )
}

# print_nullclines_legend('X')
# print_nullclines_legend(c('X', 'Y'))

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
    "  tlim = {tlim}\n",
    ")"
  )
}

# print_trajectory("x", c(0, 20), 1:3)
# print_trajectory(c("x", "y"), c(0, 20), 1:3, 4:6)

print_header_2 = function(ind, ind_min, ind_max, ind_n, param_names, param_values) {
  glue::glue(
    "<<ind>> = seq(<<ind_min>>, <<ind_max>>, length.out = <<ind_n>>)\n",
    "parameters = c(<<paste(param_names, '=', param_values, collapse = ', ')>>)\n",
    .open = "<<", .close = ">>"
  )
}

# print_header_2("t", 0, 10, 500, c('x', 'y'), 1:2)

get_code_panel_2 = function(equation_components, param_values, ind_settings,
                            state1_list, state2_list = NULL,
                            return_type = c('all', 'settings', 'plot')) {

  ind = equation_components$independent
  ind_min = ind_settings$interval[1]
  ind_max = ind_settings$interval[2]
  ind_n = ind_settings$ind_n

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

# eq1 = "\\frac{dS}{dt} = b\\times N - \\beta\\times S \\times I - a \\times S"
# eq2 = "\\frac{dI}{dt} = \\beta\\cdot S\\cdot I-\\gamma\\cdot I-a\\cdot I"
# eq3 = "\\frac{dR}{dt} = \\gamma\\cdot I-a\\cdot R"
# eqs = c(eq1, eq2, eq3)
#
# cat(get_code_panel_2(
#   process_equations(eqs),
#   1:6,
#   list("interval" = c(0, 20), "ind_n" = 500),
#   state1_list = list(name = "x", value = 1:2, range = c(0, 5)),
#   state2_list = NULL,
#   return_type = 'plot'
# ))
#
# cat(get_code_panel_2(
#   process_equations(eqs),
#   1:6,
#   list("interval" = c(0, 20), "ind_n" = 500),
#   state1_list = list(name = "x", value = 1:2, range = c(0, 5)),
#   state2_list = list(name = "y", value = 0:1, range = c(0, 8)),
#   return_type = 'plot'
# ))