update_equation_n_gen = function() {
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

withCustomHandler = function(expr) {
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

create_tempdir = function() {
  temp_dir = tempfile()
  dir.create(temp_dir)
  return(temp_dir)
}

create_and_save_plot = function(dir, settings_expr, plt_expr, plt_name) {

  eval(parse(text = settings_expr))
  png(
    file.path(dir, paste0(plt_name, ".png")),
    res = 300, units = "in", width = 9.33, height = 7
  )
  eval(parse(text = plt_expr))
  dev.off()
}

delete_dir = function(temp_dir) {
  unlink(temp_dir, recursive = TRUE)
}

make_counter = function() {
  count = 0
  f = function(reset=FALSE) {
    if (reset) {
      count <<- 0
    } else {
      count <<- count + 1
    }
    #print(count)
  }
  return(f)
}

# eq1 = "\\frac{dS}{dt} = b\\times N - \\beta\\times S \\times I - a \\times S"
# eq2 = "\\frac{dI}{dt} = \\beta\\cdot S\\cdot I-\\gamma\\cdot I-a\\cdot I"
# eq3 = "\\frac{dR}{dt} = \\gamma\\cdot I-a\\cdot R"
# eqs = c(eq1, eq2, eq3)
# process_equations(eqs)


# Ideas:
#
# Todos los parametros (ecuaciones, estados, parametros, etc.) que son
# obtenidos en el sidebarpanel, van a parar a un reactiveValues llamado 'store'.
#
# Todo lo que sucede en cada panel va dentro de su propio modulo.
# De esa forma no me tengo que preocupar por los botones de descarga y etc.
