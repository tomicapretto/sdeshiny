recycle = function(num_list) {
  length_max <- max(vapply(num_list, length, integer(1)))
  lapply(num_list, rep_len, length.out = length_max)
}

split_equation = function(equation, split = "=") {
  equations = unlist(strsplit(equation, split))
  if (length(equations) != 2) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_two_components"]])
  }
  return(equations)
}

parse_differential = function(equation) {
  slash_count = stringr::str_count(equation, "/")
  if (slash_count != 1) {
    lang = getOption("sdeshiny.lang")
    stop(paste0(LANG_MSG[[lang]][["need_one_differential"]], slash_count, "."))
  }
  equations = stringr::str_remove_all(split_equation(equation, "/"), " ")
  if (any(!stringr::str_starts(equations, "d"))) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_leibniz"]])
  }

  if (any(stringr::str_remove(equations, "d") == "")) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["empty_var"]])
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
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_one_equation"]])
  }

  if (!length(independent) == 1) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_one_independent"]])
  }

  params = sort(setdiff(setdiff(parameters, dependents), independent))
  params = params[order(nchar(params))]

  return(list(
    "eqs" = equations,"state" = dependents,
    "params" = params, "independent" = independent
  ))
}

process_states = function(string) {
  string = sub("\\s*^,", "", string)
  string = sub(",\\s*$", "", string)

  if (gsub(" ", "", string) == "") {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_non_empty_state"]])
  }

  x = suppressWarnings(as.numeric(string)) # Si es '' tambien da NA.
  if (!is.na(x)) {
    return(x)
  }

  if (!grepl(",", string)) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["use_commas"]])
  }

  x = tryCatch({
    as.numeric(unlist(strsplit(string, ",")))
  },
  warning = function(cnd) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["unrecognized_initial_states"]])
  },
  error = function(cnd) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["unrecognized_initial_states"]])
  })
  x[!is.na(x)]
}

process_param = function(param) {
  if (is.na(param)) {
    lang = getOption("sdeshiny.lang")
    stop(LANG_MSG[[lang]][["need_non_empty_params"]])
  }
  return(param)
}
