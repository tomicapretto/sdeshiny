app_ui = function() {
  fluidPage(
    useShinyjs(),
    shinypop::use_notiflix_notify(
      position = "right-bottom",
      timeout = 8000
    ),
    tags$head(
      tags$style(
        HTML('.form-group {margin-bottom: 10px;}')
      )
    ),
    sidebarLayout(
      app_sidebar(),
      app_mainpanel()
    )
  )
}

