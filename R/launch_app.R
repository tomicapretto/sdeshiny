#' Run application
#'
#' @return An object that represents the app. Printing the object or passing it to runApp() will run the app.

#'
#' @importFrom shiny shinyApp fluidPage sidebarLayout mainPanel sidebarPanel tabsetPanel tabPanel verbatimTextOutput observeEvent eventReactive renderUI uiOutput renderText fluidRow tagList actionButton sliderInput updateSliderInput numericInput textInput reactiveValues req  column tags observe HTML br h3 hr plotOutput renderPlot selectInput updateSelectInput downloadHandler downloadButton wellPanel imageOutput isolate renderImage updateNumericInput
#' @importFrom shinyjs useShinyjs enable disable disabled hide hidden show
#' @importFrom deSolve ode
#' @importFrom ggplot2 ggplot geom_line labs theme scale_color_viridis_d aes ggsave
#' @importFrom phaseR flowField nullclines trajectory
#' @importFrom grDevices dev.off png
#' @export
launch_app = function() {
  shiny::runApp(shinyApp(app_ui, app_server), launch.browser = TRUE)
}
