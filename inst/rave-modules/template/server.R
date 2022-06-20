library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  # For loader
  loader_server(input, output, session, ...)

  # For the main module
  module_server(input, output, session, ...)


}
