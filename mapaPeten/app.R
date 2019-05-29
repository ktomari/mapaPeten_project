# TODO
# - determine why when switching "grouping" it loads one map and then another.
# - some bugs in spanish

source("global.R")

ui <- fluidPage(
  includeCSS("petenex.css"),
  petenexUI("RUN")
)

server <- function(input, output) {
  callModule(petenex, "RUN")
}

# Run the application 
shinyApp(ui = ui, server = server)