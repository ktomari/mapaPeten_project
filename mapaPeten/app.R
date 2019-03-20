library(shiny)

source("mapa_functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  includeCSS("mapaPeten.css"),
  fluidRow(
  tags$div(class="bloc",
           tags$h1("Mapa de Peten", class="central"),
           tags$p("Plotting may take up to a few minutes.", class="central")
  )

  ),
  fluidRow(
    tags$div(class="bloc",
    uiOutput("k_select", class="central")
    )
    
  ),
  fluidRow(
    tags$div(class="bloc",
             plotOutput("outplot", width = "100%", height="1000px")
    )
  )
   
)  # End of fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$k_select <- renderUI(
    sliderInput("chosen_k", 
                "Please select cluster value:",
                min = 1, 
                max = 100,
                value = 40)
    )
   
  output$outplot <- renderPlot({
    req(input$chosen_k)
    params <- list()
    params$cluster_method <- "hclust"
    params$user_k <- input$chosen_k
    params$user_network <- "geographic"
    params$centrality <- "degree"
    
    n <- 5
    
    withProgress(message = 'Calculating clusters', value = 0, {
      incProgress(1/n, detail = "Calculating clusters")
      tmp <- fn_cluster(gov_lp_inclusive, params)
      gov_lp1 <- tmp$sfo
      
      incProgress(1/n, detail = "Calculating cluster polygons")
      cluster_polys <- fn_polygoner(gov_lp1, gov_b)
      
      incProgress(1/n, detail = "Calculating centrality")
      tmp <- fn_centrality(gov_lp1, params)
      
      incProgress(1/n, detail = "Designing Plot")

      g <- ggplot() +
        geom_sf(data=gov_b) +
        geom_sf(data=cluster_polys,
                mapping=aes(fill=cluster_percent_maya)) +
        geom_sf(data=tmp$lines,
                color="red") +
        geom_sf(data=tmp$centralized_sf,
                mapping=aes(color=centrality_percent)) +
        scale_color_distiller(type="seq", 
                              palette="Oranges",
                              direction=-1
        )
      incProgress(1/n, detail = "Rendering plot")
      g
    })  # progres msg
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

