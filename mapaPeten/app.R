library(shiny)
library(leaflet)

source("mapa_functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  includeCSS("mapaPeten.css"),
  fluidRow(
      tags$div(class="bloc",
               tags$h1("Mapa de Peten", class="central"),
               tags$p("Plotting may take up to a few minutes.", class="central")
    )  # end of column
  ),
  
  fluidRow(
    tags$div(class="bloc",
    uiOutput("k_select", class="central")
    )
    
  ),
  fluidRow(
    tags$div(class="bloc",
             leafletOutput(outputId = "leafmap", height=600)
             # plotOutput("outplot", width = "100%", height="1000px")
    )
  )
   
)  # End of fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run <- reactive({
    req(input$chosen_k)
    params <- list()
    params$cluster_method <- "hclust"
    params$user_k <- input$chosen_k
    params$user_network <- "geographic"
    params$centrality <- "degree"
    
    n <- 4
    
    withProgress(message = 'Calculating clusters', value = 0, {
      incProgress(1/n, detail = "Calculating clusters")
      cluster_object <- fn_cluster(spatials$gov_lp_inclusive, params)
      gov_lp1 <- cluster_object$sfo
      
      incProgress(1/n, detail = "Calculating cluster polygons")
      cluster_polys <- fn_polygoner(gov_lp1, spatials$gov_b)
      
      incProgress(1/n, detail = "Calculating centrality")
      centralities <- fn_centrality(gov_lp1, params)
      
      incProgress(1/n, detail = "Calculations Complete")
    })
    
    # return
    list(
      cluster_object=cluster_object,
      gov_lp1=gov_lp1,
      cluster_polys=cluster_polys,
      centralities=centralities
      )
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$k_select <- renderUI(
    sliderInput("chosen_k", 
                "Please select cluster value:",
                min = 2, 
                max = 70,
                value = 40)
    )
   
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # output$outplot <- renderPlot({
  #   l <- run()
  #   
  #   g <- ggplot() +
  #     # peten boundaries
  #     geom_sf(data=spatials$gov_b) +
  #     
  #     # background polygon
  #     geom_sf(data=l$cluster_polys,
  #             mapping=aes(fill=cluster_percent_maya)) +
  #     
  #     # within-cluster network lines
  #     geom_sf(data=l$centralities$lines,
  #             color="red") +
  #     
  #     # within-cluster points
  #     geom_sf(data=l$centralities$centralized_sf,
  #             mapping=aes(color=centrality_percent)) +
  #     scale_color_distiller(type="seq", 
  #                           palette="Oranges",
  #                           direction=-1
  #     )
  #   g  # return
  # })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$leafmap <- renderLeaflet({
    map <- leaflet() %>%
      addTiles()
    
    l <- run()
    
    # convert to sp objects
    cluster_polys <- l$cluster_polys %>%
      st_transform(crs=4326) %>%
      as("Spatial")
    
    centralized_sf <- l$centralities$centralized_sf %>%
      st_transform(crs=4326) %>%
      as("Spatial")
      
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    cluster_polys$color <- fn_pal1(10)[as.numeric(
      cut(
        cluster_polys$cluster_percent_maya,
        breaks = 10)
    )
    ]
    
    for(i in seq_len(nrow(cluster_polys))){
      map <- map %>%
        addPolygons(
          data=cluster_polys[i,],
          fillColor=cluster_polys$color[i],
          stroke=T,
          weight=1,
          fillOpacity=.9,
          color="#777",
          smoothFactor = 1
        )
    }
    
    for(i in seq_len(nrow(centralized_sf))){
      map <- map %>%
        addCircleMarkers(
          data=centralized_sf[i,],
          color=centralized_sf$centrality_color[i],
          radius=3,
          stroke=F,
          fillOpacity=1,
          label=centralized_sf$lugar_pobl[i],
          labelOptions = labelOptions(
            interactive=T
          )
        )
    }
    
    map  # return
  })  # end of render leafletmap

}

# Run the application 
shinyApp(ui = ui, server = server)

