petenexUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tags$h1("PetenEx",
              class="title")
    ),
    fluidRow(
      column(8),
      column(
        width=2,
        uiOutput(ns("language_choice"))
        ),
      column(2)
    ),
    # horizontal rule
    fluidRow(column(2),
             column(width = 8,
                    tags$div(
                      class = "hrule"
                    )),
             column(2)), 
    # introduction
    fluidRow(
      column(2),
      column(width=8,
             uiOutput(ns("introduction"))
             ),
      column(2)
    ),
    # horizontal rule
    fluidRow(column(2),
             column(width = 8,
                    tags$div(
                      class = "hrule"
                    )),
             column(2)), 
    # tooltips & settings
    fluidRow(column(2),
             column(width = 6,
                    uiOutput(ns("tooltips"))),
             column(width = 2,
                    uiOutput(ns("mode")),
                    uiOutput(ns("grouping")),
                    uiOutput(ns("spec_var"))
                    ),
             column(2)),
    # horizontal rule
    fluidRow(column(2),
             column(width = 8,
                    tags$div(
                      class = "hrule"
                    )),
             column(2)),
    # Map
    fluidRow(
      column(2),
      column(
        width=8,
        tags$div(
          class="map_container",
          leafletOutput(outputId = ns("leafmap"), height=600)
        )
      ),
      column(2)
    )
  )  # end of tagList
}