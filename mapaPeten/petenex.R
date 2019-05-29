petenex <- function(input, output, session){
  ns <- session$ns
  
  # language ----
  # Choose a language
  output$language_choice <- renderUI(
    selectInput(
      inputId = ns("chosen_language"),
      label = "Language/Lengua",
      selected = "English",
      choices = d$webtext$language,
      width="100%"
    )
  )
  
  # (RX) webtext ----
  # Adopt the chosen language
  rx_webtext <- reactive({
    req(input$chosen_language)
    d$webtext[d$webtext$language == input$chosen_language, ]
  })
  
  # intro ----
  # print an introduction
  output$introduction <- renderUI({
    req(input$chosen_language)
    # return
    tagList(tags$p(rx_webtext()$introduction,
                   class = "text_box"))
  })
  
  # mode ----
  # Have user choose a mode of exploration
  output$mode <- renderUI({
    req(input$chosen_language)
    selectInput(
      inputId = ns("chosen_mode"),
      label = rx_webtext()$widget_mode,
      choices = fn_split(rx_webtext()$mode),
      width="100%"
    )
  })
  
  # grouping ----
  output$grouping <- renderUI({
    req(input$chosen_language)
    req(input$chosen_mode)
    if(grepl("1\\.", input$chosen_mode, perl=T)) {
      tmp <- d$var_sets[,input$chosen_language]
      selectInput(
        inputId = ns("chosen_grouping"),
        label = rx_webtext()$widget_set_choice,
        choices = tmp,
        selected = "age",
        width="100%"
      )
    } else if (grepl("2\\.", input$chosen_mode, perl=T)) {
      tags$p("Feature coming soon. Característica que viene pronto.")
    }
  })
  
  # (RX) choices ----
  # Adopt the chosen language
  rx_choices <- reactive({
    req(input$chosen_language)
    req(input$chosen_mode)
    req(input$chosen_grouping)
    
    # retrieve names of desired columns to subset
    vars <-
      d$var_sets[which(d$var_sets[input$chosen_language] == 
                         input$chosen_grouping), "variables"]
    
    sub_vars <- d$var_sets[which(d$var_sets[input$chosen_language] == 
                                   input$chosen_grouping), "sub_variables"]
    
    if(is.na(vars) & is.na(sub_vars)) return(NA)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Here we need to differentiate
    # between variables and sub_variables
    # (as found in variable_subset.csv).
    # Essentially, if the value comes from
    # the column "variable" then we need
    # to apply fn_split(); otherwise we're
    # working with the column
    # "sub_variables", in which case we
    # need to take that string and get the
    # unique values from the census
    # data.frame, eg. one sub_variable is
    # categoria, in which case we need to
    # produce a list including CASERIO,
    # FINCA, etc.
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(!(is.na(sub_vars))){
      sub_vars <- fn_split(sub_vars)
      
      if(length(sub_vars) > 1){
        sub_vars <- sapply(sub_vars, function(x) {
          unique(cd$gov_lp_inclusive[x])
        })
      } else {
        
        sub_vars <- cd$gov_lp_inclusive %>%
          st_drop_geometry() %>%
          pull(sub_vars) %>%
          unique()
      }
      
      # return
      sub_vars
    } else {
      vars <- fn_split(vars)
      
      # return the vernacular
      d$dic[which(d$dic$variable_name %in% vars), 
            input$chosen_language]
    }
    
  })
  
  # spec_var ----
  # Create a dropdown list of possible variables.
  output$spec_var <- renderUI({
    req(input$chosen_language)
    req(input$chosen_mode)
    req(input$chosen_grouping)
    if(length(rx_choices()) > 1) {
      selectInput(
        inputId = ns("chosen_spec_var"),
        label = rx_webtext()$widget_spec_var,
        choices = rx_choices(),
        width="100%"
      )
    } else if (!(is.na(rx_choices()))) {
      selectInput(
        inputId = ns("chosen_spec_var"),
        label = rx_webtext()$widget_spec_var,
        choices = rx_choices(),
        width="100%"
      )
    }
  })
  
  # tooltips ----
  # Create tooltips
  output$tooltips <- renderUI({
    req(input$chosen_language)
    req(input$chosen_mode)
    req(input$chosen_grouping)
    tagList(
      tags$h4(rx_webtext()$widget_tooltips),
      tags$p(d$tooltips[
        which(d$tooltips$subset==rx_grouping()), 
        input$chosen_language],
             class="text_box")
    )
  })
  
  # (RX) current grouping ----
  # transliterate chosen_grouping to recognized subset
  rx_grouping <- reactive({
    req(input$chosen_language)
    req(input$chosen_mode)
    req(input$chosen_grouping)
    
    d$var_sets$subset[
      which(d$var_sets[input$chosen_language]==input$chosen_grouping)]
  })
  
  # (RX) current sub_variable ----
  rx_subvar <- reactive({
    req(input$chosen_language)
    req(input$chosen_mode)
    req(input$chosen_grouping)
    
    # Here we begin to form the args for
    # fn_simple()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Does set_choice/subset have
    # variables to send to the fn?
    if(rx_grouping() %in% 
       d$var_sets$subset[which(!(is.na(d$var_sets$variables)))]){
      req(input$chosen_spec_var)
      # eg. subset = age
      # eg. input$spec_var = Ages 0-4
      # get generic name for the specific variable.
      vr <- d$dic$variable_name[
        which(d$dic[input$chosen_language]==input$chosen_spec_var)]
      subvar <- NULL
    } else if (
      rx_grouping() %in% 
      d$var_sets$subset[which(!(is.na(d$var_sets$sub_variables)))]
    ) {
      req(input$chosen_spec_var)
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Unlike vr, no special
      # transliteration needs to occur here
      # because the dropdown pick-list was
      # created from unique factors in the
      # variable.
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      subvar <- input$chosen_spec_var
      vr <- NULL
    } else {
      subvar <- NULL
      vr <- NULL
    }
    
    # return
    list(subvar=subvar, vr=vr)
  })
  
  # (RX) leaf_df ----
  rx_leaf_df <- reactive({
    req(input$chosen_language)
    req(input$chosen_mode)
    
    # SIMPLE MODE
    if(grepl("1\\.", input$chosen_mode, perl=T)){
      req(input$chosen_grouping)
      # get generic name for subset (i.e.
      # non-vernacular form)
      subset <- rx_grouping()
      
      # obtain variables/sub_variables (see variable_subset.csv)
      vr <- rx_subvar()$vr
      subvar <- rx_subvar()$subvar
      
      # RETURN
      # Run function with the arguments we
      # defined above.
      fn_simple(
        df = cd$gov_lp_inclusive,
        subset = subset,
        vr = vr,
        subvar = subvar
      )
    }  # end of SIMPLE mode
  })
  
  # leafmap ----
  # Create a leaflet map
  output$leafmap <- renderLeaflet({
    req(input$chosen_language)
    req(input$chosen_mode)
    
    # SIMPLE MODE
    if(grepl("1\\.", input$chosen_mode, perl=T)){
      req(input$chosen_grouping)
      # Create leaflet map
      map <- leaflet() %>%
        addTiles()
      
      # Add Peten Boundary
      map <- map %>%
        addPolygons(
          data=as(cd$gov_b, "Spatial"),
          fillColor="#ccc",
          stroke=T,
          weight=1,
          fillOpacity=.1,
          color="#777",
          smoothFactor = 1
        )
      
      l <- rx_leaf_df()
      
      df_sp <- l$df %>%
        as("Spatial")
      
      withProgress(message = 'In Progress/En Progreso', value = 0, {
        for(i in seq_len(nrow(df_sp))){
          incProgress(1/nrow(df_sp))
          map <- map %>%
            addCircleMarkers(
              data=df_sp[i,],
              color=df_sp$color[i],
              radius=3,
              stroke=F,
              fillOpacity=1,
              label=HTML(fn_labelMaker(df_sp$lugar_pobl[i],
                                       df_sp$cod_censo[i],
                                       df_sp$simple_val[i]
                                       )),
              labelOptions = labelOptions(
                interactive=T
              )
            )
        }
        
        # return
        map
      })
    }  # End of SIMPLE mode
    
  })
}