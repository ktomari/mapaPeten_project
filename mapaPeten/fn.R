
fn_labelMaker <- function(x,y,z) {
  paste0(
    "<b>",
    x,
    "</b><p>ID: ",
    y,
    "</p><p><b>",
    z,
    "</b></p>"
  )
}

#' Create a function to subset the data.frame
#' 
#' @param df is the census data.
#' @param subset is the value found in d$var_sets$subset
#' @param vr is the variable of interest.
#' @param subvar is an optional value, eg unique(df$categoria)
#' @param map is the leaflet map already partially constructed
#' @return is a list with the df
fn_simple <- function(df=cd$gov_lp_inclusive, 
                      subset,
                      vr=NULL,
                      subvar=NULL) {
  
  l <- list()
  
  if(subset=="gender") {
    # GENDER RATIO
    
    # get ratio of women to men
    df <- df %>%
      mutate(simple_val = MUJERES/HOMBRES) %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(is.finite(simple_val)) %>%
      filter(!is.na(simple_val))
    
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(5)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 5)
      )]
    
  } else if(subset=="age"){
    # PERCENT AGE
    
    if(is.null(vr)) stop("vr must not be NULL")
    
    tmp <- purrr::pluck(st_drop_geometry(df), vr)
    total <- purrr::pluck(st_drop_geometry(df), "POBTOT_pop")
    
    # divide desired age by total pop to get percentage
    # better way of doing this with dplyr + rlang?
    df$simple_val <- tmp/total
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(!is.na(simple_val)) %>%
      filter(is.finite(simple_val))
    
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(5)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 5)
      )]
    
  } else if(subset=="type") {
    # CATEGORIA SUBSET
    # Note, coloring is based on population size.
    
    if(is.null(subvar)) stop("subvar must not be NULL")
    df <- df %>%
      filter(categoria == subvar) %>%
      rename(simple_val = POBTOT_pop)
    
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(is.finite(simple_val)) %>%
      filter(!is.na(simple_val))
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(10)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 10)
      )]
    
  } else if(subset=="ethnicity"){
    # PERCENT ETHNICITY
    
    if(is.null(vr)) stop("vr must not be NULL")
    tmp <- purrr::pluck(st_drop_geometry(df), vr)
    total <- purrr::pluck(st_drop_geometry(df), "POBTOT_pop")
    
    # divide desired age by total pop to get percentage
    # better way of doing this with dplyr + rlang?
    df$simple_val <- tmp/total
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(!is.na(simple_val)) %>%
      filter(is.finite(simple_val))
    
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(5)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 5)
      )]
    
  } else if(subset=="language") {
    # PERCENT LANGUAGE
    
    tmp <- purrr::pluck(st_drop_geometry(df), vr)
    total <- purrr::pluck(st_drop_geometry(df), "POBTOT_pop")
    
    # divide desired age by total pop to get percentage
    # better way of doing this with dplyr + rlang?
    df$simple_val <- tmp/total
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(!is.na(simple_val)) %>%
      filter(is.finite(simple_val))
    
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(5)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 5)
      )]
    
  } else if(subset=="indigeneity") {
    # INDIGENEITY RATIO
    
    df <- df %>%
      mutate(simple_val = GEIND/GENOIND)
    
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(is.finite(simple_val)) %>%
      filter(!is.na(simple_val)) %>%
      mutate(color=case_when(simple_val > 2 ~ "#ff0000",
                             TRUE ~ "#ffff00"))
    
    
    # fn_pal1 <- colorRampPalette(c("yellow", "red"))
    # 
    # df$color <- fn_pal1(5)[
    #   as.numeric(
    #     cut(
    #       df$simple_val,
    #       breaks = 5)
    #   )]
    
  } else if(subset=="maya") {
    # PERCENT MAYA
    
    df <- df %>%
      rename(simple_val = PercentMaya)
    
    df <- df %>%
      select(cod_censo, lugar_pobl, simple_val) %>%
      filter(is.finite(simple_val)) %>%
      filter(!is.na(simple_val))
    fn_pal1 <- colorRampPalette(c("yellow", "red"))
    
    df$color <- fn_pal1(5)[
      as.numeric(
        cut(
          df$simple_val,
          breaks = 5)
      )]
  }
  
  l$df <- df
  
  # return
  l
}