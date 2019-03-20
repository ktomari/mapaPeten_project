library(dplyr)
library(tidygraph)
library(ggplot2)
library(sf)
library(sp)
library(spdep)
library(linkcomm)
library(dismo)

# DATA ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gov_lp_inclusive <- st_read(
  dsn="acdip_pop_project.gpkg",
  layer="gov_lp_inclusive"
)

gov_b <- st_read(
  dsn="acdip_pop_project.gpkg",
  layer="gov_b"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bounding box
bbox <- st_bbox(gov_b)

# bounding box to polygon
peten_extent <- bbox %>%
  st_as_sfc() %>%
  st_cast(to="POLYGON")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# highways
hwy_primary <- st_read(
  dsn="acdip_pop_project.gpkg",
  layer="osm_hwy_primary"
) %>%
  st_intersection(gov_b)
hwy_secondary <- st_read(
  dsn="acdip_pop_project.gpkg",
  layer="osm_hwy_secondary"
) %>%
  st_intersection(gov_b)
hwy_tertiary <- st_read(
  dsn="acdip_pop_project.gpkg",
  layer="osm_hwy_tertiary"
) %>%
  st_intersection(gov_b)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create simple spatial weights matrix based on distance decay.
#' 
#' @param sfo is a SF OBJECT. Should be clean (only points of interest).
#' @param id_column is CHARACTER - name of the ID column in sfo.
#' @param radius is NUMERIC - the radius of the neighborhood.
fn_weights_matrix <- function(sfo, 
                              id_column="cod_censo", 
                              radius=5000){
  
  # do not accept non-networks
  if(nrow(sfo) < 3) {
    warning("Warning fn_weights_matrix, number of features < 2")
    return(NULL)
  }
  
  # Convert to SP object.
  spo <- sfo %>%
    as("Spatial")
  
  # Create nb object.
  nbdist <- spo %>%
    sp::coordinates() %>%
    spdep::dnearneigh(d1 = 1,
                      d2 = radius,
                      row.names = spo@data[[id_column]]) %>%
    spdep::nbdists(coords = sp::coordinates(spo)) 
  
  # Distance Decay
  nbdist <- lapply(nbdist, function(x) {
    if (is.null(x))
      return(NULL)
    else
      1 / x
  })
  
  # Create Spatial Weights listw
  w <- spo %>%
    sp::coordinates() %>%
    spdep::dnearneigh(d1 = 1,
                      d2 = radius,
                      row.names = spo@data[[id_column]]) %>%
    spdep::nb2listw(glist = nbdist,  # general weights
                    style = "W",
                    zero.policy = T)
  
  
  lines <- w %>%
    listw2lines(coords=sp::coordinates(spo), 
                proj4string = CRS(proj4string(spo))) %>%
    st_as_sf()
  
  
  # Convert to Matrix.
  mat <- spdep::listw2mat(w)
  # mat <- spdep::nb2mat(nbdist)
  
  # add names
  colnames(mat) <- rownames(mat)
  
  # return
  list(mat=mat, lines=lines)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fn_cluster <- function(sfo, params){
  # subset only populated data
  sfo2 <- sfo %>%
    filter(POBTOT.y > 0 & !is.na(POBTOT.y))
  
  # get w
  w_list <- fn_weights_matrix(sfo2)
  
  if(params$cluster_method=="louvain"){
    adj_mat <- w_list$mat %>%
      graph.adjacency(mode="undirected", weighted=T) %>%
      as_tbl_graph()
    
    adj_mat <- adj_mat %>%
      activate(nodes) %>%
      filter(!node_is_isolated()) %>%
      mutate(groups = as.factor(group_louvain()))
    
    sfo2 <- sfo2 %>%
      left_join(as_tibble(adj_mat), 
                by=c("cod_censo"="name"))
  }
  if(params$cluster_method=="hclust"){
    # remove empty pops, subset percentmayan
    
    # make sp
    sfo3 <- as(sfo2, "Spatial")
    
    # create distance matrix
    matrix_of_dist1 <- dist(sfo3@coords)
    
    clusters <- hclust(matrix_of_dist1)  # just geographic
    
    sfo2 <- sfo2 %>%
      mutate(
        groups = as.factor(cutree(clusters, params$user_k))
      )
  }
  
  
  
  # return
  list(sfo=sfo2, lines=w_list$lines)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fn_polygoner <- function(sfo, boundary){
  sfo %>%
    group_by(groups) %>%
    summarize(cluster_percent_maya = sum(PEMAYA)/sum(POBTOT.y), 
              do_union=T) %>%
    st_triangulate() %>%
    st_union(by_feature = T) %>%
    st_intersection(boundary)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fn_centrality <- function(sfo, params){
  # take data with groups & split by group
  sf_list <- split(sfo, sfo$groups)
  
  # obtain adj matrix/polylines for each cluster
  if(params$user_network == "geographic"){
    mat_list <- lapply(sf_list, function(x){
      tmp <- fn_weights_matrix(x)
      if(is.null(tmp)) return(NULL)
      
      # covert matrix into adjacency matrix
      tmp$mat <- tmp$mat %>%
        graph_from_adjacency_matrix(mode="undirected", weighted=T) %>%
        as_tbl_graph() 
      
      tmp
    })
  }
  
  # Extract Lines
  lines <- lapply(seq_along(mat_list), function(i){
    if(is.null(mat_list[[i]])) return(NULL)
    mat_list[[i]]$lines %>%
      mutate(groups=i)
  })
  
  lines <- do.call(rbind, lines)
  
  # Calculate centrality and convert list to all tibbles
  if(params$centrality=="degree"){
    mat_list <- lapply(mat_list, function(x){
      if(is.null(x)) return(NULL)
      # return tibble
      x$mat %>%
        activate(nodes) %>%
        filter(!node_is_isolated()) %>%
        mutate(centrality = centrality_degree())%>%
        as_tibble()
    })
  }
  
  # Error Checking
  if(length(sf_list)!=length(mat_list)) stop("Error fn_centrality lists length")
  
  # Join centrality scores with sf objects.
  sf_list <- lapply(seq_along(sf_list), function(i){
    # if mat_list is blank
    if(is.null(mat_list[[i]])){
      warning(paste("mat_list iteration", i, "skipped."))
      return(NULL)
    }
    # Otherwise, join mat_list
    sf_list[[i]] %>%
      left_join(mat_list[[i]], by=c("cod_censo"="name")) %>%
      suppressWarnings()
  })
  
  centralized_sf <- do.call(rbind, sf_list)
  
  centralized_sf <- centralized_sf %>%
    group_by(groups) %>%
    mutate(centrality = ifelse(is.na(centrality), 0, centrality)) %>%
    mutate(centrality_percent = centrality/max(centrality))
  
  list(lines=lines,
       centralized_sf=centralized_sf
  )  # return
}