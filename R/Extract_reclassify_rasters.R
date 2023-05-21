#' Title Extract raster classification
#'
#' @param modis_cropped_500 Modis file after cropped with boundary shapefile and reproject to resolution 500m
#' @param reclass_table_path path Where the classsification table are stored in .csv
#' @param reclass_order_path path Where order of the matrix are stored in .csv
#'
#' @description
#' The results of this function is a table consist of X,Y point of the MODIS data, classification of the point.
#'
#' @return reclassified_table
#' @export
#'
#' @examples
#' (require)library(terra)
#' (require)library(tidyverse)
#' (require)library(MASS)
#' (require)library(ggalluvial)
#' (require)library(rgdal)
#' (require)library(dplyr)
#' reclassified_table =
#' Extract_reclassify_rasters(modis_cropped_500,reclass_table_path,reclass_order_path) %>%
#' drop_na()
#'
Extract_reclassify_rasters = function(modis_cropped_500,
                                      reclass_table_path,
                                      reclass_order_path){
  # Extract rasters into one big table containing cell land cover class indices
  all_indices_table = lapply(modis_cropped_500, function(modis_raster){
    terra::as.data.frame(modis_raster, xy = TRUE) %>%
      `names<-`(c("X", "Y", "LC_index")) %>%
      as_tibble()
  }) %>%
    bind_rows(.id = "Varname") %>%
    tidyr::extract("Varname", "Year", "A(\\d+)001", convert = TRUE) %>%
    mutate(LC_index = as.integer(LC_index)) %>%
    print()

  # Load reclassification table and reclassification factor order
  reclass_table = read.csv(reclass_table_path) %>%
    as_tibble()
  reclass_order = read.csv(reclass_order_path) %>%
    mutate_all(.funs = as.factor) %>%
    as_tibble()

  # Reclassify table
  reclassified_table = all_indices_table %>%
    inner_join(reclass_table) %>%
    mutate(LC_class = factor(LC_class, levels = reclass_order$LC_class)) %>%
    print()

  plot(modis_cropped_500[[1]])
  plot(bounding_shapefile, add = TRUE)

  write.csv(reclassified_table, "../Results_example/Files/Modis_reclassified_table.csv", row.names = FALSE)

  reclassified_table
}
