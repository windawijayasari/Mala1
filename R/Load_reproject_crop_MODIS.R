#'Title Load_reproject_crop_MODIS
#'
#' @param modis_database_path path Where all files from MODIS satellite data are stored
#' @param dataset_selected type of MODIS dataset, in this case LC_type1
#' @param bounding_shapefile_path path Where the boundary shapefile of the area are stored
#'
#' @description
#' This function erform these operations: (1) load as a raster collection, (2) select dataset,
#' (3) reproject raster into boundary's CRS, (4) crop raster, (5) mask raster
#' Although the produced raster is a cropped then masked raster, henceforth,
#' it is called cropped raster for clarifty purpose.
#' It will return all the MODIS files cropped and reprojected.
#' The plot of the first file is just the file example.
#'
#'
#' @return modis_cropped_rasters
#' @export
#'
#' @examples
#' modis_cropped_rasters = Load_reproject_crop_MODIS(modis_database_path, dataset_selected = "LC_Type1", bounding_shapefile_path)
#'
Load_reproject_crop_MODIS = function(modis_database_path,
                                     dataset_selected = "LC_Type1",
                                     bounding_shapefile_path){
  # Load shapefile and its CRS (coordinate system)
  bounding_shapefile = vect(bounding_shapefile_path)
  plot(bounding_shapefile)
  bounding_shapefile_crs = crs(bounding_shapefile)

  # List MODIS hdf files
  modis_hdf_files = list.files(modis_database_path, full.names = TRUE)

  #Crop and reproject raster
  modis_cropped_rasters = lapply(modis_hdf_files, function(modis_file){
    modis_raster = rast(modis_file)[dataset_selected]
    modis_repojected =
      terra::project(modis_raster, bounding_shapefile_crs, method = "near")
    modis_cropped = crop(modis_repojected, bounding_shapefile, snap = "OUT")
    modis_masked = mask(modis_cropped, bounding_shapefile)
  }) %>%
    `names<-`(modis_hdf_files)

  plot(modis_cropped_rasters[[1]])
  plot(bounding_shapefile, add = TRUE)

  png(file="../Results_example/Modis_cropped_raster_2001.png")
  plot(modis_cropped_rasters[[1]])
  plot(bounding_shapefile, add = TRUE)
  dev.off()

  pdf(file="../Results_example/Modis_cropped_raster_2001.pdf")
  plot(modis_cropped_rasters[[1]])
  plot(bounding_shapefile, add = TRUE)
  dev.off()

  modis_cropped_rasters
}
