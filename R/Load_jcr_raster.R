#' Title
#'
#' @param jcr_database_path = write path where the jcr data store
#' @param bounding_shapefile_path = write path where the jcr data store
#'
#' @return jcr_cropped_500
#' @export
#'
#' @examples
#' jcr_cropped_500 = Load_jcr_raster(jcr_database_path,bounding_shapefile_path)
#'
Load_jcr_raster = function(jcr_database_path,
                            bounding_shapefile_path){

  # Load shapefile and its CRS (coordinate system)
  bounding_shapefile = vect(bounding_shapefile_path)
  plot(bounding_shapefile)
  bounding_shapefile_crs = crs(bounding_shapefile)

  # List jcr tif files
  jcr_files = list.files(jcr_database_path, pattern = "tif$", full.names = TRUE)
  plot(rast(jcr_files[1]))

  #Reproject the raster
  jcr_rasters = lapply(jcr_files, function(jcr_file){
    a = rast(jcr_file)
    jcr_repojected =
      terra::project(a, bounding_shapefile_crs, method = "bilinear")
    }) %>%
    `names<-`(jcr_files)

  #using resample for jcr
  #extent_to_use <- intersect(extent(jcr_rasters), extent(modis_rasters))
  #jcr_500 = terra::rast(extent = c(762569.6, 826569.6, 9198435, 9251935), resolution = 500)
  jcr_500 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 500)
  #jcr_500 = terra::rast(extent = c(762569.6, 826569.6, 9198435, 9251935), resolution = 500)
  jcr_cropped_500 = lapply(jcr_rasters, function(the_raster){
    jcr_resample = terra::resample(the_raster, r_500, method = "bilinear")
    a = terra::crop(jcr_resample, bounding_shapefile, snap = "out") %>% mask(bounding_shapefile)
    })

  plot(jcr_cropped_500[[1]])
  plot(bounding_shapefile, add = TRUE)

  png(file="../Results_example/JCR_cropped_raster_2001.png")
  plot(jcr_cropped_500[[1]])
  plot(bounding_shapefile, add = TRUE)
  dev.off()

  pdf(file="../Results_example/JCR_cropped_raster_2001.pdf")
  plot(jcr_cropped_500[[1]])
  plot(bounding_shapefile, add = TRUE)
  dev.off()

  jcr_cropped_500
}
