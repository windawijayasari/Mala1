#' Title
#'
#' @param jcr_cropped_500 the JCR data after resample to 500m resolution
#'
#' @return the table containing X, Y point and water extent of the X,Y point
#' @export
#'
#' @examples
#' all_jcr_table = Extract_jcr_rasters(jcr_cropped_500)

Extract_jcr_rasters = function(jcr_cropped_500){
  # Extract rasters into one big table containing cell jcr value
  all_jcr_table = lapply(jcr_cropped_500, function(jcr_raster){
    terra::as.data.frame(jcr_raster, xy = TRUE) %>%
      `names<-`(c("X", "Y", "value")) %>%
      as_tibble()

  }) %>%
    bind_rows(.id = "Varname") %>%
    tidyr::extract("Varname", "Year", "JCR_CitarumHulu(\\d+)", convert = TRUE) %>%
    print()

 all_jcr_table
}
