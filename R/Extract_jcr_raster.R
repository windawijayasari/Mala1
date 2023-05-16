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
