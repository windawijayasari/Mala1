Pixel_over_time = function(reclassified_table){
  
  alluviumdata = reclassified_table %>% 
    filter(Year %in% c(2001,2020)) %>%
    mutate(pixel = paste(X, Y, sep = "_")) %>%
    rename(LandCover = LC_class)
  LC_overtime<-
  ggplot(alluviumdata,
         aes(x = Year, stratum = LandCover, alluvium = pixel,
             fill = LandCover, label = LandCover)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback") +
    geom_stratum(color = "darkgray") +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("#009e73", "#f0e442", "#d55e00"), 
                      name = "Land Cover") + 
    ylab("Pixel count") + 
    ggtitle("")
  
  print(LC_overtime)
  
  png(file="../Results_example/LC_pixel_over_time.png")
  print(LC_overtime)
  dev.off()
  
  pdf(file="../Results_example/LC_pixel_over_time.pdf")
  print(LC_overtime)
  dev.off()
}

