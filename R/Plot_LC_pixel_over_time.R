#' Title
#'
#' @param reclassified_table a csv file from the MODIS data extraction
#'
#'
#' @return an image of land cover over time
#' @export
#'
#' @examples
#' # Countable every year
#'countTable = reclassified_table %>%
#'  group_by(Year, LC_class) %>%
#'  summarise(Count = n()) %>%
#'  print()
#'write.csv(countTable, "../results/countable_every year.csv", row.names = FALSE)
#'
#' #plots the count
#'ggplot(countTable) +
#'  geom_area(aes(Year, Count, group = LC_class, fill = LC_class)) +
#'  scale_fill_manual(values = c("#009e73", "#f0e442", "#d55e00")) +
#'  theme(legend.position="bottom")
#'
#' Pixel_over_time(reclassified_table)

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

