LCC_bar_plot = function(reclassified_table){
  #plot reclasified table in bar plot
  bar_plot = ggplot(reclassified_table, aes(Year, fill=LC_class))+
    scale_fill_manual(values=c("#009e73", "#f0e442", "#d55e00"))+
    geom_bar(position = 'fill')
  
  plot(bar_plot)
  
  png(file="../Results_example/Plot Modis in bar.png")
  plot(bar_plot)
  dev.off()
  
  pdf(file="../Results_example/Plot Modis in bar.pdf")
  plot(bar_plot)
  dev.off()
  
  
  bar_plot
}

