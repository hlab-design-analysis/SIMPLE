########################################################################################
#
# plotProportionMatrix
# 
########################################################################################
#
# A function to plot the proportion in a given matrix or at given locations in a matrix. 
#
########################################################################################
#
#
# Authors:
#
# - Eros Quesada [first draft]
# 
# Dev. Notes.
#
#
# 2024-03-15: Created 
# 
########################################################################################

## P1: Create a function to plot the proportion in a given matrix or at given locations in a matrix. 
plotProportionMatrix <- function(
  mat, # The matrix from which obtain the proportions
  sizeLabelText = 1
  ){
  
  p <- ggplot() + 
    geom_bar(data = mat, aes(fill=species, y=proportion_plot, x=station), position="fill", stat="identity") + 
    geom_label(data = mat %>% filter(species == "sprat"), aes(fill=species, y=.90, x=station, label = paste0("pS: ", round(proportion, 2))), size = 30*sizeLabelText, label.padding = unit(.5*sizeLabelText, "lines")) + 
    geom_label(data = mat %>% filter(species == "herring"), aes(fill=species, y=.10, x=station, label = paste0("pH: ", round(proportion, 2))), color = "white", size = 30*sizeLabelText, label.padding = unit(.5*sizeLabelText, "lines")) + 
    labs(y = "proportion (weight)", x = "sensor") +
    coord_flip() + 
    scale_fill_manual(
      values = c("herring" = "blue", "sprat" = "red", "empty" = "black")
    ) +
    theme_bw() + 
    theme(
      legend.position = "none", 
      axis.text.y = element_text(face="bold")
    )
  
}
