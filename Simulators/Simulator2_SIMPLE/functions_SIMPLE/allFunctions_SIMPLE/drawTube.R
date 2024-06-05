########################################################################################
#
# drawTube
# 
########################################################################################
#
# A function to draw numerically or graphically a matrix representing a tube. 
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
# 2024-03-13: Created 
# 
########################################################################################

## P1: Create a function to draw numerically or graphically a matrix representing a tube. 
# Draw the tube
drawTube <- function(
  TUBE,  # This is a pre-existing matrix filled with a catch
  plot = c(1,0), legend = c(1,0) # Should the tube be plotted when ready
  ){
  
  drawTube <- TUBE %>% 
    melt %>%
    filter(Var3 == 1) %>% 
    select(-Var3) %>% 
    dplyr::rename(
      column = Var2,
      row = Var1, 
      value = value
    ) %>% 
    mutate(
      value = as.character(value)
    )
  
  if(plot == 1){
    plotTube <- ggplot(
      data = drawTube, aes(column, row, fill = value)
      ) +
      geom_tile() +
      #facet_wrap(.~fac) +
      labs(y = "Tube height (fishes)", x = "Tube length (fishes)", fill = "Species") + 
      scale_fill_manual(
        labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
        values = c("1" = "blue", "2" = "red", "0" = "black")
      ) + 
      coord_equal() + 
      theme_bw() + 
      theme(
        plot.background = element_rect(fill = "white")
      )
    
    if(legend == 1){
      
      plotTube
      
    } else {
      plotTube + 
        theme(
          legend.position = "none"
        )
    }
    
  } else{
    drawTube
  }
}
