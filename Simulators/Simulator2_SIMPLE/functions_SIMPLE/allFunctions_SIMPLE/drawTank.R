########################################################################################
#
# drawTank
# 
########################################################################################
#
# A function to draw numerically or graphically a matrix representing a tank.
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

## P1: Create a function to draw a matrix representing a tank of specified dimensions. 
# Draw the tank
drawTank <- function(
  TANK, # This is a pre-existing matrix filled with a catch
  plot = c(1,0), # Should the tank be plotted when ready?
  type = c("species", "hauls")
  ){
  drawTank <- TANK %>% 
    reshape2::melt() %>% 
    mutate(
      value = as.factor(ifelse(is.na(value), 0, value))
    )
  
  if(plot == 1){
    ggplot(data = drawTank, aes(Var2, Var1, fill = value)) +
      geom_tile() +
      #facet_wrap(.~fac) +
      labs(y = "Tank height (fishes)", x = "Tank width (fishes)") + 
      coord_equal() + 
      scale_y_reverse(lim = c(tankHeight+1,0)) +
      theme_bw() + 
      theme(
        plot.background = element_rect(fill = "white")
      ) +
      guides(fill = guide_legend(title = ifelse(type == "species", "Species", "Haul"))) + 
      if(type == "species"){
        scale_fill_manual(
          labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
          values = c("1" = "blue", "2" = "red", "0" = "black")
        ) 
      } else if(type == "hauls"){
        scale_fill_focus(
          c({{unique(melt(TANK)$value)[unique(melt(TANK)$value)!=0]}}),
          color_other = "black"
        ) 
      }
  } else{
    drawTank
  }
}
