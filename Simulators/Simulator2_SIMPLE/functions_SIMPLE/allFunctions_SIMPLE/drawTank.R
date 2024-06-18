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
  type = c("species", "hauls", "weight", "volume")
  ){
  drawTank <- TANK %>% 
    reshape2::melt()
  
  if(plot == 1){
    if(type == "species"){
      drawTank <- drawTank %>% 
        filter(Var3 == 2) 
      
      drawTankPlot <- ggplot(data = drawTank  %>% 
                               mutate(
                                 value = as.factor(ifelse(is.na(value), 0, value))
                               ), aes(Var2, Var1, fill = value)) +
        geom_tile() +
        guides(fill = guide_legend(title = "Species")) + 
        scale_fill_manual(
          labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
          values = c("1" = "blue", "2" = "red", "0" = "black")
        ) 
      
    } else if(type == "weight"){
      
      drawTank <- drawTank %>% 
        dplyr::filter(Var3 == 3)%>% 
        mutate(
          value = as.numeric(value)
        ) 
      drawTankPlot <- ggplot(data = drawTank, aes(Var2, Var1, fill = value)) +
        geom_raster(data =  drawTank, aes(Var2, Var1, fill = value)) +
        guides(fill = guide_legend(title = "Weight")) +
        scale_fill_viridis_c(option = "magma", na.value = "black") 
      
    } else if(type == "volume"){

      drawTank <- drawTank %>% 
        dplyr::filter(Var3 == 4)%>% 
        mutate(
          value = as.numeric(value)
        ) 
      drawTankPlot <- ggplot(data = drawTank, aes(Var2, Var1, fill = value)) +
        geom_raster(data =  drawTank, aes(Var2, Var1, fill = value)) + 
        guides(fill = guide_legend(title = "Volume")) +
        scale_fill_viridis_c(option = "viridis", na.value = "black")
      
    } else if(type == "hauls"){
      drawTank <- drawTank %>%
        filter(Var3 == 5) %>% 
        mutate(
          value = ifelse(is.na(value), "0", value), 
          value = as.factor(value)
        ) 
      drawTankPlot <- ggplot(data = drawTank, aes(Var2, Var1, fill = value)) +
        geom_tile() +
        guides(fill = guide_legend(title = "Haul")) +
        scale_fill_focus(
          c({{unique(drawTank$value)[unique(drawTank$value)!=0]}}),
          color_other = "black"
        ) 
    }
    
    drawTankPlot <- drawTankPlot + 
      labs(y = "Tank height (fishes)", x = "Tank width (fishes)") + 
      coord_equal() + 
      scale_y_reverse(lim = c(tankHeight+1,0)) +
      theme_bw() + 
      theme(
        plot.background = element_rect(fill = "white")
      ) 
    
    drawTankPlot
    
  } else{
    drawTank
  }
}


