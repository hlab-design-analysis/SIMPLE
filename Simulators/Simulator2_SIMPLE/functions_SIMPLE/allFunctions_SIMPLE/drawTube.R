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
  plot = c(1,0), legend = c(1,0), # Should the tube be plotted when ready
  type = c("species", "weight", "volume")
  ){
  if(type == "species"){
    
    drawTube <- TUBE %>% 
      melt %>%
      filter(Var3 == 2) %>% 
      dplyr::select(-Var3) %>% 
      dplyr::rename(
        column = Var2,
        row = Var1, 
        value = value
      ) %>% 
      mutate(
        value = as.character(value)
      )
    
  } else if(type == "weight"){
    
    drawTube <- TUBE %>% 
      melt %>%
      filter(Var3 == 3) %>% 
      dplyr::select(-Var3) %>% 
      dplyr::rename(
        column = Var2,
        row = Var1, 
        value = value
      ) %>% 
      mutate(
        value = as.numeric(value),
        value = ifelse(value == 0, NA, value)
      )
    
  } else if(type == "volume"){
    
    drawTube <- TUBE %>% 
      melt %>%
      filter(Var3 == 4) %>% 
      dplyr::select(-Var3) %>% 
      dplyr::rename(
        column = Var2,
        row = Var1, 
        value = value
      ) %>% 
      mutate(
        value = as.numeric(value),
        value = ifelse(value == 0, NA, value)
      )
    
  }

  
  if(plot == 1){
    
    if(type == "species"){
      
      plotTube <- ggplot(
        data = drawTube, aes(column, row, fill = value)
      ) +
        geom_tile() + 
        scale_fill_manual(
          labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
          values = c("1" = "blue", "2" = "red", "0" = "black")
        )
    
    }
      
    if(type == "weight" | type == "volume"){
      
      if(all(is.na(drawTube$value))){
        drawTube <- drawTube %>% mutate(value = ifelse(is.na(value), "0", value))
        plotTube <- ggplot() +
          geom_raster(data =  drawTube, aes(column, row, fill = value)) +
          labs(y = "Tube height (fishes)", x = "Tube length (fishes)", fill = type) +
          scale_fill_manual(values = c("0" = "black"))
        
      } else{
        plotTube <- ggplot() +
          geom_raster(data =  drawTube, aes(column, row, fill = value)) +
          labs(y = "Tube height (fishes)", x = "Tube length (fishes)", fill = type) + 
          scale_fill_viridis_c(option = ifelse(type == "weight", "magma", "viridis"), na.value = "black")
      }
     
      
    }
    
      plotTube <- plotTube +
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

