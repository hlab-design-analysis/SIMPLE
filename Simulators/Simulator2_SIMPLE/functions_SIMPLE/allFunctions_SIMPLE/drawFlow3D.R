########################################################################################
#
# drawTank
# 
########################################################################################
#
# A function to draw a 3d animated plot showing the flow of fishes by species and weight
# into a pipe. 
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
# 2024-06-05: Created 
# 
########################################################################################

## P1: Write a function to draw a 3d animated plot showing the flow of fishes by species and weight into a pipe
drawFlow3D <- function(flowTubeList, speed){
  
  # Turn the flowtube list into a list of wide df reporting weight and species, with a timestep indication  
  flowTubeList <- lapply(1:length(flowTubeList), function(x){
    cbind(
      flowTubeList[[x]] %>% 
        melt() %>% 
        filter(Var3 == 3) %>% 
        rename(
          Weight = value
        ), 
      flowTubeList[[x]] %>% 
        melt() %>% 
        filter(Var3 == 2) %>% 
        rename(
          Species = value
        ) %>% 
        select(
          Species
        )
      ) %>% 
      mutate(
        timeStep = as.numeric(x)
        )
      })
  
  # Turn the list into a df
  flowTubeDf <- do.call(rbind, flowTubeList)
  
  # Specify species as character
  flowTubeDf$Species = ifelse(a$Species == 1, "herring", ifelse(a$Species == "2", "sprat", "empty"))
  
  # Delete timestamp 1 - due to a known issue with plotly (https://github.com/plotly/plotly.R/issues/1696)
  flowTubeDf <- flowTubeDf %>% filter(timeStep != 1)
  
  p <- plot_ly(flowTubeDf, x = ~Var1, 
          y = ~Var2, 
          z = ~Weight, 
          color = ~Species, 
          frame = ~timeStep, 
          colors = c("herring" = "blue", 
                     "sprat" = "red",
                     "empty" = NA),
          type = "scatter3d", 
          mode = "markers") %>% 
    add_markers() %>%
    layout(
      xaxis = list(
        range=c(0,100)
      )
    ) 
  
  
  animation_opts(
    p, 
      frame = speed
    )
  
}


