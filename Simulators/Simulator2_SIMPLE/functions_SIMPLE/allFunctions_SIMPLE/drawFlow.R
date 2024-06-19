########################################################################################
#
# drawFlow
# 
########################################################################################
#
# A function to draw graphically the flow between two matrices representing a tank and a tube 
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

## P1: Create a function to draw graphically the flow between two matrices representing a tank and a tube 
# Draw the flow
drawFlow <- function(
  TANK, # A matrix representing a tank, possibly filled
  TUBE, # A matrix representing a tube, 
  multipleVars = c(1,0), # Should the plot include weight and volume at side of
  pIndicator = c(1,0), 
  sizeLabelText = 1, 
  type = c("species", "weight", "volume")
  ){
  
  ## First organize the proportion panel 
  pIndicatorDf <- rbind(
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 0,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 1,
      sensor= 1,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 1,
      sensor= 25,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 1,
      sensor= 50,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 1,
      sensor= 75,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TANK, 
      plot = 0,
      partial = 1,
      sensor= 100,
      typeSensor = "single",
      typeMatrix = "tank"
    ),
    calculateProportionMatrix(
      TUBE, 
      plot = 0,
      partial = 0,
      sensor= "None",
      typeSensor = "None",
      typeMatrix = "tube"
    ),
    calculateProportionMatrix(
      TUBE, 
      plot = 0,
      partial = 1,
      sensor= "01_25",
      typeSensor = "interval",
      typeMatrix = "tube"
    ),
    calculateProportionMatrix(
      TUBE, 
      plot = 0,
      partial = 1,
      sensor= "25_50",
      typeSensor = "interval",
      typeMatrix = "tube"
    ),
    calculateProportionMatrix(
      TUBE, 
      plot = 0,
      partial = 1,
      sensor= "50_75",
      typeSensor = "interval",
      typeMatrix = "tube"
    ),
    calculateProportionMatrix(
      TUBE, 
      plot = 0,
      partial = 1,
      sensor= "75_100",
      typeSensor = "interval",
      typeMatrix = "tube"
    )
  ) %>% 
    mutate(
      species = ifelse(is.na(proportion), "empty", species),
      species = factor(species, levels = c("sprat", "herring", "empty")),
      proportion_plot = ifelse(is.na(proportion), 1, proportion),
      station = factor(
        station, 
        levels = c(
          "tube_all", "tubeSensor_75_100", "tubeSensor_50_75", "tubeSensor_25_50", "tubeSensor_01_25",
          "tank_all", "tankSensor_100", "tankSensor_75", "tankSensor_50", "tankSensor_25", "tankSensor_1"
        )
      )
    )
  
  # Check the presence of NA: when only one species of the two on a sensor is NA it should be dropped, or a black ribbon will appear. 
  checkNa <- lapply(split(pIndicatorDf, pIndicatorDf$station), function(x) ifelse(sum(is.na(x$proportion)) == 1, 1,0))
  checkNaNames <- names(checkNa[checkNa == 1])
  pIndicatorDf <- pIndicatorDf[!(pIndicatorDf$station %in% checkNaNames & is.na(pIndicatorDf$proportion)),]
  
  # Second organize the plots 
  p <- cowplot::ggdraw() + #https://stackoverflow.com/questions/68238373/plotting-a-map-with-a-zoomed-mini-map
    suppressMessages(coord_equal(xlim = c(-10, 30), ylim = c(-10, 12), expand = FALSE)) +
    annotation_custom(
      ggplotGrob(
        suppressMessages(
          drawTank(TANK, plot = 1, type = type) + 
            theme(
              axis.text = element_text(size = 8),
              axis.title = element_text(size = 9),
              legend.text =  element_text(size = 8),
              legend.title = element_text(size = 9),
              legend.position = "top", 
              legend.direction = "horizontal", 
              legend.key.height = unit(.1, "cm"),
              legend.key.width = unit(.25, "cm") 
              ))
        ), 
      xmin = -30, xmax = 20, ymin = -3, ymax = 12
      ) +
    annotation_custom(
      ggplotGrob(
        suppressMessages(
          drawTube(TUBE, type = type, plot = 1, legend = 0)) + 
          labs(y = "\n", x = "") + 
          theme(
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 9)
          )
        ), xmin = 0, xmax = 30, ymin = -10, ymax = -5) +
    geom_polygon(
      data = data.frame(
        x = c(-7.5,-4.5,-1), 
        y = c(-3,-3.5,-3), 
        group = c(1,1,1)
      ),
      aes(
        x=x
        ,y=y
        ,group=group
      )
    ) +
    geom_polygon(
      data = data.frame(
        x = c(-5,-5,-4,-4), 
        y = c(-3,-7.5,-7.5,-3), 
        group = c(1,1,1,1)
      ),
      aes(
        x=x
        ,y=y
        ,group=group
      )
    ) +
    geom_polygon(
      data = data.frame(
        x = c(-4,-4,0,0), 
        y = c(-6.75,-7.5,-7.5,-6.75), 
        group = c(1,1,1,1)
      ),
      aes(
        x=x
        ,y=y
        ,group=group
      )
    ) +
    theme(
      plot.background = element_rect(fill="white", color = "NA") 
    ) 
    if(multipleVars == 1){
      p <- p +  
      annotation_custom(
        ggplotGrob(
          suppressMessages(
            drawTank(TANK, plot = 1, type = "volume") +
              theme(
                axis.text = element_text(size = 8),
                axis.title = element_text(size = 9),
                legend.text =  element_text(size = 8),
                legend.title = element_text(size = 9),
                legend.position = "top",
                legend.direction = "horizontal",
                legend.key.height = unit(.1, "cm"),
                legend.key.width = unit(.25, "cm")
              ))
        ), xmin = 0, xmax = 30, ymin = -3, ymax = 12
      ) +
       annotation_custom(
          ggplotGrob(
            suppressMessages(
              drawTank(TANK, plot = 1, type = "weight") + 
                theme(
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 9),
                  legend.text =  element_text(size = 8),
                  legend.title = element_text(size = 9),
                  legend.position = "top", 
                  legend.direction = "horizontal", 
                  legend.key.height = unit(.1, "cm"),
                  legend.key.width = unit(.25, "cm") 
                ))
          ), xmin = -20, xmax = 30, ymin = -3, ymax = 12
        ) +
        annotation_custom(
          ggplotGrob(
            suppressMessages(
              drawTube(TUBE, type = "weight", plot = 1, legend = 0)) + 
              labs(y = "\n", x = "") + 
              theme(
                axis.text.y = element_text(size = 8),
                axis.text.x = element_blank(),
                axis.title.y = element_text(size = 9),
                axis.title.x = element_blank(),
              )
          ), xmin = 0, xmax = 30, ymin = -10, ymax = -1) +
        annotation_custom(
          ggplotGrob(
            suppressMessages(
              drawTube(TUBE, type = "volume", plot = 1, legend = 0)) + 
              labs(y = "\n", x = "") + 
              theme(
                axis.text.y = element_text(size = 8),
                axis.text.x = element_blank(),
                axis.title.y = element_text(size = 9),
                axis.title.x = element_blank(),
              )
          ), xmin = 0, xmax = 30, ymin = -10, ymax = 2) 
    } 
    if(pIndicator == 1){
      p <- p +  annotation_custom(
        ggplotGrob(
          suppressMessages(
              plotProportionMatrix(
                pIndicatorDf,
                sizeLabelText = sizeLabelText
                )  + 
                theme(
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 9)
                )
              )
        
        ), xmin = 20, xmax = 30, ymin = -3, ymax = 10
      )
    }
  
  p 
  
}










