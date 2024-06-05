########################################################################################
#
# calculateProportionMatrix
# 
########################################################################################
#
# A function to calculate numerically the proportion in a given matrix.
# or at given locations in a matrix. 
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

## P1: Create a function to calculate numerically and/or graphically the proportion in a given matrix or at given locations in a matrix. 
calculateProportionMatrix <- function(
  mat, # The matrix from which obtain the proportions
  plot = c(1,0),
  partial = c(1,0), # If 1, specify the sensor position along the main matrix axis (see typeMatrix)
  sensor = c(1,25,50,75,100), # Percentage at height of the tank at which you want to monitor the percentage. Note, you will select just one row!
  typeSensor = c("single", "interval"), 
  typeMatrix = c("tank", "tube") # When tank the sensors are disposed horizontally while when tube vertically 
){
  
  if(partial == 0){
    
    if(typeMatrix == "tube"){
      # Turn zero to NA
      mat <- ifelse(mat == 0, NA, mat)
    }
    
    pHerring <- as.numeric(prop.table(table(mat == 1))["TRUE"])
    pSprat <- as.numeric(prop.table(table(mat == 2))["TRUE"])
    
    # Extract the number of fishes in the set of hauls 
    for(h in 1:nHaul){
      nFish_temp <- length(haulsList[[h]]$fishes[,,2])
      if(h == 1){
        nFish <- nFish_temp
      }else{
        nFish <- c(nFish, nFish_temp)
      }
    }
    # Extract total number of fishes
    nFishes_all <- sum(nFish)
    pFlowPlotDf <- data.frame(
      station = ifelse(typeMatrix == "tank", c("tank_all", "tank_all"), c("tube_all", "tube_all")),
      species = c("herring", "sprat"),
      proportion = c(pHerring, pSprat)
    )
      
      if(typeMatrix == "tube"){ 
        mat <- ifelse(is.na(mat), 0, mat) # Turn again NA to zero
      }
    
  } else {
    if(typeMatrix == "tank"){
     
      divisionFactor = case_when( # Remember that we are looking at the matrix upside down in the tank case
        sensor == 1 ~ nrow(mat)^(-1),
        sensor == 25 ~ 0.25, 
        sensor == 50 ~ 0.50,
        sensor == 75 ~ 0.75,
        sensor == 100 ~ 1
      )  
    
    } else {
      if(typeSensor == "single"){
        
        divisionFactor = case_when( # Remember that we are looking at the matrix as it is in the tube case
          sensor == 1 ~ ncol(mat)^(-1),
          sensor == 25 ~ 0.25, 
          sensor == 50 ~ 0.50,
          sensor == 75 ~ 0.75,
          sensor == 100 ~ 1
        )  
        
      } else{
        
        ## Possible intervals
        listIntervals <- list(
          interval_1 = 1:(ncol(mat)*.25),
          interval_2 = (ncol(mat)*.25):(ncol(mat)*.5), 
          interval_3 = (ncol(mat)*.5):(ncol(mat)*.75),
          interval_4 = (ncol(mat)*.75):(ncol(mat))
        )
        
        ## Determine what interval suits best
        interval = ifelse(sensor == "01_25",
                          listIntervals[1],
                          ifelse(
                            sensor == "25_50",
                            listIntervals[2],
                            ifelse(
                              sensor == "50_75",
                              listIntervals[3],
                              ifelse(
                                sensor == "75_100",
                                listIntervals[4]
                              ))))
        
        interval <- unlist(interval) 
       
      }
    }
    if(typeMatrix == "tank"){
      
      if(typeSensor == "single"){
        
        mat[-nrow(mat)*divisionFactor,,] <- NA # All except the row selected turns into NA  
        
      } else {
        
        mat[-(interval),,] <- NA # All except the interval selected turns into NA  
      }
      
    }else{
      
      if(typeSensor == "single"){
        
        mat[,-ncol(mat)*divisionFactor,] <- NA # All except the selected columns into NA
        
      } else {
        
        mat[,-(interval),] <- NA # All except the interval selected turns into NA  

      }
      
      # Turn zero to NA
      mat <- ifelse(mat == 0, NA, mat)
      
      }
    
    pHerring <- as.numeric(prop.table(table(mat == 1))["TRUE"])
    pSprat <- as.numeric(prop.table(table(mat == 2))["TRUE"])
    # Extract the number of fishes in the set of hauls 
    for(h in 1:nHaul){
      nFish_temp <- length(haulsList[[h]]$fishes[,,2])
      if(h == 1){
        nFish <- nFish_temp
      }else{
        nFish <- c(nFish, nFish_temp)
      }
    }
    # Extract total number of fishes
    nFishes_all <- sum(nFish)
    
    if(typeMatrix == "tube"){ 
      mat <- ifelse(is.na(mat), 0, mat) # Turn again NA to zero
    }
    
    if(typeMatrix == "tank"){
      
      pFlowPlotDf <- data.frame(
        station = c(paste0("tankSensor_",sensor), paste0("tankSensor_",sensor)),
        species = c("herring", "sprat"),
        proportion = c(pHerring, pSprat)
        
      ) 
    }else{
      pFlowPlotDf <- data.frame(
        station = c(paste0("tubeSensor_",sensor), paste0("tubeSensor_",sensor)),
        species = c("herring", "sprat"),
        proportion = c(pHerring, pSprat)
      ) 
    }
  
  }
  
  if(plot == 1){ # To plot rapidly the matrix and see the selected interval
    if(typeMatrix == "tank"){
      print(drawTank(mat, plot = plot, type = "species")) 
    }else{
      print(drawTube(mat, plot = plot, legend = 1))
    }
  }
  
  pFlowPlotDf
  
  
}


