########################################################################################
#
# buildTank
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
# Build the tank
buildTank <- function(
  tankHeight,
  tankWidth,
  bigTank, 
  plot = c(1,0)
  ){
  
  tank <- matrix(NA,tankHeight,tankWidth)
  
  if(plot == 1){
    if(bigTank == 1){
      tankR <- apply(tank, 2, rev)
      print(image(t(tankR)))
    }else{
      print(drawTank(tank, plot = 1, type = "species"))  
    }
  }
  
  tank
  
  }
