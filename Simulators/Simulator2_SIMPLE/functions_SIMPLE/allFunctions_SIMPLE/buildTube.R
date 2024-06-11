########################################################################################
#
# buildTube
# 
########################################################################################
#
# A function to draw numerically or graphically a matrix representing a tube
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

## P1: Create a function to draw a matrix representing a tube of specified dimensions. 
# Build the tube
buildTube <- function(
  heightTube,
  lengthTube,
  plot = c(1,0)
  ){
  
  flowtube <- array(0, list(heightTube,lengthTube,5))
  
  if(plot == 1){
    suppressMessages(print(drawTube(flowtube, plot = 1, legend = 1, type = "species")))   
  }
  
  flowtube
  
  }
