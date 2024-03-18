########################################################################################
#
# SIMPLE - SIM2
# 
########################################################################################
#
# This script produce a simulation of small pelagic catch flow in 2D, based on matrices.
# 
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
# 2024-03-03: Created. Developed tank, fish pourer/sinker, tube and flow between them
# 2024-03-13: Add proportions at different steps in tank and tube on the plot  
# 2024-03-15: Add the possibility of including more than one haul and proportion sensors. 
# 
# 
#
########################################################################################
#
# Settings ----- 
# Here we tune some settings and indicate relevant directories on our machines.
#
########################################################################################

## P1: Set wd, where SIMPLE folder is
setwd("~/Personal_Eros_locale/SIMPLE/Simulators/Simulator2_SIMPLE")

## P2: Indicate directory to store intermediate results (should be a place able to allocate large files e.g. 70gb)
supportResultsDir <- "~/mnt/CNAS/SIMPLE_Auxiliary"

## P3: Indicate directory to store results
ResultsDir <- "Results"

## P4: Load the additional settings
source("settings_SIMPLE/settings_SIMPLE.R")

########################################################################################
#
# Libraries ----- 
# Here we load useful libraries.
#
########################################################################################

## P1: Load libraries -----
source("libraries_SIMPLE/libraries_SIMPLE.R")

########################################################################################
#
# Custom functions ----- 
# Here we load useful custom functions
#
########################################################################################

## Load custom functions 
source("functions_SIMPLE/functionsLink_SIMPLE.R")

########################################################################################
#
# Set parameters -----
# Here we set the initial condition for the simulation
#
########################################################################################

## P1: Choose parameters values 
# Catch properties
nHaul = 3 # Number of hauls
p_herring = c(0.1, 0.2, 0.3) # Proportion of herring. If nHaul > 1, use vector with one value per each
prob = c(p_herring, p_sprat)
N = c(5000, 200000, 100000) # Catch of both species If nHaul > 1, use vector with one value per each

# Tank properties
tankHeight = 700
tankWidth = 500
bigTank = 0

# Tube properties
heightTube = 10 
lengthTube = 100

## P2: Alternatively pick one of the following and load the parameters from script script simulationLog.R
#
## Simulation 1
# In this simulation a catch of 50000 fishes with proportion 0.1 herring and 0.9 sprat is caught in one single haul that flows into the tube. 
# Fishes are completely mixed in the process and no length or weight of them is considered. 
#
# Time estimated: 
# - 1.5 days when resulting plot are stored to produce animation.
# - few minutes when just numeric results are stored. 
#
#
## Simulation 2
# In this simulation a catch of 329800 fishes with proportion 0.1 herring and 0.9 sprat is caught in one single haul that flows into the tube. 
# Fishes are completely mixed in the process and no length or weight of them is considered. 
# The value of 329800 has been choosen as it represent the mean number caught in 2023 per trip (max was 133850000), if we consider the fishes weight being 10gr.
# This is consistent with statistics reported in Eero 2012 in case of Sprat (weight across all ages): https://academic.oup.com/icesjms/article/69/6/1010/618064.

# Time estimated: 
# - couple of days when resulting plot are stored to produce animation.
# - couple of minutes when just numeric results are stored. 
#

## Simulation 3
# In this simulation a catch of 305000 fishes in three hauls (N = c(5000, 200000, 100000)) with different proportions (pH = c(0.1, 0.2, 0.3))  flows into the tube. 
# Fishes in the tank are deposited in layers corresponding with hauls with random mixing at the border, depending on missing values at the cell below the single fish at the moment of pouring.
#
# Time estimated: 
# - Few minutes without images.
# - 15h with images
#
## Load the pre-existing 
SimulationFromLog = 0 # Turn on: 1
Simulation = 1 # Choosen simulation

source("~/Personal_Eros_locale/SIMPLE/Simulators/Simulator2_SIMPLE/simulationLog_SIMPLE/simulationLog.R")

########################################################################################
#
# Create the haul list -----
# Here we create an haul list
#
########################################################################################
## P1: Make the haul list
# The haul list is created starting from the parameters specified in "Set parameters". 
# It reports a list of hauls, with each haul being a list of catch, proportion of herring
# and proportion of sprat. 
haulsList <- makeHaulsList(
  nHaul = nHaul, 
  p_herring = p_herring,
  N = N
)

########################################################################################
#
# Additional calculations ----
# Here we set the initial condition for the simulation
#
########################################################################################
## P1: Assign the species to the fishes caught based on proportions and quantity
# Given the probability on n particles we expect n*prob fishes belonging to each species (https://stackoverflow.com/questions/34186755/sample-vector-exactly-according-to-the-probability-given)
for(i in 1:length(haulsList)){
  haulsList[[i]] <- c(
    # Here what we need to do is to append to each haul in the haul list a vector containing two categories with a length equal to N and a proportion equal to the ones of the species as indicated by the parameters
    haulsList[[i]], # Basically speciesVector = rep(c("herring","sprat"), round(N * c(p_herring, p_sprat)))
    list(
      speciesVector = rep(
        c("herring","sprat"), 
        round(
          as.numeric(haulsList[[i]][1]) * as.numeric(c(haulsList[[i]][2], haulsList[[i]][3]))
          )
        )
      )
    )    
  }

## P2: Turn in numbers the species
# Turn the speciesVector in the haulList into numbers (herring = 1, sprat = 2) to optimize memory
for(i in 1:length(haulsList)){
  haulsList[[i]][4] <- list(ifelse(as.character(unlist(haulsList[[i]][4])) == "herring", 1, 2))
  }

## P3: Try to extract n samples
#  If we sample from these a very high number of times, we should obtain the probability given.
#  For instance the proportions in haul 1 were
haulsList[[1]][2] # For herring
haulsList[[1]][3] # For sprat
replics = 100000  # Thus resampling 100000 times from our speciesVector in haul 1 we expect to reach these numbers
#prop.table(table(replicate(replics,dqsample(as.numeric(unlist(haulsList[[1]][4])), 1, replace = F))))

########################################################################################
#
# Build the tank ----
# Here we build the tank according to selections
#
########################################################################################
## P1: Build the tank.
# Note that if more than one haul is present the tank will be rebuilt below, with the 
# same tankHeight and tankWidth. 
tank <- buildTank(
  tankHeight = tankHeight, # 700
  tankWidth = tankWidth, # 500
  bigTank = bigTank, 
  plot = 1
)

########################################################################################
#
# Pour fishes in tank ----
# Here we pour a stream of fishes from the haulsList to the tank
# This is done by selecting randomly positions in the tank matrix (n = n fishes) and 
# assigning fishes to them. Then the fishes are sunk to the bottom of the tank matrix. 
# If more than one haul is present, the magnitude of the hauls determine the position of 
# the fishes belonging to them. In practice the tank matrix is divided in submatrices and
# each submatrix is assigned to an haul. The extension of the submatrix is proportional to 
# the magnitude of the haul. The reasoning about the assignation of fishes belonging to one 
# haul to the relative submatrix is the same as in the one haul case. The fishes are finally 
# sunk to the bottom all together. 
#
########################################################################################
## P1: Pour the fishes in the tank
# First, we need to take into account the eventual presence of different hauls and their magnitude. 
if(nHaul == 1){
  # Select position at random in the tank matrix 
  positions <- dqsample(length(tank), N[1])
  
  # Fill these position with the fishes in the haul 
  tank[positions] <- as.numeric(unlist(haulsList$Haul_1$speciesVector))
  
  # Show the results (one could imagine that this is a random time when fishes are falling into the tank from above)
  if(bigTank == 1){
    tankR <- apply(tank, 2, rev)
    image(t(tankR))
  }else{
    drawTank(tank, plot = 1)  
  }
  
} else {
  
  ## In case more than one haul are involved it is a bit trickier, due to the way R indexes matrices. 
  # First we need to assign increasing numbers by row to the tank matrix, as R use columns to index. 
  tank <- matrix(1:(tankHeight*tankWidth),tankHeight,tankWidth,byrow = T) # Fill it with numbers from first two last cell, R will fill column
  tank <- tank[nrow(tank):1,] # To mimic that the bottom is the first to fill, we flip it also vertically
  # Now the bottom left cell of the matrix value is 1, the right top cell value is length(tank) - the maximum. 
  
  # Second, we divide the tank matrix proportionally to the magnitude of the haul.
  propTank <- floor(length(tank)*prop.table(N)) 

  # Third we create a list where to store the position by haul 
  positions = as.list(rep(NA, nHaul))
  names(positions) <- paste0("Haul_", 1:length(positions))
    
  # Allow to sample by haul only in the region of cells proportional to the magnitude of the haul
  # Start from the bottom with the first haul

  for(i in 1:nHaul){
    
    if(i == 1){ # When i equal to 1, we treat the first haul hence we need to start from bottom of matrix

      # Identify the cell value until, starting from the bottom of the tank, which the tank matrix may be occupated
      regionOccupable_start <- 1 # Indeed the first cell to start is the one bottom left, for which tank == 1. 
      regionOccupable_end <- propTank[1] # propTank[1] is the limit cell of the region that the haul 1 is allowed to occupy given its magnitude.
      
      # Select position at random in the tank matrix
      positions[i] <- list(
        dqsample(
          which(
            tank %in% regionOccupable_start:ifelse(
                regionOccupable_end<regionOccupable_start,
                (regionOccupable_start + regionOccupable_end),
                regionOccupable_end
                )
            ), 
            N[i], 
            replace = F
            )
          )
      
      # Assign the fishes in the haul to the positions
      tank[as.numeric(unlist(positions[i]))] <- ifelse(as.numeric(unlist(haulsList[[i]][4])) == 1, "herring", "sprat") # Here we need to go back to character notation to avoid confusion between numbers used for index and 1 and two used for the species
      
    } else { # When between 1 and nHaul we need to start from the previous haul to the next
      
      # Update the cell value with the value until, starting from the value of the previous haul of the tank,  the tank matrix may be occupied
      regionOccupable_start <- propTank[i-1]
      regionOccupable_end <- propTank[i]
      
      # Select position at random in the tank matrix
      positions[i] <- list(
        dqsample(
          which(
            tank %in% regionOccupable_start:ifelse(
              regionOccupable_end<regionOccupable_start,
              (regionOccupable_start + regionOccupable_end),
              regionOccupable_end
              )
            ), 
            N[i], 
            replace = F
            )
          )
      
      # Assign the fishes in the haul to the positions
      tank[as.numeric(unlist(positions[i]))] <- ifelse(as.numeric(unlist(haulsList[[i]][4])) == 1, "herring", "sprat") # Here we need to go back to character notation to avoid confusion between numbers used for index and 1 and two used for the species
      
    }
    
   
      
  }
  
  tank <- ifelse(
    tank == "herring", 
    1, 
    ifelse(
      tank == "sprat",
      2,
      0
      )
    ) # Here we need to go back to character notation to avoid confusion between numbers used for index and 1 and two used for the species
  
  # Finally we produce a twin matrix reporting the fishes with the same position but with information of the haul of origin
  haulsTankDf <- melt(positions)
  tankHaulsTwinMatrix <- tank
  tankHaulsTwinMatrix[haulsTankDf$value] <- haulsTankDf$L1
  
}

## P3: Plot the result of the pouring for the main tank matrix
drawTank(tank, plot = 1, type = "species")

## P3: Plot the result of the pouring including the haul tank twin matrix 
ggarrange(
  drawTank(tank, plot = 1, type = "species"),
  drawTank(tankHaulsTwinMatrix, plot = 1, type = "hauls")
)


########################################################################################
#
# Sink fishes to the bottom of the tank ----
# Here we send the fishes caught in the haul(s) to the bottom of the tank. 
# The fishes in the current version go down vertically, depending if an empty cell is
# available below your position for their fall. This implied that in case of more than 
# one hauls a bit of mixing at the zone of contact with the catch may be expected. 
#
########################################################################################

## P1: Sink the fishes for the main tank matrix
for(j in 1:ncol(tank)){
  tank <- ifelse(tank == 0, NA,tank)
  recFullCol <- tank[!is.na(tank[,j]),j]
  nRecFullCol <- length(recFullCol)
  tank[,j] <- c(rep(NA, tankHeight-nRecFullCol), recFullCol)
}

## P1: Sink the fishes for the haul tank twin matrix
for(j in 1:ncol(tank)){
  tankHaulsTwinMatrix <- ifelse(tankHaulsTwinMatrix == 0, NA,tankHaulsTwinMatrix)
  recFullCol <- tankHaulsTwinMatrix[!is.na(tankHaulsTwinMatrix[,j]),j]
  nRecFullCol <- length(recFullCol)
  tankHaulsTwinMatrix[,j] <- c(rep(NA, tankHeight-nRecFullCol), recFullCol)
}
tankHaulsTwinMatrix <- ifelse(is.na(tankHaulsTwinMatrix), 0, tankHaulsTwinMatrix)

## P3: Plot the results for the main tank matrix
if(bigTank == 1){
  tankR <- apply(tank, 2, rev)
  image(t(tankR))
}else{
  drawTank(tank, plot = 1, type = "species")  
}

## P4: Plot the results including the haul tank twin matrix
ggarrange(
  drawTank(tank, plot = 1, type = "species"),
  drawTank(tankHaulsTwinMatrix, plot = 1, type = "hauls")
)

########################################################################################
#
# Build the tube ----
# Here we build the tube according to selections
#
########################################################################################
## P1: Build the tube
flowtube <- buildTube(
  heightTube = heightTube, 
  lengthTube = lengthTube, 
  plot = 1
)

########################################################################################
#
# Build the connection ----
# Here we build the connection between the tank and the tube
#
########################################################################################
## P1: Build the connection between tank and tube
suppressMessages(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1))

## P2: Add proportion revealed at different intervals of the connection between tank and tube
suppressMessages(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1))

########################################################################################
#
# Flow the fishes inside the tube ----
# Here we flow the fishes between the tank and the tube
#
########################################################################################
## P1: Set the parameter for the flow
timeSteps = length(tank)/heightTube
plotFlow = 0
plotFlowEach = 10000
saveFlow = 1
quality = 100
addition = 0
ltb <- list(flowtube)

# The following lines avoid to produce an empty frame when saving the plot, this happens for time being multiplier of tank width x tube height, as there are no more fishes in the last tank row
toAvoid <- which(1:timeSteps %% (tankWidth/heightTube) == 0) + 1

while(!all(is.na(tank))){
  
  for(t in 1:timeSteps){
    
    if(t == 1){
      
      tankRow <- tankHeight # We speicify the row of the Tank we are sampling
      cat("\n", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),])), "\n")
      
      positionExitRow <- sample(which(!is.na(tank[nrow(tank),])), 10) # Here we sample in the last row the position of the fishes that will exit
      flowtube[c(1:heightTube),1] <- tank[nrow(tank),positionExitRow] # These fishes enter the tube, while 
      tank[nrow(tank), positionExitRow] <- NA # Their position in the tank are turned to NA 
      ltb[[2]] <- flowtube # We save the result in a step of the flowtube time list
      # ltk[[2]] <- tank  # We save the result in a step of the tank time list - avoided, ends up exhausting the memory
      t <- t + 1 # We specify we moved away from the start
      #print(flowtube[,1:10]) # We print the results
      
        
      if((!t %in% toAvoid)){ # The second part is due to the matrix being NULL when no more fishes are present in a row, which coincides with the time at which the time is a multiplier of tank width x tube height
        plotUpdate <- drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1) # This plot an update of the flow  
      }
      
      if(saveFlow == 1 & (!t %in% toAvoid)){ # The second part is due to the matrix being NULL when no more fishes are present in a row, which coincides with the time at which the time is a multiplier of tank width x tube height
        
        ggsave(
          filename = paste0(
            supportResultsDir,
            "/flowTankTube/pngs/image-", 
            ifelse(
              t<=10, 
              "0000000", 
              ifelse(
                t<=100, 
                "000000",
                ifelse(
                  t<=1000, 
                  "00000",
                  ifelse(
                    t<=10000, 
                    "0000",
                    ifelse(
                      t<=100000, 
                      "000",
                      "00"
                    )
                  )
                )
              )
            ),
            t-1,
            ".png"
          ),
          plot = plotUpdate,
          width = 20, 
          height = 10,
          dpi = quality
        )
        
      }
      
    } else {
      if(sum(!is.na(tank[nrow(tank),])) >= heightTube ){ # If more than 10 elements are present in the last row of the tank, we extract 10 randomly
        
        cat("\n", "Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),])), "\n", "\n")
        
        positionExitRow <- sample(which(!is.na(tank[nrow(tank),])), heightTube) # Here we sample in the last row the position of the fishes that will exit
        flowtube[,2:lengthTube] <- flowtube[,1:lengthTube-1] # Here we shift plus one to the right the tube matrix to simulate flow
        flowtube[,1] <- NA # The first column of the tube matrix is duplicated at this stage on the second column, we fill it empty 
        flowtube[c(1:heightTube),1] <- tank[nrow(tank),positionExitRow] # We replace the first column with values sampled from the tank last row. 
        tank[nrow(tank), positionExitRow] <- NA # Their position in the tank are turned to NA 
        
        ltb[[2+t]] <- flowtube # We save the result in a step of the tube time list
        # ltk[[2+t]] <- tank  # We save the result in a step of the tank time list  - avoided, ends up exhausting the memory
        t <- t + 1
        #print(flowtube[,1:10]) # We print the results
        
          
        if((!t %in% toAvoid)){ # The second part is due to the matrix being NULL when no more fishes are present in a row, which coincides with the time at which the time is a multiplier of tank width x tube height
          plotUpdate <- drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1) # This plot an update of the flow  
        }
        
        if(saveFlow == 1){ 
          
          ggsave(
            filename = paste0(
              supportResultsDir,
              "/flowTankTube/pngs/image-", 
              ifelse(
                t<=10, 
                "0000000", 
                ifelse(
                  t<=100, 
                  "000000",
                  ifelse(
                    t<=1000, 
                    "00000",
                    ifelse(
                      t<=10000, 
                      "0000",
                      ifelse(
                        t<=100000, 
                        "000",
                          "00"
                        )
                      )
                    )
                  )
                ),
              t-1,
              ".png"
              ),
            plot = plotUpdate,
            width = 20, 
            height = 10,
            dpi = quality
          )
        
        }
          
        if(plotFlow == 1 & t %/% plotFlowEach %in% seq(plotFlowEach, timeSteps, plotFlowEach)){ # This plot every plotFlowEach iteration as specified
          
          print(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1)) # Try and plot results
          
        }
          
        
      } else if (sum(!is.na(tank[nrow(tank),])) < heightTube & sum(!is.na(tank[nrow(tank),])) >=1) { # If more less than 10 elements are present in the last row of the tank, we extract what is there
        
        cat("\n", "Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),])), "\n", "\n")
        
        positionExitRow <- which(!is.na(tank[nrow(tank),])) # No need to sample in this case, all fishes left in the bottom matrix row are selected to move in the tube 
        flowtube[,2:lengthTube] <- flowtube[,1:lengthTube-1] # Here we shift plus one to the right the tube matrix to simulate flow
        flowtube[,1] <- NA # The first column of the tube matrix is duplicated at this stage on the second column, we fill it empty 
        flowtube[c(1:heightTube),1] <- c(tank[nrow(tank),positionExitRow], rep(NA, heightTube-length(tank[nrow(tank),positionExitRow]))) # Here a bit of pulsation is allowed between one row and the other (NA value when fishes in the bottom row are <10) 
        tank[nrow(tank), positionExitRow] <- NA # Their position in the tank are turned to NA 
        
        ltb[[2+t]] <- flowtube # We save the result in a step of the tube time list
        # ltk[[2+t]] <- tank  # We save the result in a step of the tank time list  - avoided, ends up exhaust the memory
        t <- t + 1
        
        #print(flowtube[,1:10]) # We print the results
       
        
        if((!t %in% toAvoid)){ # Due to the matrix being NULL when no more fishes are present in a row, which coincides with the time at which the time is a multiplier of tank width x tube height
          plotUpdate <- drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1) # This plot an update of the flow  
        }
        
        if(saveFlow == 1){
          
          ggsave(
            filename = paste0(
              supportResultsDir,
              "/flowTankTube/pngs/image-", 
              ifelse(
                t<=10, 
                "0000000", 
                ifelse(
                  t<=100, 
                  "000000",
                  ifelse(
                    t<=1000, 
                    "00000",
                    ifelse(
                      t<=10000, 
                      "0000",
                      ifelse(
                        t<=100000, 
                        "000",
                        "00"
                      )
                    )
                  )
                )
              ),
              t-1,
              ".png"
            ),
            plot = plotUpdate,
            width = 20, 
            height = 10,
            dpi = quality
          )
          
        }
        
        if(plotFlow == 1 & t %/% plotFlowEach %in% seq(plotFlowEach, timeSteps, plotFlowEach)){ # This plot every plotFlowEach iteration as specified
          
          print(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1)) # Try and plot results
          
        }
        
      } else { # It means that zero element are present in the last row of the tank it means that we need to replace this row with the one above
        
        cat("\n", "Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),])), "\n", "\n")
        cat("Tank matrix row", tankRow, "completed. Proceeding with tank matrix row", tankRow + 1, "\n")
        
        tankRow <- tankRow - 1 # We speicify the row of the Tank we are sampling
        tank <- rbind(rep(NA, ncol(tank)), tank[-tankHeight,])  # Here we shift plus one to the right the tube matrix to simulate flow
       
        if(plotFlow == 1 & t %/% plotFlowEach %in% seq(plotFlowEach, timeSteps, plotFlowEach)){ # This plot every plotFlowEach iteration as specified
          print(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1)) # Try and plot results
        }
        
      }
      
    }  
    
  }
  
  cat("\n", "\n", "\n", "Completed", "\n", "\n", "\n")
  
  if(plotFlow == 1){ # This plot every plotFlowEach iteration as specified
    print(drawFlow(tank, flowtube, pIndicator = 1, sizeLabelText = .1)) # Try and plot results
  }
  
}

## P2: Save the resulting matrix
save(ltb, file = "~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes/Simulation3/Sim_3_mtx.RData")

## P3: Produce an animation of the simulation
# To produce an animation of the results it is important to 
# i.  Go to terminal and rename the single frames in subsequent order, since apparently there is still a gap in the naming  every 50 frames we use the command: 
#    
#     (once in the directory containing the frames) ls -v | cat -n | while read n f; do mv -n "$f" "$n.ext"; done # Credits: https://stackoverflow.com/questions/3211595/renaming-files-in-a-folder-to-sequential-numbers
# 
# ii. Produce the video with 
#
#      (if 50gap frame is solved) ffmpeg -framerate 25 -i image-%08d.png   -c:v libx264 -pix_fmt yuv420p out.mp4  # Credit: https://stackoverflow.com/questions/24961127/how-to-create-a-video-from-images-with-ffmpeg
#      (if 50gap frame is not solved and i. was done) ffmpeg -framerate 25 -i image-%d.png   -c:v libx264 -pix_fmt yuv420p out.mp4  # Credit: https://stackoverflow.com/questions/24961127/how-to-create-a-video-from-images-with-ffmpeg
# 
#      Adjust framerate on needs 
#
# iii. Export video
##

################## 
# Obtain a df from results
##################
## For the tube 
# Clean results
ltb <- ltb[lengths(ltb) != 0]

# Reverse matrices
listFlow <- do.call(rbind, lapply(ltb, t))

# Obtain a df 
dFlow <- as.data.frame(listFlow)

# Include sequence number
dFlow <- cbind(
  dFlow, 
  rowname = rep(1:lengthTube, length(ltb))
)

# Include time 
dFlow <- cbind(
  dFlow, 
  time = unlist(lapply(1:(nrow(dFlow)/lengthTube), function(x) rep(x, lengthTube)))
)

# Now turn it into a long df
dFlow <- pivot_longer(dFlow, cols = !c("rowname", "time")) 

# Rename the columns
dFlow <- dFlow %>% dplyr::rename(
  colM  = rowname, # The column called rowname is what in the original matrix was a column
  rowM  = name,    # The column called name    is what in the original matrix was a row
  time  = time,
  species = value  # The column named value    is what in the original matrix was the combination of row and column, i.e. the species
) 

# Clean the output
dFlow <- dFlow %>% 
  dplyr::mutate(
    rowM = gsub("V", "", rowM), # Remove the V in rowM, 
    rowM = as.numeric(rowM), 
    colM = as.numeric(colM)
) 
