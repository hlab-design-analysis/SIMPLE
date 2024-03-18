########################################################################################
#
# simulationLog
# 
########################################################################################
#
# This script record simulation parameter so that we can avoid re-specifying them.
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
# 
# 
#
########################################################################################
#
# Settings. 
# Here we tune some settings and indicate relevant directories on our machines.
#
########################################################################################

if(SimulationFromLog == 1){
  
  ## Simulation 1
  # In this simulation a catch of 50000 fishes with proportion 0.1 herring and 0.9 sprat is caught in one single haul that flows into the tube. 
  # Fishes are completely mixed in the process and no length or weight of them is considered. 
  
  # Time estimated: 
  # - 1.5 days when resulting plot are stored to produce animation.
  # - few of minutes when just numeric results are stored. 
  
  if(Simulation == 1){
    
    # Catch properties
    nHaul = 1 # Number of hauls
    p_herring = 0.1
    p_sprat = 1 - p_herring
    x = c("herring","sprat")
    prob = c(p_herring, p_sprat)
    N = 50000
    
    # Tank properties
    tankHeight = 700
    tankWidth = 500
    bigTank = 0
    
    # Tube properties
    heightTube = 10 
    lengthTube = 100 
    
  }
  
  ## Simulation 2
  # In this simulation a catch of 329800 fishes with proportion 0.1 herring and 0.9 sprat is caught in one single haul that flows into the tube. 
  # Fishes are completely mixed in the process and no length or weight of them is considered. 
  # The value of 329800 has been choosen as it represent the mean number caught in 2023 per trip, if we consider the fishes weight being 10gr.
  # This is consistent with statistics reported in Eero 2012 in case of Sprat (weight across all ages): https://academic.oup.com/icesjms/article/69/6/1010/618064.
  
  # Time estimated: 
  # - couple of days when resulting plot are stored to produce animation.
  # - couple of minutes when just numeric results are stored. 
  
  if(Simulation == 2){
    
    # Catch properties
    nHaul = 1 # Number of hauls
    p_herring = 0.1
    p_sprat = 1 - p_herring
    x = c("herring","sprat")
    prob = c(p_herring, p_sprat)
    N = 329800
    
    # Tank properties
    tankHeight = 700
    tankWidth = 500
    bigTank = 0
    
    # Tube properties
    heightTube = 10 
    lengthTube = 100 
    
  }
  
  ## Simulation 3
  # In this simulation a catch of 305000 fishes in three hauls (N = c(5000, 200000, 100000)) with different proportions (pH = c(0.1, 0.2, 0.3))  flows into the tube. 
  # Fishes in the tank are deposited in layers corresponding with hauls with random mixing at the border, depending on missing values at the cell below the single fish at the moment of pouring.

  # Time estimated: 
  # - Few minutes without images.
  # - 15h with images
  
  if(Simulation == 3){
    
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
    
  }
  
}
