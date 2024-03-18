########################################################################################
#
# Settings_SIMPLE
# 
########################################################################################
#
# In this file we set the settings needed for the simulation
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
## P0 Print message
cat("[X]       Loading settings")

## P1: Avoid exp notation
options(scipen = 999)

## P2: Suppress dplyr summarize info 
options(dplyr.summarise.inform = FALSE)

## P3: Give the max.print
options(max.print = 1000)

## P4: For replicability
set.seed(123)

## P2: Print message
cat('\n')
cat(green("          \u2713  - Completed")) # Inform on the progress  
cat("\n")