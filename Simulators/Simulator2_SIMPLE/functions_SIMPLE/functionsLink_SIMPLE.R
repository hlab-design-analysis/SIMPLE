########################################################################################
#
# functionsLink
# 
########################################################################################
#
# In this small script we load all function in the folder allFunctions_SIMPLE
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
cat("[X]       Loading custom functions")

## P1: Load all functions in folder allFunctions_SIMPLE
for(i in dir("functions_SIMPLE/allFunctions_SIMPLE")) {
  source(paste0("functions_SIMPLE/allFunctions_SIMPLE", "/", i))
}

## P2: Print message
cat('\n')
cat(green("          \u2713  - Completed")) # Inform on the progress  
cat("\n")