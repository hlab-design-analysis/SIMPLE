########################################################################################
#
# Libraries_SIMPLE
# 
########################################################################################
#
# In this file we load relevant libraries used in the simulation
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

## P1: Load the libraries
library(tidyverse)
library(binhf)
library(reshape2)
library(plotly)
library(catmaply)
library(crayon)
library(dqrng)
library(ggfocus)
library(ggpubr)

## P2: Print message
cat('\n')
cat(green("          \u2713  - Completed")) # Inform on the progress  
cat("\n")