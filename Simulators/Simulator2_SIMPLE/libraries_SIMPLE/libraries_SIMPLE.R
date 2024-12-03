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
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(binhf))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(catmaply))
suppressPackageStartupMessages(library(crayon))
suppressPackageStartupMessages(library(dqrng))
suppressPackageStartupMessages(library(ggfocus))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(abind))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(reactable))
suppressPackageStartupMessages(library(reactablefmtr))



## P2: Print message
cat('\n')
cat(green("          \u2713  - Completed")) # Inform on the progress  
cat("\n")