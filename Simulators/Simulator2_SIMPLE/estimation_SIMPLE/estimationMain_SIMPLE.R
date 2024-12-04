########################################################################################
#
# Estimation 
# 
########################################################################################
#
# This script estimates the proportion and variance of a simulated flow of small 
# pelagics, based on the flow produced in SmallPelagicFlowSim.R script and using the 
# samples extracted in the sampling.R script. The estimation is done both for one 
# extraction (sampling) and for multiextraction (resampling). 
#
########################################################################################
#
#
# Authors:
#
# - 
# 
# Dev. Notes.
#
#
# 2024-12-03: Created. 
#
########################################################################################
## Below the part on the estimation

## P1: Estimate for the single extraction
source("estimation_SIMPLE/estimation_singleExtraction_SIMPLE/estimationSingleExtractionMain.R")

## P1: Estimate for the multi extraction
source("estimation_SIMPLE/estimation_multiExtraction_SIMPLE/estimationMultiExtractionMain.R")
