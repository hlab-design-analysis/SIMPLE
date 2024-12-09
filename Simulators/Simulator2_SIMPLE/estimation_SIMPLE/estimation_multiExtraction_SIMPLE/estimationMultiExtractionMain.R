########################################################################################
#
# Estimation
# 
########################################################################################
#
# This script estimates the proportion and variance of a simulated flow of small 
# pelagics, based on the flow produced in SmallPelagicFlowSim.R script and using the 
# samples extracted in the sampling.R script.
# 
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
# 2024-07-05: Created. 
#
########################################################################################
## Below the part on the estimation

## P1: Load the sampling data
# For simple random sampling 
load(paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SRS.RData"))

# For systematic sampling 
load(paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SS.RData"))

## P2: Estimate the mean 
# Estimate mean for SRS samples 
source("estimation_SIMPLE/estimation_multiExtraction_SIMPLE/estimateMean/estimateMean_SRS.R")

# Estimate mean for SS samples 
source("estimation_SIMPLE/estimation_multiExtraction_SIMPLE/estimateMean/estimateMean_SS.R")


# Summarize the results
source("estimation_SIMPLE/estimation_multiExtraction_SIMPLE/estimateMean/resultsMean.R")

## P3: Estimate the variance 
# Define the number of Variance Estimators that are in use
nVarEstimators <- 8

# Estimate variance for SS samples 
source("estimation_SIMPLE/estimation_multiExtraction_SIMPLE/estimateVariance/estimateVariance_SS.R")

