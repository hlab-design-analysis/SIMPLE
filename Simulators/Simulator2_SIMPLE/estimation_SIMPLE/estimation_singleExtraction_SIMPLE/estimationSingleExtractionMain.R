########################################################################################
#
# Estimation single extraction
# 
########################################################################################
#
# This script estimates the proportion and variance of a simulated flow of small 
# pelagics, based on the flow produced in SmallPelagicFlowSim.R script and using the 
# samples extracted in the sampling.R script. The estimation is done for one 
# extraction, meaning that there is only one sampling event in which n buckets are
# collected. 
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
load(paste0(supportResultsDir, "/fishesSampled/Simulation", simName, "/Sim_", simName, "/fishesSampled_SimpleRandomSampling.RData")) 

# For systematic sampling 
load(paste0(supportResultsDir, "/fishesSampled/Simulation", simName, "/Sim_", simName, "/fishesSampled_SystematicSampling.RData")) 

## P2: Estimate the mean 
# Estimate mean for SRS samples 
source("estimation_SIMPLE/estimation_singleExtraction_SIMPLE/estimateMean/estimateMean_SRS.R")

# Estimate mean for SS samples 
source("estimation_SIMPLE/estimation_singleExtraction_SIMPLE/estimateMean/estimateMean_SS.R")

# Summarize the results
source("estimation_SIMPLE/estimation_singleExtraction_SIMPLE/estimateMean/resultsMean.R")

## P4: Estimate the variance 
# Estimate variance for SRS samples 
#source("estimation_SIMPLE/estimateVariance_SRS.R")

# Estimate variance for SS samples 
#source("estimation_SIMPLE/estimateVariance_SS.R")

