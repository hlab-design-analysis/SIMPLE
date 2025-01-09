########################################################################################
#
# mainScript
# 
########################################################################################
#
# This script calls relevant script to: 
#  
# i)   Create a simulation of small pelagic catch flow in 2D, based on matrices.
# ii)  Sample from the flow 
# iii) Estimate the parameter of interest based on the sample extracted. 
# 
# The features of the flow and thus the results, are based on the parameters 
# specified by the user (e.g. the proportion of herring desired) at the beginning of this
# script. 
#
########################################################################################
#
#
# Authors:
#
# - Eros Quesada [first draft]
# - Nuno Prista
# - Annica de Groote
#
# Dev. Notes.
#
# 2024-07-05: Created. In the current version it essentialy reports the contents once in
#             of the SmallPelagicFlowSim.R script, now only used for simulating the flow
#
########################################################################################
#
# Settings ----- 
# Here we tune some settings and indicate relevant directories on our machines.
#
########################################################################################

## P1: Set wd, where SIMPLE folder is
setwd("~/Public_Eros/SIMPLE/Simulators/Simulator2_SIMPLE")

## P2: Indicate directory to store intermediate results (should be a place able to allocate large files e.g. 70gb)
supportResultsDir <- "~/mnt/CNAS/SIMPLE_Auxiliary"

## P3: Indicate directory to store large results
ResultsDir <- "results"

## P4: Load the additional settings
source("settings_SIMPLE/settings_SIMPLE.R")


## P5: Define additional parameters
cores = 30 # For parallel computing, decrease if needed! 

########################################################################################
#
# Create simulation files -----
# Here we create a folder to store the results of the simulation
#
########################################################################################

## P0: Give a name to your simulation
simName = 4

## Create a folder to store results 
# Light results are stored in the repository indicated in the header
# Check its existence and create if none
if(!dir.exists(paste0("results_SIMPLE/Simulation", simName))){
  dir.create(paste0("results_SIMPLE/Simulation", simName))
}


########################################################################################
#
# Set parameters -----
# Here we set the initial condition for the simulation
#
########################################################################################

## P1: Choose parameters values 
# Catch properties
nHaul = 1 # Number of hauls
p_herring = c(0.66) # Proportion of herring. If nHaul > 1, use vector with one value per each
W = c(50000) # Catch of both species If nHaul > 1, use vector with one value per each 

# Tank properties
tankHeight = 3500
tankLength = 2000
bigTank = 0

# Tube properties
heightTube = 1000 
lengthTube = 3000

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

## Compression tube
lateralCompression = 1 # Inevitably, some of the fishes at the end of the flow will be intervallated with water.
typeLateralCompression = "sharp" # Specify the type of compression desired.The latter can be "sigmoid", which pushes the fishes to the right, if the right cell is empty, originating a sigmoid shape or "sharp", reassinging fishes to cell to obtain the  sharpest shape possible.

# Execute script
source("~/Personal_Eros_locale/SIMPLE/Simulators/Simulator2_SIMPLE/simulationLog_SIMPLE/simulationLog.R")

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
# Load data ----- 
# Here we load real sampling data to use as reference for biological parameters (
# e.g. weigth distribution). 
#
########################################################################################

## Load data 
load ("~/Public_Eros/SIMPLE/Simulators/Simulator1_SIMPLE/001_Inputs_SimRealHaul/Input_data_her.27.25-29_8.Rdata"); df_her <- df0
load ("~/Public_Eros/SIMPLE/Simulators/Simulator1_SIMPLE/001_Inputs_SimRealHaul/Input_data_spr.27.22-32_8.Rdata"); df_spr <- df0

########################################################################################
#
# Simulate the flow ----- 
# The following line create virtual hauls based on the user parameters, fill the tank 
# with their content and flow the fishes inside a tube until the tube is empty. 
#
# NOTE: it may take time for the script to complete (usually within 12h, but may take up
# and over this time based on user selection and computational power.) 
#
########################################################################################
## Simulate the flow. 
source("flowSimulation_SIMPLE/SmallPelagicFlowSim.R")

########################################################################################
#
# Compression ----- 
# The following line compress the flow produced at the prior step to avoid areas in which
# fishes are mixed with empty spaces.
# The compression is made only if the parameter "lateralCompression" is set to 1. 

source("Simulator2_SIMPLE/lateralCompression_SIMPLE/lateralCompression_SIMPLE.R")

########################################################################################
#
# Segmentation ----- 
# The following line segment the flow produced at the prior step into tons and buckets.
# These are later used to sample and estimate the proportion of herring and sprat. 
#
source("Simulator2_SIMPLE/segmentation_SIMPLE/segmentation_SIMPLE.R")

########################################################################################
#
# Sampling and Re-sampling ----- 
# The following line performs a systematic sampling based on user selection. 
# In detail, the flow generated at the previous step is segmented in tons and buckets.
# Then, based on the frequency specified by the user, a ton is selected as random start.
# Inside this ton the first bucket is chosen and extracted. From this point in the flow, 
# one bucket is extracted every n tons (again, based on user selection). 
#
# NOTE: it may take time for the script to complete (usually within 5h, but may take up
# and over this time based on user selection and computational power.) 
#
########################################################################################
## Produce the sampling (one extraction)
source("sampling_SIMPLE/sampling.R")

## Produce the resampling (multiple extractions)
source("resampling_SIMPLE/resampling_SIMPLE.R")

########################################################################################
#
# Estimation ----- 
# The following line estimates the proportion and variance of a simulated flow of small 
# pelagics, based on the flow produced in SmallPelagicFlowSim.R script and using the 
# samples extracted in the sampling.R script.
#
########################################################################################
## Produce the estimation (for single and multiple extraction i.e. for sampling and resampling results)
source("estimation_SIMPLE/estimationMain_SIMPLE.R")
