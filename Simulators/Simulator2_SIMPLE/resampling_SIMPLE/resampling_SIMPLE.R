########################################################################################
#
# resampling
# 
########################################################################################
#
# This script re-samples from simulated flow of small pelagic catch flow 
# segmented in tons and buckets. 
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
# 2024-11-05: Created
#
########################################################################################
#
# Load data ----- 
# Here we load the flow as it was created in the segmentation_SIMPLE.R script.
#
########################################################################################

## Clean env
rm(list=setdiff(ls(), c("p_herring", "nHaul", "W", "cores", "supportResultsDir", "simName", "p_herring", "resamplingFlowSRS", "resamplingFlowSS", "do_systematic_samples_N_n"))); gc()

## Load the flow segmented 
load("~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes/Simulation4/Sim_4_flowBucketTonsAssigned.RData")

## Resampling

# For SRS 
startTime_SRS <- Sys.time()

# Parallel resampling 
clust <- makeCluster(cores)
parallel::clusterEvalQ(clust, c(library("tidyverse"), library("reshape2")))
parallel::clusterExport(clust, c("resamplingFlowSRS", "samplingTimes", "flow", "samplingFrequency"))

resampling_SRS <- parLapply(clust, 1:samplingTimes, function(x){
  
  # Fit the predictions
  df <- resamplingFlowSRS()
  
  # return data as dataframe
  df <- as.data.frame(df)
  
  # return results
  df 
  
})

# Close cluster
stopCluster(clust)

for(i in 1:length(resampling_SRS)){ # Add replicas label

    resampling_SRS[[i]]$Replica <- i
}

# Turn into a df 
resampling_SRS <- do.call(rbind, resampling_SRS)

endTime_SRS <- Sys.time()
time_SRS = endTime_SRS - startTime_SRS
time_SRS # Estimated 4.88

# Save SRS resampling 
save(resampling_SRS, file = paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SRS.RData"))

# For SS
startTime_SS <- Sys.time()

# Parallel resampling 
clust <- makeCluster(cores)
parallel::clusterEvalQ(clust, c(library("tidyverse"), library("reshape2")))
parallel::clusterExport(clust, c("resamplingFlowSS", "do_systematic_samples_N_n", "samplingTimes", "flow", "samplingFrequency"))

resampling_SS <- parLapply(clust, 1:samplingTimes, function(x){
  # Fit the predictions
  df <- resamplingFlowSS()
  
  # return data as dataframe
  df <- as.data.frame(df)
  
  # return results
  df 
  
})

# Close cluster
stopCluster(clust)

for(i in 1:length(resampling_SS)){ # Add replicas label
  resampling_SS[[i]]$Replica <- i
}

# Turn into a df 
resampling_SS <- do.call(rbind, resampling_SS)

endTime_SS <- Sys.time()
time_SS = endTime_SS - startTime_SS
time_SS # Estimated 6.21

# Save SS resampling 
save(resampling_SS, file = paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SS.RData"))
