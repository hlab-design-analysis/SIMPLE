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
rm(list=setdiff(ls(), c("supportResultsDir", "simName", "p_herring"))); gc()

## Load the flow segmented 
load("~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes/Simulation4/Sim_4_flowBucketTonsAssigned.RData")

### Set number of replicates
samplingTimes <- 1000


## Set sampling frequency
samplingFrequency <- 30

### Simple random sampling

resamplingFlowSRS <- function(x){
  
  ## Extract randomly n buckets
  extractedBucketsSRS <- sample(flow[,,7], samplingFrequency)
  
  # First find corresponding tons
  indexesSRS <- flow %>% 
    melt %>%
    filter(Var3 %in% c(6,7)) %>%
    mutate(selected = ifelse(Var3 == 7 & value %in% extractedBucketsSRS, 1, 0)) %>% 
    filter(Var3 == 7) %>% 
    #select(Var1, Var2, value) %>% 
    filter(selected == 1) %>%
    select(Var1, Var2) %>%
    dplyr::rename(row = Var1, column = Var2) %>% 
    mutate(rowcol = paste0(row, "_",column))
  
  tonBucketCombiSRS <- flow %>% 
    melt %>%
    filter(Var3 == 6) %>%
    dplyr::rename(row = Var1, column = Var2) %>% 
    mutate(rowcol = paste0(row, "_",column)) %>% 
    filter(rowcol %in% indexesSRS$rowcol)
  
  tonVecSRS <- unique(tonBucketCombiSRS$value)
  
  ## Then create an empty list of samples
  samplesList_SRS <- as.list(rep(NA, length(extractedBucketsSRS)))
  names(samplesList_SRS) <- paste0("sample", 1:length(extractedBucketsSRS))
  
  ## Then create the list of the attributes, for those fishes that were selected 
  fishesSelectedID <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,1][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  fishesSelectedSP <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,2][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  fishesSelectedW <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,3][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  fishesSelectedV <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,4][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  fishesSelectedH <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,5][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  fishesSelectedT <- sapply(1:length(extractedBucketsSRS),  function(x){flow[,,6][which(flow[,,7] %in% extractedBucketsSRS[x], arr.ind = T)]})
  
  for(i in 1:length(samplesList_SRS)){
    samplesList_SRS[[i]] = list(
      "bucket" = extractedBucketsSRS[i],
      "identifier" = fishesSelectedID[[i]],
      "species" = fishesSelectedSP[[i]], 
      "weight" = fishesSelectedW[[i]],
      "volume" = fishesSelectedV[[i]],
      "haul" = fishesSelectedH[[i]],
      "ton" = fishesSelectedT[[i]]
    )
  }
  
  ## Transform into a df
  samplesDf_SRS <- do.call(rbind, lapply(1:length(samplesList_SRS), function(x) do.call(cbind, samplesList_SRS[[x]])))
  finalDf_long_SRS <- samplesDf_SRS %>% 
    as.data.frame() %>% 
    dplyr::group_by(bucket, species, ton) %>% 
    dplyr::summarize(
      weightSpecies = sum(weight)
    ) %>% 
    ungroup() %>% 
    group_by(bucket) %>% 
    mutate(
      weightTot = sum(weightSpecies)
    ) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(
      pWeight = weightSpecies/weightTot
    )
  
  finalDf_wide_SRS <- finalDf_long_SRS %>% 
    select(bucket, species, ton, pWeight, weightTot) %>% 
    pivot_wider(names_from = c("species"), values_from = pWeight) 
  

  
  ## Assign names consistent to variance improvement project case
  # These were: 
  # - "SAparentId" = Takes NA as we do not to link to sampling events etc.  
  # - "Trip"       = Takes simulation number? 
  # - "SamplingLocation" = Takes NA (was before/after centrifuge before)       
  # - "SampleFrequencyTon" =  ??? Takes NA
  # - "BucketName" = Replace column named "bucket", but need to paste "bucket_" ahead of value. 
  # - "BucketSequenceTon" =  Replace column named "ton".
  # - "BucketType" = Takes NA. 
  # - "InBucketWeightTOT" = Replace column named "weightTot". 
  # - "InBucketWeightHER" = Calculated by multipling the pWeight by weightTot by row
  # - "InBucketWeightSPR" = Calculated by multipling the pWeight by weightTot by row    
  # - "InBucketWeightOTHER" = Takes zero temporary
  # - "InBucketWeightNI"    = Takes zero temporary
  # - "InBucketPropWeightHER" = Should replace column named "pWeight", needs wide version
  # - "InBucketPropWeightSPR" = Should replace column named "pWeight", needs wide version
  # - "InBucketPropWeightOTHER" = Takes zero temporary
  # - "InBucketPropWeightNI"    = Takes zero temporary
  # - "LandingWeightTOT"      = Equal to sum(W)  
  # - "NBucketsTotal"         = Equal to length(extractedBucketsSRS)
  # - "NBucketsTotalPerType"  = Takes NA
  
  finalDf_wide_SRS %>%
    dplyr::rename(
      BucketName = bucket, 
      BucketSequenceTon = ton, 
      InBucketWeightTOT = weightTot, 
      InBucketPropWeightHER = `1`, 
      InBucketPropWeightSPR = `2` 
    ) %>% 
    mutate(
      BucketName = paste0("bucket_", BucketName), 
      SAparentId = NA, 
      Trip = NA, 
      SamplingLocation = NA, 
      SampleFrequencyTon = NA, 
      BucketType = NA,
      InBucketPropWeightOTHER = 0, 
      InBucketPropWeightNI = 0, 
      InBucketWeightHER = InBucketWeightTOT * InBucketPropWeightHER, 
      InBucketWeightSPR = InBucketWeightTOT * InBucketPropWeightSPR,
      InBucketWeightOTHER = InBucketWeightTOT * InBucketPropWeightOTHER,
      InBucketWeightNI = InBucketWeightTOT * InBucketPropWeightNI,
      LandingWeightTOT = sum(W), 
      NBucketsTotal = length(extractedBucketsSRS), 
      NBucketsTotalPerType = NA, 
      levelOfExtraction = NA, # ASK
      populationName = simName, 
      samplingScheme = "SS"
    )
  
}


## Systematic sampling

resamplingFlowSS <- function(x){
  
  # Compute the possible bucket combinations
  bucketInterval <- seq(1, max(flow[,,7], na.rm = T), 1)
  possibleBucketComb <- do_systematic_samples_N_n (N=length(bucketInterval), n=30)
  
  # Pick one randomly 
  extractedBucketsSS <- possibleBucketComb[sample(nrow(possibleBucketComb), 1),]
  
  # Find corresponding tons
  indexesSS <- flow %>% 
    melt %>%
    filter(Var3 %in% c(6,7)) %>%
    mutate(selected = ifelse(Var3 == 7 & value %in% extractedBucketsSS, 1, 0)) %>% 
    filter(Var3 == 7) %>% 
    #select(Var1, Var2, value) %>% 
    filter(selected == 1) %>%
    select(Var1, Var2) %>%
    dplyr::rename(row = Var1, column = Var2) %>% 
    mutate(rowcol = paste0(row, "_",column))
  
  tonBucketCombi <- flow %>% 
    melt %>%
    filter(Var3 == 6) %>%
    dplyr::rename(row = Var1, column = Var2) %>% 
    mutate(rowcol = paste0(row, "_",column)) %>% 
    filter(rowcol %in% indexesSS$rowcol)
  
  tonVecSS <- unique(tonBucketCombi$value)
  
  ## Calculate the proportion in the bucket selected
  # Extract fishes according to the bucket
  fishesSelected_SS <-  lapply(1:length(tonVecSS), function(x){
    fishesSelectedSp <-  flow[,,2][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]
    fishesSelectedW  <-  flow[,,3][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]
  })
  fishesSelected_SS
  
  ## First create an empty list of samples
  samplesList_SS <- as.list(rep(NA, length(extractedBucketsSS)))
  names(samplesList_SS) <- paste0("sample", 1:length(extractedBucketsSS))
  
  ## Then create the list of the attributes, for those fishes that were selected 
  fishesSelectedID <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,1][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  fishesSelectedSP <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,2][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  fishesSelectedW <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,3][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  fishesSelectedV <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,4][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  fishesSelectedH <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,5][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  fishesSelectedT <- sapply(1:length(extractedBucketsSS),  function(x){flow[,,6][which(flow[,,7] %in% extractedBucketsSS[x], arr.ind = T)]})
  
  for(i in 1:length(samplesList_SS)){
    samplesList_SS[[i]] = list(
      "bucket" = extractedBucketsSS[i],
      "identifier" = fishesSelectedID[[i]],
      "species" = fishesSelectedSP[[i]], 
      "weight" = fishesSelectedW[[i]],
      "volume" = fishesSelectedV[[i]],
      "haul" = fishesSelectedH[[i]],
      "ton" = fishesSelectedT[[i]]
    )
  }
  
  ## Transform into a df
  samplesDf_SS <- do.call(rbind, lapply(1:length(samplesList_SS), function(x) do.call(cbind, samplesList_SS[[x]])))
  finalDf_long_SS <- samplesDf_SS %>% 
    as.data.frame() %>% 
    dplyr::group_by(bucket, species, ton) %>% 
    dplyr::summarize(
      weightSpecies = sum(weight)
    ) %>% 
    ungroup() %>% 
    group_by(bucket) %>% 
    mutate(
      weightTot = sum(weightSpecies)
    ) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(
      pWeight = weightSpecies/weightTot
    )
  
  finalDf_wide_SS <- finalDf_long_SS %>% 
    select(bucket, species, ton, pWeight, weightTot) %>% 
    pivot_wider(names_from = c("species"), values_from = pWeight)  
  
  
  
  ## Assign names consistent to variance improvement project case
  # These were: 
  # - "SAparentId" = Takes NA as we do not to link to sampling events etc.  
  # - "Trip"       = Takes simulation number? 
  # - "SamplingLocation" = Takes NA (was before/after centrifuge before)       
  # - "SampleFrequencyTon" =  ??? Takes NA
  # - "BucketName" = Replace column named "bucket", but need to paste "bucket_" ahead of value. 
  # - "BucketSequenceTon" =  Replace column named "ton".
  # - "BucketType" = Takes NA. 
  # - "InBucketWeightTOT" = Replace column named "weightTot". 
  # - "InBucketWeightHER" = Calculated by multipling the pWeight by weightTot by row
  # - "InBucketWeightSPR" = Calculated by multipling the pWeight by weightTot by row    
  # - "InBucketWeightOTHER" = Takes zero temporary
  # - "InBucketWeightNI"    = Takes zero temporary
  # - "InBucketPropWeightHER" = Should replace column named "pWeight", needs wide version
  # - "InBucketPropWeightSPR" = Should replace column named "pWeight", needs wide version
  # - "InBucketPropWeightOTHER" = Takes zero temporary
  # - "InBucketPropWeightNI"    = Takes zero temporary
  # - "LandingWeightTOT"      = Equal to sum(W)  
  # - "NBucketsTotal"         = Equal to length(extractedBucketsSRS)
  # - "NBucketsTotalPerType"  = Takes NA
  
  finalDf_wide_SS %>%
    dplyr::rename(
      BucketName = bucket, 
      BucketSequenceTon = ton, 
      InBucketWeightTOT = weightTot, 
      InBucketPropWeightHER = `1`, 
      InBucketPropWeightSPR = `2` 
    ) %>% 
    mutate(
      BucketName = paste0("bucket_", BucketName), 
      SAparentId = NA, 
      Trip = NA, 
      SamplingLocation = NA, 
      SampleFrequencyTon = NA, 
      BucketType = NA,
      InBucketPropWeightOTHER = 0, 
      InBucketPropWeightNI = 0, 
      InBucketWeightHER = InBucketWeightTOT * InBucketPropWeightHER, 
      InBucketWeightSPR = InBucketWeightTOT * InBucketPropWeightSPR,
      InBucketWeightOTHER = InBucketWeightTOT * InBucketPropWeightOTHER,
      InBucketWeightNI = InBucketWeightTOT * InBucketPropWeightNI,
      LandingWeightTOT = sum(W), 
      NBucketsTotal = length(extractedBucketsSRS), 
      NBucketsTotalPerType = NA, 
      levelOfExtraction = NA, # ASK
      populationName = simName, 
      samplingScheme = "SS"
    )
  
}


## Resampling

# For SRS 
startTime_SRS <- Sys.time()
resampling_SRS <- replicate(samplingTimes, resamplingFlowSRS(), simplify = F)
endTime_SRS <- Sys.time()
time_SRS = endTime_SRS - startTime_SRS
time_SRS # Estimated 4.88

# Save SRS resampling 
save(resampling_SRS, file = paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SRS.RData"))

# For SS
startTime_SS <- Sys.time()
resampling_SS <- replicate(samplingTimes, resamplingFlowSS(), simplify = F)
endTime_SS <- Sys.time()
time_SS = endTime_SS - startTime_SS
time_SS # Estimated 6.21

# Save SS resampling 
save(resampling_SS, file = paste0(supportResultsDir, "/resampling/Simulation", simName, "/Sim_", simName, "_resampling_SS.RData"))

