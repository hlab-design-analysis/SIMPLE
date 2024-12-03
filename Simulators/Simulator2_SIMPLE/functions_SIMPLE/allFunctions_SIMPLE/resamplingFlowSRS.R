### Simple random sampling
resamplingFlowSRS <- function(x){
  
  ## Extract randomly n buckets
  extractedBucketsSRS <- sample(flow[,,7][!is.na(flow[,,7])], samplingFrequency)
  
  # First find corresponding tons
  indexesSRS <- flow %>% 
    melt %>%
    filter(Var3 %in% c(6,7)) %>%
    mutate(selected = ifelse(Var3 == 7 & value %in% extractedBucketsSRS, 1, 0)) %>% 
    filter(Var3 == 7) %>% 
    #dplyr::select(Var1, Var2, value) %>% 
    filter(selected == 1) %>%
    dplyr::select(Var1, Var2) %>%
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
  
  samplesDf_SRS
  
}


