## Systematic resampling
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
    #dplyr::select(Var1, Var2, value) %>% 
    filter(selected == 1) %>%
    dplyr::select(Var1, Var2) %>%
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
  
  samplesDf_SS
  
}
