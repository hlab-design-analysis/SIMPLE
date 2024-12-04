## P1: Estimate mean SRS
# Estimate mean in SRS
finalDf_long_SRS_multiExtraction <-  resampling_SRS %>% 
  as.data.frame() %>% 
  #dplyr::filter(species != 0) %>% # TBF!
  dplyr::group_by(bucket, species, Replica, ton) %>% 
  dplyr::summarize(
    weightSpecies = sum(weight)
  ) %>% 
  ungroup() %>% 
  group_by(bucket, Replica) %>% 
  mutate(
    weightTot = sum(weightSpecies)
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    pWeightBucket = weightSpecies/weightTot
  )

# Store results for simple random sampling 
resultsSRSampling_multiExtraction <- finalDf_long_SRS_multiExtraction %>% 
  dplyr::select(bucket, species, weightSpecies, Replica) %>% 
  dplyr::group_by(species, Replica) %>% 
  dplyr::summarise(
    weightSpecies = sum(weightSpecies)
  ) %>% 
  ungroup()%>% 
  group_by(Replica) %>% 
  mutate(
    weightTot = sum(weightSpecies)
  ) %>% 
  rowwise() %>% 
  mutate(
    pWeight = weightSpecies/weightTot
  )

# Create wide version
finalDf_wide_SRS_multiExtraction <- finalDf_long_SRS_multiExtraction %>% 
  dplyr::select(bucket, species, ton, pWeightBucket, weightTot, Replica) %>% 
  pivot_wider(names_from = c("species"), values_from = pWeightBucket) 

## Assign names consistent to variance improvement project case
# From Nuno e-mail (2022-10-25 )
# 
# SAparentId	just a row id (in case it is needed to link to original data)
# Trip	Unique id of Landing
# SamplingLocation	Location of sampling: "before"=before centrifuge, "after" = after centrifuge", "partial" = unclear (and some catch missing)
# SampleFrequencyTon	Frequency of sampling (in ton). Approximate.
# BucketName	Unique id of bucket
# BucketSequenceTon	Tonnage of bucket when sample was taken. Approximate
# BucketType	Type of bucket: "A" = main, "B" = replicate
# InBucketWeightTOT	Total weight of bucket (in kg)
# InBucketWeightHER	Total weight of HER in bucket (in kg)
# InBucketWeightSPR	Total weight of SPR in bucket (in kg)
# InBucketWeightOTHER	Total weight of OTHER in bucket (in kg)
# InBucketWeightNI	Total weight of NI (=Non-identifiable or non-identified organisms) in bucket (in kg)
# InBucketPropWeightHER	Proportion of HER in bucket (% in Weight)
# InBucketPropWeightSPR	Proportion of SPR in bucket (% in Weight)
# InBucketPropWeightOTHER	Proportion of OTHER in bucket (% in Weight)
# InBucketPropWeightNI	Proportion of NI (=Non-identifiable or non-identified organisms) in bucket (% in Weight)
# LandingWeightTOT	Total weight of landing (in kg)
# NBucketsTotal	Number of buckets total. Estimated as round(LandingWeightTOT/AverageInBucketWeightTOT)
# NBucketsTotalPerType	Number of buckets total (calculated per Sampling Location). Estimated as round(LandingWeightTOT/AverageInBucketWeightTOTperSamplingLocation)

# So: 
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


## Generate a final df looking alike the ones shared by Nuno 
# First create a reference with the amount of buckets extracted by Replica
extractedBucketsSRS <- finalDf_wide_SRS_multiExtraction %>% 
  dplyr::select(Replica, bucket) %>% 
  distinct() %>% 
  dplyr::group_by(Replica) %>% 
  summarize(NBucketsTotal = n())

finalDf_wide_SRS_multiExtraction <- finalDf_wide_SRS_multiExtraction %>%
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
    NBucketsTotalPerType = NA, 
    levelOfExtraction = NA, # ASK
    populationName = paste(paste0("simN", simName), paste0("nHaul(", nHaul, ")"), paste0("pHer(", paste(p_herring, sep = ","), ")"), sep = "_"), # Sim name, haul, pherringinHaul
    samplingScheme = "SRS"
  ) %>% 
  dplyr::group_by(Replica) %>% 
  mutate(
    NBucketsTotal = round(LandingWeightTOT/mean(InBucketWeightTOT))
  ) %>% 
  ungroup()

# Reorder the columns (we need the new added in the end to emulate the data passed by Nuno)
finalDf_wide_SRS_multiExtraction <- finalDf_wide_SRS_multiExtraction %>% 
  dplyr::select(
    "SAparentId",
    "Trip",
    "SamplingLocation",       
    "SampleFrequencyTon",
    "BucketName",              
    "BucketSequenceTon",     
    "BucketType",
    "InBucketWeightTOT",
    "InBucketWeightHER",      
    "InBucketWeightSPR",
    "InBucketWeightOTHER",
    "InBucketWeightNI",       
    "InBucketPropWeightHER",
    "InBucketPropWeightSPR",
    "InBucketPropWeightOTHER",
    "InBucketPropWeightNI",
    "LandingWeightTOT",
    "NBucketsTotal",          
    "NBucketsTotalPerType", 
    "populationName",
    "samplingScheme", 
    "NBucketsTotal", 
    "Replica"
  ) %>% 
  data.frame()

