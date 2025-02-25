## P1: Estimate mean SRS 
# Estimate mean in SRS
finalDf_long_SRS_singleExtraction <- samplesDf_SRS %>% 
  as.data.frame() %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(
    weightSpecies = sum(weight)
  ) %>% 
  ungroup() %>% 
  mutate(
    weightTot = sum(weightSpecies)
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    pWeight = weightSpecies/weightTot
  )

finalDf_wide_SRS_singleExtraction <- finalDf_long_SRS_singleExtraction %>% 
  dplyr::select(species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) 

# Store results for simple random sampling 
resultsSRSampling_singleExtraction <- finalDf_long_SRS_singleExtraction %>% 
  filter(species != "0" ) %>% 
  dplyr::select(species, pWeight) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(
    meanSampledProportion = mean(pWeight)
  )
