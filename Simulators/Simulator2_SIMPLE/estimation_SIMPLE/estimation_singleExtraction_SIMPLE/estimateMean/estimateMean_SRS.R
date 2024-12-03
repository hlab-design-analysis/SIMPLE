## P1: Estimate mean SRS 
# Estimate mean in SRS
finalDf_long_SRS <- samplesDf_SRS %>% 
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

finalDf_wide_SRS <- finalDf_long_SRS %>% 
  dplyr::select(species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) 

# Store results for simple random sampling 
resultsSRSampling <- finalDf_long_SRS %>% 
  filter(species != "0" ) %>% 
  dplyr::select(species, pWeight) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(
    meanSampledProportion = mean(pWeight)
  )
