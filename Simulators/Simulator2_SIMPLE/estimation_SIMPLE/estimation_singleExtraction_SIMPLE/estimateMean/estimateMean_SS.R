## P1: Estimate mean SS 
# Estimate mean in SS  
finalDf_long_SS_singleExtraction <- samplesDf_SS %>% 
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

finalDf_wide_SS_singleExtraction <- finalDf_long_SS_singleExtraction %>% 
  dplyr::select(species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) #%>% 

# Store results for systematic sampling 
resultsSSampling_singleExtraction <- finalDf_long_SS_singleExtraction %>% 
  dplyr::filter(species != "0" ) %>% 
  dplyr::select(species, pWeight) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(
    meanSampledProportion = mean(pWeight)
  )