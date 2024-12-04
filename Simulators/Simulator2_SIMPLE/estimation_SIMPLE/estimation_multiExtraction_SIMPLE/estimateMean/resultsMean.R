## Finally, place together the results
#  As a table
finalSummary_schemes_multiExtraction <- rbind(
  resultsSRSampling_multiExtraction %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  resultsSSampling_multiExtraction %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
) %>% 
  data.frame() %>% 
  mutate(
    proportionTrue = ifelse(species == 1, 0.66, 0.33)  # To be replaced with p_herring 
  ) %>% 
  dplyr::rename(meanSampledProportion = pWeight) %>% 
  rowwise() %>% 
  mutate(
    diffSimTrue = abs(proportionTrue - meanSampledProportion)
  ) %>% 
  dplyr::select(
    -weightSpecies, - weightTot
  ) 

# Since is too many rows to be saved otherwise we summarize further
finalSummary_schemes_multiExtraction_median <- finalSummary_schemes_multiExtraction %>% 
  ungroup() %>%
  dplyr::group_by(scheme, species) %>% 
  reframe(
    medianSampledProportion = median(meanSampledProportion), 
    IQRSampledProportion = IQR(meanSampledProportion)
  ) %>% 
  mutate(
    proportionTrue = ifelse(species == 1, 0.66, 0.33)  # To be replaced with p_herring 
  ) %>% 
  rowwise() %>% 
  mutate(
    diffSimTrue = abs(proportionTrue - medianSampledProportion)
  ) %>% 
  rename(
    "Scheme" = 1, 
    "Species" = 2, 
    "Replica" = 3, 
    "MedE(P)" = 4, 
    "P" = 5, 
    "P - MedE(P)" = 6
  )

# Save as image
tableSummaryImg_multiExtraction <- kable(finalSummary_schemes_multiExtraction_median) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_ComparisonSchemes_", simName,"_multiExtraction", ".jpg"))

# As a plot
theTruthPlot_multiExtraction <- data.frame(
  #scheme = "The truth", 
  bucket = NA, 
  species = c(1, 2),
  weightSpecies = NA,  
  weightTot = NA,
  pWeight = c(0.66, 0.33) # This should be replace with p_herring
)

finalDf_long_schemes_multiExtraction <- rbind(
  finalDf_long_SRS_multiExtraction %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  finalDf_long_SS_multiExtraction %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
)


finalDf_long_schemes_medians_multiExtraction <- finalDf_long_schemes_multiExtraction %>% 
  group_by(Replica, species, scheme) %>% 
  summarize(
    median = median(pWeightBucket)
  ) 

comparisonSchemesPlot_multiExtraction <- finalDf_long_schemes_multiExtraction %>% 
  as.data.frame() %>%
  mutate(
    species = as.factor(ifelse(species == 1, "herring", "sprat"))
  ) %>% 
  ggplot() + 
  geom_boxplot(aes(x = species, y = pWeightBucket, fill = factor(Replica)), position=position_dodge(1), width=0.1, size = .1, outlier.colour = "gray80", color = "gray70", alpha = .5) + 
  geom_point(data = finalDf_long_schemes_medians_multiExtraction, aes(x = species, y = median, color = factor(Replica)), shape = 23, size = .1, position=position_dodge(1), alpha = .5) +
  geom_point(data = theTruthPlot_multiExtraction, aes(x = species, y = pWeight), shape = 8, color = "red") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.05)) + 
  #scale_fill_manual(values = c("yellow", "orange", "black")) + 
  labs(fill = "", x = "Species", y = "Proportion (in weight)") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 5), 
    axis.text = element_text(size = 5), 
    legend.title = element_text(size = 5), 
    legend.text = element_text(size = 5), 
    strip.background = element_rect(
      fill = "black"
    ), 
    strip.text = element_text(
      color = "white", 
      size = 6
    ),
    legend.position = "none"
    ) + 
  coord_flip() + 
  facet_wrap(~scheme) 

ggsave(
  filename = paste0("finalComparisonSchemes_multiExtraction.png"),
  plot = comparisonSchemesPlot_multiExtraction,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 30,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)
