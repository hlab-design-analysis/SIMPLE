## Finally, place together the results
#  As a table
finalSummary_schemes_singleExtraction <- rbind(
  resultsSRSampling_singleExtraction %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  resultsSSampling_singleExtraction %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
) %>% 
  mutate(
    proportionTrue = ifelse(species == 1, 0.66, 0.33)  # To be replaced with p_herring 
  ) %>% 
  rowwise() %>% 
  mutate(
    diffSimTrue = abs(proportionTrue - meanSampledProportion)
  ) %>% 
  rename(
    "Scheme" = 1, 
    "Species" = 2, 
    "E(P)" = 3, 
    "P" = 4, 
    "P - E(P)" = 5
  )

# Save as image
tableSummaryImg_singleExtraction <- kable(finalSummary_schemes_singleExtraction) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_ComparisonSchemes_", simName, "_singleExtraction", ".jpg"), height = 1)

# As a plot
theTruthPlot_singleExtraction <- data.frame(
  scheme = "The truth", 
  bucket = NA, 
  species = c(1, 2),
  weightSpecies = NA,  
  weightTot = NA,
  pWeight = c(0.66, 0.33) # This should be replace with p_herring
)

finalDf_long_schemes_singleExtraction <- rbind(
  finalDf_long_SRS_singleExtraction %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  finalDf_long_SS_singleExtraction %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
)

comparisonSchemesPlot_singleExtraction <- finalDf_long_schemes_singleExtraction %>% 
  as.data.frame() %>%
  mutate(
    species = as.factor(ifelse(species == 1, "herring", "sprat"))
  ) %>% 
  ggplot() + 
  geom_boxplot(aes(x = species, y = pWeight, group = interaction(scheme, species)), position=position_dodge(1), size = .1) + 
  geom_point(data = finalDf_long_schemes_singleExtraction, aes(x = species, y = pWeight, group = interaction(scheme, species), shape = scheme), size = 2.5, position=position_dodge(1)) +
  geom_point(data = theTruthPlot_singleExtraction, aes(x = species, y = pWeight, fill = scheme), shape = 8, color = "red") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.05)) + 
  scale_fill_manual(values = c("yellow", "orange", "black")) + 
  scale_shape_manual(values = c("SRS" = 4, "SS" = 23)) + 
  labs(fill = "", x = "Species", y = "Proportion (in weight)") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 5), 
    axis.text = element_text(size = 5), 
    legend.title = element_text(size = 5), 
    legend.text = element_text(size = 5)
    ) + 
  coord_flip()

ggsave(
  filename = paste0("finalComparisonSchemes_singleExtraction.png"),
  plot = comparisonSchemesPlot_singleExtraction,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 10,
  units = "cm",
  dpi = 1000,
  bg = "white"
)
