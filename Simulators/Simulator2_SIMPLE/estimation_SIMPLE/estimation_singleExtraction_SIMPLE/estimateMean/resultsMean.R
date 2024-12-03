## Finally, place together the results
#  As a table
finalSummary_schemes <- rbind(
  resultsSRSampling %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  resultsSSampling %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
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
tableSummaryImg <- kable(finalSummary_schemes) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_ComparisonSchemes_", simName, ".jpg"), height = 1)

# As a plot
theTruthPlot <- data.frame(
  scheme = "The truth", 
  bucket = NA, 
  species = c(1, 2),
  weightSpecies = NA,  
  weightTot = NA,
  pWeight = c(0.66, 0.33) # This should be replace with p_herring
)

finalDf_long_schemes <- rbind(
  finalDf_long_SRS %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  finalDf_long_SS %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
)

comparisonSchemesPlot <- finalDf_long_schemes %>% 
  as.data.frame() %>%
  mutate(
    species = as.factor(ifelse(species == 1, "herring", "sprat"))
  ) %>% 
  ggplot() + 
  geom_boxplot(aes(x = species, y = pWeight, fill = scheme), position=position_dodge(1)) + 
  geom_point(data = theTruthPlot, aes(x = species, y = pWeight, fill = scheme), shape = 8, color = "red") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.05)) + 
  scale_fill_manual(values = c("yellow", "orange", "black")) + 
  labs(fill = "", x = "Species", y = "Proportion (in weight)") + 
  theme_bw() +
  theme(axis.text = element_text(size = 6)) + 
  coord_flip()

ggsave(
  filename = paste0("finalComparisonSchemes.png"),
  plot = comparisonSchemesPlot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 10,
  units = "cm",
  dpi = 500,
  bg = "white"
)
