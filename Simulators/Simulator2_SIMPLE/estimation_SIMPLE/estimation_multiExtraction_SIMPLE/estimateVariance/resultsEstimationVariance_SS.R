## Estimate variance results 

## P1: Summarize the results
# For SS 
df_varianceComparison_SS_multiExtraction_summary <- df_varianceComparison_SS_multiExtraction %>% # Extract the summary of the variance results
  t() %>% 
  as.data.frame() %>% 
  rowwise() %>% 
  mutate(
    scheme = "SS", 
    meanVariance = mean(c_across(all_of(2:nVarEstimators))),
    sdVariance = sd(c_across(all_of(2:nVarEstimators))),
    minVariance = min(c_across(all_of(2:nVarEstimators))), 
    maxVariance = max(c_across(all_of(2:nVarEstimators)))
  )


df_varianceComparison_SS_multiExtraction_summary <- df_varianceComparison_SS_multiExtraction_summary %>% # Extract the name of the estimators having minimum value at a certain replica
  as.data.frame() %>% 
  mutate(minimumVarianceBelongsTo = apply(.[,2:nVarEstimators], 1, function(x) names(x)[which.min(x)]))  %>%  # Credits: https://stackoverflow.com/questions/37196854/r-dplyr-get-name-of-which-min-rowwise
  tibble::rownames_to_column(var = "Replica") %>% 
  relocate(scheme, .before = 1)

# Select only relevant columns 
# df_varianceComparison_SS_multiExtraction_summary <- df_varianceComparison_SS_multiExtraction_summary %>% 
#   dplyr::select(
#      Replica, meanVariance, sdVariance, minVariance, maxVariance, minimumVarianceBelongsTo
#   )


# Save as a tables
table_df_varianceComparison_SS_multiExtraction_summary <- kable(df_varianceComparison_SS_multiExtraction_summary) %>% # Save as image
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_VarianceComparisonSchemes_", simName,"_multiExtraction_SS", ".jpg"))

df_varianceComparison_SS_multiExtraction_summary_leaderboard <- df_varianceComparison_SS_multiExtraction_summary %>% # Create a table reporting the score of variance estimators 
  group_by(minimumVarianceBelongsTo) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(
    scheme = "SS"
  )

## P2: Combine in a table the results for SRS and SS
df_varianceComparison_FINAL_multiExtraction_summary_leaderboard <- df_varianceComparison_SS_multiExtraction_summary_leaderboard %>% 
  arrange(
    desc(n)
  )

# Save as a table
table_df_varianceComparison_FINAL_multiExtraction_summary <- df_varianceComparison_FINAL_multiExtraction_summary_leaderboard %>% 
  dplyr::rename(
    "Estimator" = 1, 
    "Performance \n (how many times lowest in variance)" = 2,
    "Scheme" = 3
  ) %>% 
  kable() %>% # Save as image
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = TRUE) %>%
  as_image(file = paste0("results_SIMPLE/Simulation", simName, "/results_VarianceComparisonSchemes_", simName,"_multiExtraction_FINAL", ".jpg"))

## P2: Plot the results
df_varianceComparison_FINAL_multiExtraction_summary_leaderboard_plot <- df_varianceComparison_FINAL_multiExtraction_summary_leaderboard %>%
  ggplot() + 
  geom_col(aes(x = reorder(minimumVarianceBelongsTo, n), y = n, fill = minimumVarianceBelongsTo)) + 
  coord_flip() + 
  labs(x = "Variance estimator", y = "Number of time minimum variance in replicas") + 
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
  ) 

# Save the results
ggsave(
  filename = paste0("finalComparisonSchemes_varianceEstimation_multiExtraction.png"),
  plot = df_varianceComparison_FINAL_multiExtraction_summary_leaderboard_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 30,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)  