########################################################################################
#
# sampling
# 
########################################################################################
#
# This script samples from simulated flow of small pelagic catch flow 
# segmented in tons and buckets. 
# 
########################################################################################
#
#
# Authors:
#
# - Eros Quesada [first draft]
#
# Dev. Notes.
#
# 2024-07-01: Created. 
# 2024-11-05: The segmentation part is now divided by the sampling script. 
#
########################################################################################
#
# Load data ----- 
# Here we load the flow as it was created in the SmallPelagicFlowSim.R script.
#
########################################################################################

## Clean env
rm(list=setdiff(ls(), c("supportResultsDir", "simName", "p_herring"))); gc()

## Load the flow segmented 
load("~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes/Simulation4/Sim_4_flowBucketTonsAssigned.RData")

## Visualize the results
p <- flow %>% 
  melt %>%
  filter(Var3 == 6) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    value = as.character(value)
  ) %>% 
  ggplot(aes(column, row, fill = value), color = "black") +
  geom_tile() + 
  theme_bw() + 
  theme(legend.position = "none")

ggsave(
  filename = paste0("flowSegmentedInTons.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

p <- flow %>% 
  melt %>%
  filter(Var3 == 7) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    value = as.character(value)
  ) %>% 
  ggplot(aes(column, row, fill = value), color = "black") +
  geom_tile() + 
  theme_bw() + 
  theme(legend.position = "none") 

ggsave(
  filename = paste0("flowSegmentedInBuckets.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)


### Simple random sampling

## Demonstration 
## Set sampling frequency
samplingFrequency <- 30

## Extract randomly n buckets
extractedBucketsSRS <- sample(flow[,,7], samplingFrequency)

# Find corresponding tons
indexesSRS <- flow %>% 
  melt %>%
  filter(Var3 %in% c(6,7)) %>%
  mutate(selected = ifelse(Var3 == 7 & value %in% extractedBucketsSRS, 1, 0)) %>% 
  filter(Var3 == 7) %>% 
  #select(Var1, Var2, value) %>% 
  filter(selected == 1) %>%
  select(Var1, Var2) %>%
  rename(row = Var1, column = Var2) %>% 
  mutate(rowcol = paste0(row, "_",column))

tonBucketCombiSRS <- flow %>% 
  melt %>%
  filter(Var3 == 6) %>%
  rename(row = Var1, column = Var2) %>% 
  mutate(rowcol = paste0(row, "_",column)) %>% 
  filter(rowcol %in% indexesSRS$rowcol)

tonVecSRS <- unique(tonBucketCombiSRS$value)

## Visualise the tons selected 
f <- flow
p <- f %>% 
  melt %>%
  filter(Var3 == 6) %>% 
  mutate(selected = ifelse(is.na(value), NA, ifelse(value %in% tonVecSRS, 1, 0))) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    selected = as.character(selected)
  ) %>% 
  ggplot(aes(column, row, fill = selected), color = "black") +
  geom_tile() + 
  labs(fill = "Selected ton \n") +
  scale_fill_manual(
    values = c("gray", "red"), na.value = "black",
    labels = c("0", "1", "empty")) + 
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = paste0("selectedTonSRS.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## Visualise the buckets selected 
f <- flow
f[,,7] <- ifelse(is.na(f[,,7]), NA, ifelse(f[,,7] %in% extractedBucketsSRS, 1, 0))
table(f[,,7], useNA = "al")

# Plot
p <- f %>% 
  melt %>%
  filter(Var3 == 7) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    value = as.character(value)
  ) %>% 
  ggplot(aes(column, row, fill = value), color = "black") +
  geom_tile() + 
  labs(fill = "Selected bucket \n") +
  scale_fill_manual(
    values = c("gray", "red"), na.value = "black",
    labels = c("0", "1", "empty")) + 
  theme_bw() +
  theme(
    legend.position = "bottom"
    )

ggsave(
  filename = paste0("selectedBucketsSRS.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## First create an empty list of samples
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
finalDf_long_SRS <- samplesDf_SRS %>% 
  as.data.frame() %>% 
  dplyr::group_by(bucket, species) %>% 
  dplyr::summarize(
    weightSpecies = sum(weight)
  ) %>% 
  ungroup() %>% 
  group_by(bucket) %>% 
  mutate(
    weightTot = sum(weightSpecies)
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    pWeight = weightSpecies/weightTot
  )

finalDf_wide_SRS <- finalDf_long_SRS %>% 
  select(bucket, species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) 

# Store results for simple random sampling 
resultsSRSSampling <- finalDf_long_SRS %>% 
  select(bucket, species, pWeight) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(
    meanSampledProportion = mean(pWeight), 
    varSampledProportion = var(pWeight)
  )

### Systematic sampling

# Compute the possible ton combination. [Note this line was surpassed by using the bucket combination as systematic samples taken with low amount of tons provide sequences skipping part of the flow]
#possibleTonComb <- do_systematic_samples_N_n (N=length(unique(as.numeric(flow[,,6]))), n=30)

# Pick one randomly 
#tonVec <- possibleTonComb[sample(nrow(possibleTonComb), 1),]

# Extract the first bucket from each of the selected tons
#buckVec <- sapply(1:length(tonVec), function(x){
#  min(flow[,,7][which(flow[,,6] %in% tonVec[x], arr.ind=TRUE)])
#})

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
  #select(Var1, Var2, value) %>% 
  filter(selected == 1) %>%
  select(Var1, Var2) %>%
  rename(row = Var1, column = Var2) %>% 
  mutate(rowcol = paste0(row, "_",column))

tonBucketCombi <- flow %>% 
  melt %>%
  filter(Var3 == 6) %>%
  rename(row = Var1, column = Var2) %>% 
  mutate(rowcol = paste0(row, "_",column)) %>% 
  filter(rowcol %in% indexes$rowcol)

tonVecSS <- unique(tonBucketCombi$value)


## Visualise the tons selected 
f <- flow

p <- f %>% 
  melt %>%
  filter(Var3 == 6) %>% 
  mutate(selected = ifelse(is.na(value), NA, ifelse(value %in% tonVecSS, 1, 0))) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    selected = as.character(selected)
  ) %>% 
  ggplot(aes(column, row, fill = selected), color = "black") +
  geom_tile() + 
  labs(fill = "Selected ton \n") +
  scale_fill_manual(
    values = c("gray", "red"), na.value = "black",
    labels = c("0", "1", "empty")) + 
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = paste0("selectedTonsSS.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## Visualise the buckets selected 
f <- flow

p <- f %>% 
  melt %>%
  filter(Var3 == 7) %>% 
  mutate(selected = ifelse(is.na(value), NA, ifelse(value %in% bucketVec, 1, 0))) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    selected = as.character(selected)
  ) %>% 
  ggplot(aes(column, row, fill = selected), color = "black") +
  geom_tile() + 
  labs(fill = "Selected bucket \n") +
  scale_fill_manual(
    values = c("gray", "red"), na.value = "black",
    labels = c("0", "1", "empty")) + 
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = paste0("selectedBucketsSS.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

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
  samplesList[[i]] = list(
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
finalDf_long_SS <- samplesDf_SS %>% 
  as.data.frame() %>% 
  dplyr::group_by(bucket, species) %>% 
  dplyr::summarize(
    weightSpecies = sum(weight)
  ) %>% 
  ungroup() %>% 
  group_by(bucket) %>% 
  mutate(
    weightTot = sum(weightSpecies)
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    pWeight = weightSpecies/weightTot
  )

finalDf_wide_SS <- finalDf_long_SS %>% 
  select(bucket, species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) #%>% 

#rename("Weight proportion of herring" = 1, "Weight proportion of sprat" = 2)

# Store results for systematic sampling 
resultsSSSampling <- finalDf_long_SS %>% 
  select(bucket, species, pWeight) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(
    meanSampledProportion = mean(pWeight), 
    varSampledProportion = var(pWeight)
  )

## Finally, place together the results
# As a table
finalSummary_schemes <- rbind(
  resultsSRSSampling %>% mutate(scheme = "SRS") %>% relocate(scheme, .before = 1), 
  resultsSSSampling %>% mutate(scheme = "SS") %>% relocate(scheme, .before = 1)
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
    "V(P)" = 4, 
    "P" = 5, 
    "P - E(P)" = 6
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


