########################################################################################
#
# SmallPelagicFlowSim
# 
########################################################################################
#
# This script produce a simulation of small pelagic catch flow in 2D, based on matrices.
# 
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
#
# 2024-03-03: Created. Developed tank, fish pourer/sinker, tube and flow between them
# 2024-03-13: Add proportions at different steps in tank and tube on the plot  
# 2024-03-15: Add the possibility of including more than one haul and proportion sensors. 
# 2024-05-**: Various fixes to bugs and optimizations. 
# 2024-06-**: Various fixes to bugs and optimizations. 
#
########################################################################################
########################################################################################
#
# Create the haul list -----
# Here we create an haul list
#
########################################################################################
## P1: Make the haul list
# The haul list is created starting from the parameters specified in "Set parameters". 
# It reports a list of hauls, with each haul being a list of catch, proportion of herring
# and proportion of sprat. 
haulsList <- makeHaulsList(
  nHaul = nHaul, 
  p_herring = p_herring,
  W = W
)


########################################################################################
#
# Additional calculations ----
# Here we set the initial condition for the simulation
#
########################################################################################

## P1: Select a sample for each species 
df_her<-df_her[df_her$sampId=="2009_2113",]
df_spr<-df_spr[df_spr$sampId=="2009_2105",]

## P2: Plot coupled distribution (weight and length)
df_all <- rbind(df_her %>% mutate(species = "her"), df_spr %>% mutate(species = "spr"))
LWR_plot <- ggExtra::ggMarginal(
  ggplot(df_all) + 
    geom_point(aes(x = indWt, y = lenCls, color = species), alpha = .5) + 
    scale_color_manual(
      labels = c("her" = "herring", "spr" = "sprat"), 
      values = c("her" = "blue", "spr" = "red")
    ) + 
    theme_bw() + 
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      legend.position = "bottom", 
      legend.text =  element_text(size = 8),
      legend.title = element_text(size = 9),
    ) + 
  labs(x = "Weight", y = "Length"),
  type = "density", 
  margins = "both",
  size = 8, 
  groupColour = TRUE, 
  groupFill = TRUE, 
  yparams = list(n=40, na.rm = T)
)
LWR_plot 

## Save plot
ggsave(
  filename = paste0("LWRcatch_", "sim", simName,".png"),
  plot = LWR_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## P3: Give a look to the length and weight relationship 
mh <- lm(log(lenCls) ~ log(indWt), data = df_her); rh <- summary(mh)$adj.r.squared
ms <- lm(log(lenCls) ~ log(indWt), data = df_spr); rs <- summary(ms)$adj.r.squared
lwrDf <- rbind(
  cbind(
    species = "her", 
    expand.grid(
      indWt = seq(0,max(df_her$indWt), 1), 
      lenCls = seq(0,max(df_her$lenCls), 1)
    ), 
    pred = predict(
      mh,
      newdata = expand.grid(
        indWt = seq(0,max(df_her$indWt), 1), 
        lenCls = seq(0,max(df_her$lenCls), 1)
      ))
    ), 
  cbind(
    species = "spr", 
    expand.grid(
      indWt = seq(0,max(df_spr$indWt), 1), 
      lenCls = seq(0,max(df_spr$lenCls), 1)
    ), 
    pred = predict(
      ms, 
      newdata = expand.grid(
        indWt = seq(0,max(df_spr$indWt), 1), 
        lenCls = seq(0,max(df_spr$lenCls), 1)
        )
      )
  )
)

LWRmod_plot <- ggplot() + 
  geom_point(data = df_all, aes(x = indWt, y = lenCls, color = species), alpha = .5) + 
  geom_line(data = lwrDf, aes(x = indWt, y = exp(pred)), color = "black") + 
  scale_color_manual(
    labels = c("her" = "herring", "spr" = "sprat"), 
    values = c("her" = "blue", "spr" = "red")
  ) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.position = "bottom", 
    legend.text =  element_text(size = 8),
    legend.title = element_text(size = 9),
    strip.background = element_rect(
      fill = "black"
    ), 
    strip.text = element_text(
      color = "white"
    )
  ) + 
  facet_wrap(~species)

# Show plot
LWRmod_plot
dev.off()

# Save plot
ggsave(
  filename = paste0("LWRmod_", "sim", simName,".png"),
  plot = LWRmod_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## P2: Determine the length distribution 
lf_her <- prop.table(table(df_her$lenCls)); round(lf_her*100,1); barplot(lf_her)
lf_spr <- prop.table(table(df_spr$lenCls)); round(lf_spr*100,1); barplot(lf_spr)

## P3: Determine the weight distribution 
wf_her <- prop.table(table(df_her$indWt)); names(wf_her) <- as.integer(names(wf_her))/1000; round(wf_her*100,1)
wf_spr <- prop.table(table(df_spr$indWt)); names(wf_spr) <- as.integer(names(wf_spr))/1000; round(wf_spr*100,1)

## P4: Plot 
wf_her_plot <- reshape2::melt(table(df_her$indWt)) %>% 
  dplyr::select(-Var1) %>% 
  cbind(weightClass = as.factor(names(wf_her))) %>% 
  ggplot() + 
  geom_col(aes(y = value, x = weightClass), fill = "blue", color = "black", alpha = .5) + 
  labs(x = "Weight class", y = "Frequency") + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 8, angle = 90),
    axis.title = element_text(size = 9),
    legend.position = "bottom", 
    legend.text =  element_text(size = 8),
    legend.title = element_text(size = 9)
  )

wf_spr_plot <- reshape2::melt(table(df_spr$indWt)) %>% 
    dplyr::select(-Var1) %>% 
    cbind(weightClass = as.factor(names(wf_spr))) %>% 
    ggplot() + 
    geom_col(aes(y = value, x = weightClass), fill = "red", color = "black", alpha = .5) + 
    labs(x = "Weight class", y = "Frequency") + 
    theme_bw() +
    theme(
      axis.text = element_text(size = 8, angle = 90),
      axis.title = element_text(size = 9),
      legend.position = "bottom", 
      legend.text =  element_text(size = 8),
      legend.title = element_text(size = 9)
    )

wf_plot <- ggarrange(
  wf_her_plot,
  wf_spr_plot, 
  nrow = 1, 
  labels = "AUTO"
)

# Show plot 
wf_plot

# Save plot
ggsave(
  filename = paste0("wf_",  "sim", simName,".png"),
  plot = wf_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## P4: Extract fishes from the weight distribution until the weight of the haul is reached
# First, tune the search. The following parameter is the number of fishes added to find the number of fishes needed to fill the haul catch for a given species. 
tuneSearch <- 10000 #  The higher the faster, but the less precise match with the haul catch (e.g. 10000 -> ~50kg and ~35kg imprecision on herring and sprat respectively).

# For herring
species = "herring"
for(h in 1:nHaul){
  cat("Searching the number of", species, " needed to fill the catch in", h, "\n")
  idealNumber <- floor((haulsList[[h]]$catch_w*haulsList[[h]]$p_her)/(max(df_her$indWt, na.rm = T)/1000)) # Start from the number of fishes given the max weight 
  sumFish <- 0
  
  while(
    sumFish <= (haulsList[[h]]$catch_w*haulsList[[h]]$p_her)
  ){
    cat(silver("trying with", idealNumber, ":"))
    
    possibleNbyW <- reshape2::melt(
      floor(
        idealNumber*wf_her
      ),
      value.name = "Individuals"
    ) %>% 
      rename(
        weightClass = 1
      )
    
    possibleNbyWList <- split(possibleNbyW, possibleNbyW$weightClass)
    
    for(wClass in 1:length(possibleNbyWList)){
      possibleNbyWList[[wClass]] <- rep(possibleNbyWList[[wClass]]$weightClass, possibleNbyWList[[wClass]]$Individuals)
    }
    
    sumFish <- sum(list_c(possibleNbyWList))
    
    if(sumFish <= (haulsList[[h]]$catch_w*haulsList[[h]]$p_her)){
      
      # The number of sprat fishes to fill the catch of the haul was not found
      cat(silver("not enough,", (haulsList[[h]]$catch_w*haulsList[[h]]$p_her)-sumFish," kg missing to fill the catch of", species, "in Haul", h, "\n"))
      
      # Hence, increase by the number of fishes specified in the tuneSearch
      idealNumber <- idealNumber + tuneSearch
      
    } else {
      
      # The number of sprat fishes to fill the catch of the haul was found
      cat(green("\n", "Completed -"), "It takes", idealNumber, species, "fishes to fill the", (haulsList[[h]]$catch_w*haulsList[[h]]$p_her), "kg catch of", species, "in Haul", h, "[imprecision:", abs((haulsList[[h]]$catch_w*haulsList[[h]]$p_her)-sumFish),"kg ]", "\n")
      
    }
  }
  
  # Collect the fishes extracted in the haul list
  haulsList[[h]]$catch_wsh <- list_c(possibleNbyWList)
  
  # Mix the fishes 
  haulsList[[h]]$catch_wsh <- sample(haulsList[[h]]$catch_wsh)
  
}

# For sprat
species = "sprat"
for(h in 1:nHaul){
  cat("Searching the number of", species, " needed to fill the catch in", h, "\n")
  idealNumber <- floor((haulsList[[h]]$catch_w*haulsList[[h]]$p_sprat)/(max(df_spr$indWt, na.rm = T)/1000)) # Start from the number of fishes given the max weight 
  sumFish <- 0
  
  while(
    sumFish <= (haulsList[[h]]$catch_w*haulsList[[h]]$p_sprat)
  ){
    cat(silver("trying with", idealNumber, ":"))
    
    possibleNbyW <- reshape2::melt(
      floor(
        idealNumber*wf_spr
      ),
      value.name = "Individuals"
    ) %>% 
      rename(
        weightClass = 1
      )
    
    possibleNbyWList <- split(possibleNbyW, possibleNbyW$weightClass)
    
    for(wClass in 1:length(possibleNbyWList)){
      possibleNbyWList[[wClass]] <- rep(possibleNbyWList[[wClass]]$weightClass, possibleNbyWList[[wClass]]$Individuals)
    }
    
    sumFish <- sum(list_c(possibleNbyWList))
    
    if(sumFish <= (haulsList[[h]]$catch_w*haulsList[[h]]$p_sprat)){
      
      # The number of sprat fishes to fill the catch of the haul was not found
      cat(silver("not enough,", haulsList[[h]]$catch_w-sumFish," kg missing to fill the catch of", species, "in Haul", h, "\n"))
      
      # Hence, increase by the number of fishes specified in the tuneSearch
      idealNumber <- idealNumber + tuneSearch
          
    } else {
      
      # The number of sprat fishes to fill the catch of the haul was found
      cat(green("\n", "Completed -"), "It takes", idealNumber, species, "fishes to fill the", (haulsList[[h]]$catch_w*haulsList[[h]]$p_sprat), "kg catch of", species, "in Haul", h, "[imprecision:", abs((haulsList[[h]]$catch_w*haulsList[[h]]$p_spr)-sumFish),"kg ]", "\n")
      
    }
  }
  
  # Collect the fishes extracted in the haul list
  haulsList[[h]]$catch_wss <- list_c(possibleNbyWList)
  
  # Mix the fishes 
  haulsList[[h]]$catch_wss <- sample(haulsList[[h]]$catch_wss)
  
}

## P5: Extract the total number of fishes caught in the NHauls
NFishes <- sum(sapply(haulsList, function(x) sum(length(x[[4]]), length(x[[5]]))))

## Check that n fishes stay into the tank
if(NFishes - (tankHeight*tankLength)>0){
  stop("enlarge the tank, fishes caught won´t fit!")
} else {
    "Tank is large enough to store the fishes caught"
}

## P5: Check results
# Extract the weights sampled for each haul
checkDf <- do.call(
  rbind, 
  lapply(
    1:length(haulsList), 
    function(x) data.frame(
      rbind(
        data.frame(
          haul = paste(x), 
          species = "Sprat", 
          weights = haulsList[[x]]$catch_wss
          ), 
        data.frame(
          haul = paste(x), 
          species = "Herring", 
          weights = haulsList[[x]]$catch_wsh          
        ) 
      )
    )
  )
)

# Plot
DistExtractedFish_plot <- ggplot(checkDf) + 
    geom_histogram(
      aes(
        x = weights, 
        fill = haul), 
      bins = 10
    ) +  
    labs(x = "Weight", y = "Frequency") + 
    theme_bw() + 
    theme(
      axis.text = element_text(size = 8, angle = 90),
      axis.title = element_text(size = 9),
      legend.position = "bottom", 
      legend.text =  element_text(size = 8),
      legend.title = element_text(size = 9), 
      strip.background = element_rect(
        fill = "black"
      ), 
      strip.text = element_text(
        color = "white"
      )
    ) + 
  facet_wrap(~species, scales = "free")

ggsave(
  filename = paste0("DistExtractedFish", "sim", simName,".png"),
  plot = DistExtractedFish_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## P5: Build haul array 
# The haul array is a 3d matrix which specify species weight, volume for each fish in each haul. 
for(h in 1:length(haulsList)){
  cat(paste0("Process haul: ", h, "\n"))
  
  # Generate fish identifier matrix 
  matIdHer <- matrix(1:length(haulsList[[h]]$catch_wsh), length(haulsList[[h]]$catch_wsh), 1) # One column, each row is a herring fish  with the number of matrix rows being the number of weights extracted above. 
  matIdSpr <- matrix((length(haulsList[[h]]$catch_wsh)+1):(length(haulsList[[h]]$catch_wsh)+length(haulsList[[h]]$catch_wss)), length(haulsList[[h]]$catch_wss), 1) # One column, each row is a sprat fish with the number of matrix rows being the number of weights extracted above. 
  matIdSpecies <- c(matIdHer, matIdSpr) # connect the two matrices 
  
  # Generate species species matrix 
  matSpeciesHer <- matrix("herring", length(haulsList[[h]]$catch_wsh), 1) # One column, each row is a herring fish  with the number of matrix rows being the number of weights extracted above. 
  matSpeciesSpr <- matrix("sprat", length(haulsList[[h]]$catch_wss), 1) # One column, each row is a sprat fish with the number of matrix rows being the number of weights extracted above. 
  matSpeciesSpecies <- rbind(matSpeciesHer, matSpeciesSpr) # connect the two matrices 
  
  # Extract species weights matrix
  matWeightsHer <- matrix(haulsList[[h]]$catch_wsh, length(haulsList[[h]]$catch_wsh), 1) # One column, each row is a weight with the weight extracted above. 
  matWeightsSpr <- matrix(haulsList[[h]]$catch_wss, length(haulsList[[h]]$catch_wss), 1) # One column, each row is a weight with the weight extracted above. 
  matWeightsSpecies <- rbind(matWeightsHer, matWeightsSpr) # connect the two matrices 
  
  # Generate species volume matrix
  matVolumeHer <- (matWeightsHer*1000)/932.274568364 # density her = 932.274568364 gram/
  matVolumeSpr <- (matWeightsSpr*1000)/852.182251494 # density spr = 852.182251494 gram/liter
  matVolumeSpecies <- rbind(matVolumeHer, matVolumeSpr) # connect the two matrices 
  
  # Gather the matrices the final array 
  haulArray <- array(c(matIdSpecies, matSpeciesSpecies, matWeightsSpecies, matVolumeSpecies), c((length(haulsList[[h]]$catch_wsh) + length(haulsList[[h]]$catch_wss)),1,4))
  
  # Mix the catch 
  indexes <- sample(1:nrow(haulArray)) # Defines the random order of elements in each of the matrices 
  for(fishFeature in 1:dim(haulArray)[3]){ # Each of the features (vol, weight, species) gets reordered with the same random order
    haulArray[,,fishFeature] <- haulArray[indexes,,fishFeature]
  }
  
  # Get rid of the information on weights for herring and sprat in haulsList
  haulsList[[h]] <- haulsList[[h]][-c(4,5)]  
  
  # Append haulArray to haulList
  haulsList[[h]][length(haulsList[[h]])+1] <- list(haulArray)
  names(haulsList[[h]]) <- c("catch_w", "p_herring", "p_sprat", "fishes")   
  
  # Turn in numbers the species
  haulsList[[h]]$fishes[,,2] <- ifelse(haulsList[[h]]$fishes[,,2] == "herring", 1, 2)
   
}

## P3: Try to extract n samples
#  If we sample from these a very high number of times, we should obtain the probability given.
#  For instance the proportions in haul 2 were
haulsList[[2]][2] # For herring
haulsList[[2]][3] # For sprat
replics = 100000  # Thus resampling 100000 times from our speciesVector in haul 1 we expect to reach these numbers
checkDf <- data.frame(
  cbind(
    species = ifelse(haulsList[[2]]$fishes[,1,2] == 1, "herring", "sprat"), 
    weight = haulsList[[2]]$fishes[,1,3]
    )  
)
checkDf <- checkDf[replicate(replics,dqsample(1:nrow(checkDf), 1, replace = F)), ]
checkDf$weight <- as.numeric(checkDf$weight)
round(tapply(checkDf$weight, checkDf$species, sum)/sum(checkDf$weight)*100, 2) # These should reflect haulsList[[2]][2] # For herring and haulsList[[2]][3] # For sprat
prop.table(table(ifelse(haulsList[[3]]$fishes[,,2] == 1, "herring", "sprat"))) # and the proportion of fishes. 


## P4: We finally obtain the features of the haul enclosed in the haulList object: 
# For instance for the first haul
haulsList$Haul_1 # Content of the first haul
haulsList$Haul_1$catch_w # Catch in the first haul
haulsList$Haul_1$p_herring # Proportion of herring in the first haul
haulsList$Haul_1$p_sprat # Proportion of sprat in the first haul
haulsList$Haul_1$fishes[,,1] # Identifier of the single fishes in the first haul
haulsList$Haul_1$fishes[,,2] # Species of the single fishes in the first haul
haulsList$Haul_1$fishes[,,3] # Weight of the single fishes in the first haul
haulsList$Haul_1$fishes[,,4] # Volume of the single fishes in the first haul

########################################################################################
#
# Build the tank ----
# Here we build the tank according to selections
#
########################################################################################
## P1: Build the tank.
# Note that if more than one haul is present the tank will be rebuilt below, with the 
# same tankHeight and tankLength. 
tank <- buildTank(
  tankHeight = tankHeight, # 700
  tankLength = tankLength, # 500
  bigTank = bigTank, 
  plot = 0
)

ggsave(
  filename = paste0("emptyTank_", "sim", simName,".png"),
  plot = drawTank(tank, plot = 1, type = "species"),
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

########################################################################################
#
# Pour fishes in tank ----
# Here we pour a stream of fishes from the haulsList to the tank
# This is done by selecting randomly positions in the tank matrix (n = n fishes) and 
# assigning fishes to them. Then the fishes are sunk to the bottom of the tank matrix. 
# If more than one haul is present, the magnitude of the hauls determine the position of 
# the fishes belonging to them. In practice the tank matrix is divided in submatrices and
# each submatrix is assigned to an haul. The extension of the submatrix is proportional to 
# the magnitude of the haul. The reasoning about the assignation of fishes belonging to one 
# haul to the relative submatrix is the same as in the one haul case. The fishes are finally 
# sunk to the bottom all together. 
#
########################################################################################

## P1: Pour the fishes in the tank
# First, we need to take into account the eventual presence of different hauls and their magnitude. 
if(nHaul == 1){
  
  # Select random position in the first layer of the matrix. 
  positions <- dqsample(length(tank)/5, length(haulsList[[1]]$fishes[,,1])) # The "/4" serves to consider only the first dimension of the tank array
  tank[positions] <- as.numeric(unlist(haulsList[[1]]$fishes[,,1])) # Send the identifier of the fish to the first layer of the matrix
  tank[positions+((length(tank)/5)*1)] <- as.numeric(unlist(haulsList[[1]]$fishes[,,2])) # Estrapolate the same position on the second layer of the tank matrix and add species accordingly
  tank[positions+((length(tank)/5)*2)] <- as.numeric(unlist(haulsList[[1]]$fishes[,,3])) # Estrapolate the same position on the third layer of the tank matrix and add weight accordingly
  tank[positions+((length(tank)/5)*3)] <- as.numeric(unlist(haulsList[[1]]$fishes[,,4])) # Estrapolate the same position on the fourth layer of the tank matrix and add volume accordingly
  tank[positions+((length(tank)/5)*4)] <- "Haul_1" # Add haul of origin
  
  # Show the results (one could imagine that this is a random time when fishes are falling into the tank from above)
  if(bigTank == 1){
    tankR <- apply(tank, 2, rev)
    image(t(tankR))
  }else{
    tankDraw <- tank
    drawTank(tankDraw, plot = 1, type = "species")  
  }
  
} else {
  
  ## In case more than one haul is involved it is a bit trickier, due to the way R indexes matrices. 
  # First we need to assign increasing numbers by row to the tank matrix, as R use columns to index. 
  tanK <- matrix(1:(tankHeight*tankLength), tankHeight, tankLength, byrow = T) # Fill it with numbers from first two last cell, R will fill column
  tanK <- tanK[nrow(tanK):1,] # To mimic that the bottom is the first to fill, we flip it also vertically
  # Now the bottom left cell of the matrix value is 1, the right top cell value is length(tank) - the maximum. 
  
  # Extract the number of fishes in the set of hauls 
  for(h in 1:nHaul){
      nFish_temp <- length(haulsList[[h]]$fishes[,,2])
    if(h == 1){
      nFish <- nFish_temp
    }else{
      nFish <- c(nFish, nFish_temp)
    }
  }
  
  # Extract the volume of fishes in the set of hauls 
  for(h in 1:nHaul){
      vFish_temp <- sum(as.numeric(haulsList[[h]]$fishes[,,4]))
    if(h == 1){
      vFish <- vFish_temp
    }else{
      vFish <- c(vFish, vFish_temp)
    }
  }
  
  # Second, we extract the proportion of volume each hauls occupies and thus the cell on the first layer representing that proportion
  propTank <- floor(length(tanK)*prop.table(nFish)) 
  
  # Third we create a list where to store the position by haul 
  positions = as.list(rep(NA, nHaul))
  names(positions) <- paste0("Haul_", 1:length(positions))
  
   # Allow to sample by haul only in the region of cells proportional to the magnitude of the haul
  # Start from the bottom with the first haul
  for(i in 1:nHaul){
    
    if(i == 1){ # When i equal to 1, we treat the first haul hence we need to start from bottom of matrix
      
      # Identify the cell value until, starting from the bottom of the tank, which the tank matrix may be occupated
      regionOccupable_start <- 1 # Indeed the first cell to start is the one bottom left, for which tank == 1. 
      regionOccupable_end <- propTank[i] # propTank[1] is the limit cell of the region that the haul 1 is allowed to occupy given its magnitude.
      
      # Select position at random in the tank matrix
      positions[i] <- list(
        dqsample(
          which(
            tanK %in% regionOccupable_start:ifelse(
              regionOccupable_end<regionOccupable_start,
              (regionOccupable_start + regionOccupable_end),
              regionOccupable_end
            )
          ), 
          nFish[i], 
          replace = F
        )
      )
      
      # Assign the fishes in the haul to the positions
      tank[as.numeric(unlist(positions[i]))] <- as.numeric(unlist(haulsList[[i]]$fishes[,,i])) # Send the identifier of the fish to the first layer of the matrix
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*1)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,2])) # Estrapolate the same position on the second layer of the tank matrix and add species accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*2)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,3])) # Estrapolate the same position on the third layer of the tank matrix and add weight accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*3)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,4])) # Estrapolate the same position on the fourth layer of the tank matrix and add volume accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*4)] <- "Haul_1" # Add haul of origin
      
    } else { # When between 1 and nHaul we need to start from the previous haul to the next
      
      # Update the cell value with the value until, starting from the value of the previous haul of the tank,  the tank matrix may be occupied
      regionOccupable_start <- ifelse(i==1, 1, ifelse(i==2, propTank[i-1], sum(propTank[1:(i-1)])))
      regionOccupable_end <- ifelse(i==1, propTank[i], propTank[i]+sum(propTank[1:(i-1)]))
       
      
      # Select position at random in the tank matrix
      positions[i] <- list(
        dqsample(
          which(
            tanK %in% regionOccupable_start:regionOccupable_end
          ), 
          nFish[i], 
          replace = F
        )
      )
      
      # Assign the fishes in the haul to the positions
      tank[as.numeric(unlist(positions[i]))] <- as.numeric(unlist(haulsList[[i]]$fishes[,,i])) # Send the identifier of the fish to the first layer of the matrix
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*1)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,2])) # Estrapolate the same position on the second layer of the tank matrix and add species accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*2)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,3])) # Estrapolate the same position on the third layer of the tank matrix and add weight accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*3)] <- as.numeric(unlist(haulsList[[i]]$fishes[,,4])) # Estrapolate the same position on the fourth layer of the tank matrix and add volume accordingly
      tank[as.numeric(unlist(positions[i]))+((length(tank)/5)*4)] <- paste0("Haul_", i)  # Add haul of origin
  
    
    }
    
    # Turn NA into zeros
    tank <- ifelse(is.na(tank), 0, tank)

  }
}

## P3: Plot the result of the pouring for the main tank matrix
drawTank(tank, plot = 1, type = "species")
drawTank(tank, plot = 1, type = "weight")
drawTank(tank, plot = 1, type = "volume")
drawTank(tank, plot = 1, type = "hauls")

## P3: Plot the result of the pouring including the haul tank twin matrix 
dev.off() 
pouredFishInTank_plot <- ggarrange(
  drawTank(tank, plot = 1, type = "species") + theme(legend.position = "bottom", legend.direction = "horizontal"),
  drawTank(tank, plot = 1, type = "weight") + theme(legend.position = "bottom", legend.direction = "horizontal"), 
  drawTank(tank, plot = 1, type = "volume") + theme(legend.position = "bottom", legend.direction = "horizontal"),
  drawTank(tank, plot = 1, type = "hauls") + theme(legend.position = "bottom", legend.direction = "horizontal"), 
  nrow = 1
)

## P7: Save the plot 
ggsave(
  filename = paste0("allPouredTank_", "sim", simName,".png"),
  plot = pouredFishInTank_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

# Check results
# as.numeric(table(tank[,,2])["1"]) == sum(table(haulsList$Haul_1$fishes[,,2])["1"], table(haulsList$Haul_2$fishes[,,2])["1"], table(haulsList$Haul_3$fishes[,,2])["1"]) # Should be T

########################################################################################
#
# Sink fishes to the bottom of the tank ----
# Here we send the fishes caught in the haul(s) to the bottom of the tank. 
# The fishes in the current version go down vertically, depending if an empty cell is
# available below your position for their fall. This implied that in case of more than 
# one hauls a bit of mixing at the zone of contact with the catch may be expected. 
#
########################################################################################

## P1: Sink the fishes for the main tank matrix
# ! This part should be optimized
for(d in 1:dim(tank)[3]){
  for(j in 1:ncol(tank[,,d])){
    tank[,,d] <- ifelse(tank[,,d] == 0, NA,tank[,,d])
    recFullCol <- tank[!is.na(tank[,j,d]),j,d]
    nRecFullCol <- length(recFullCol)
    tank[,j,d] <- c(rep(NA, tankHeight-nRecFullCol), recFullCol)
    }  
  }

## Save the full tank to save time in case of re-run
save(tank, file = file.path(paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation",simName,"/tankFull_sim", simName, ".R")))
save(haulsList, file = file.path(paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation",simName,"/haulList_sim", simName, ".R")))

## P3: Plot the results for the main tank matrix
if(bigTank == 1){
  tankR <- apply(tank, 2, rev)
  image(t(tankR))
}else{
  drawTank(tank, plot = 1, type = "species")  
}

## P4: Plot the results including the haul tank twin matrix
sinkedFishInTank_plot <- ggarrange(
  drawTank(tank, plot = 1, type = "species") + theme(legend.position = "bottom", legend.direction = "horizontal"),
  drawTank(tank, plot = 1, type = "weight") + theme(legend.position = "bottom", legend.direction = "horizontal"), 
  drawTank(tank, plot = 1, type = "volume") + theme(legend.position = "bottom", legend.direction = "horizontal"),
  drawTank(tank, plot = 1, type = "hauls") + theme(legend.position = "bottom", legend.direction = "horizontal"), 
  nrow = 1
)

## P7: Save the plot 
ggsave(
  filename = paste0("allSinkedTank_", "sim", simName,".png"),
  plot = sinkedFishInTank_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

########################################################################################
#
# Build the tube ----
# Here we build the tube according to selections
#
########################################################################################
## P1: Build the tube
flowtube <- buildTube(
  heightTube = heightTube, 
  lengthTube = lengthTube, 
  plot = 1
)

## P7: Save the plot 
ggsave(
  filename = paste0("emptyTube_", "sim", simName,".png"),
  plot = drawTube(flowtube, plot = 1, type = "species", legend = 1),
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

########################################################################################
#
# Build the connection ----
# Here we build the connection between the tank and the tube
#
########################################################################################
## P1: Build the connection between tank and tube
#tank <- ifelse(tank == "0", NA, tank)
flowWoProp_plot <- suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 0, sizeLabelText = .1))

## P7: Save the plot 
ggsave(
  filename = paste0("flowWoProp_", "sim", simName,".png"),
  plot = flowWoProp_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## P2: Add proportion revealed at different intervals of the connection between tank and tube
flowWiProp_plot <- suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 1, sizeLabelText = .1))

## P7: Save the plot 
ggsave(
  filename = paste0("flowWiProp_", "sim", simName,".png"),
  plot = flowWiProp_plot,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

########################################################################################
#
# Flow the fishes inside the tube ----
# Here we flow the fishes between the tank and the tube
#
########################################################################################
# Reload tank and tube if needed
# load("~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes/Simulation4/tankFull_sim4.R")
# flowtube <- buildTube(
#   heightTube = heightTube,
#   lengthTube = lengthTube,
#   plot = 1
# )
# tank[1:1900,,] <- NA
# suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 1, sizeLabelText = .1))
# tank[1:1998,,] <- NA
## P1: Set the parameter for the flow
timeSteps = length(tank)/heightTube
plotFlow = 0
plotFlowEach = 10000
saveFlow = 0
quality = 100
addition = 0
ltb <- list(flowtube)

# The following lines avoid to produce an empty frame when saving the plot, this happens for time being multiplier of tank width x tube height, as there are no more fishes in the last tank row
toAvoid <- which(1:timeSteps %% (tankLength/heightTube) == 0) + 1
# Avoid the coord fixed message, credits: https://github.com/tidyverse/ggplot2/issues/2799
cf <- coord_fixed() 
cf$default <- T

# Set the iteration counter
t = 0

# Start the flow
while(!(all(is.na(tank[1:nrow(tank)-1,,1])) & sum(!is.na(tank[nrow(tank),,1])) == 0 & all(flowtube[,,]=="0") & (!t %% lengthTube == 0))){
  
  if(t == 0){
    
    tankRow <- tankHeight # We speicify the row of the Tank we are sampling
    cat(silver("\n", "Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),,1]))), "\n")
    
    # Select the cells with filled values in the tank
    noNAs <- which(!is.na(tank), arr.ind=T)
    
    # Select the columns of the cells with filled values in the bottom of the tank
    noNAs <- unique(noNAs[noNAs[,1] == nrow(tank),2])
    
    # Now we have the row of interest (tankHeight) and the columns filled, we sample a number of columns equal to the tube height to fill the tube
    extractedInd <- as.numeric(sample(as.character(noNAs), heightTube)) # This is the column index
    extracted <- tank[nrow(tank), extractedInd,] # These are the records corresponding to the column extracted at the bottom of the tank. 
    
    # Move the extracted to the tube (i.e. copy in the tube and turn the cell in the tank to NA) 
    flowtube[c(1:heightTube),1,] <- extracted
    tank[nrow(tank), extractedInd,] <- NA 
    
    # Increase the timestep
    t <- t + 1
    
  } else {
    
    if(sum(!is.na(tank[nrow(tank),,1])) >= heightTube ){ # If more than n (= heightTube) elements are present in the last row of the tank, we extract 10 randomly
      
      cat(silver("Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),,1]))), "\n")
      
      # Select the cells with filled values in the tank
      noNAs <- which(!is.na(tank), arr.ind=T)
      
      # Select the columns of the cells with filled values in the bottom of the tank
      noNAs <- unique(noNAs[noNAs[,1] == nrow(tank),2])
      
      # Now we have the row of interest (tankHeight) and the columns filled, we sample a number of columns equal to the tube height to fill the tube
      extractedInd <- as.numeric(sample(as.character(noNAs), heightTube)) # This is the column index
      extracted <- tank[nrow(tank), extractedInd,] # These are the records corresponding to the column extracted at the bottom of the tank. 
      
      # Shift plus one to the right the tube matrix to simulate flow and empty the first column to allocate upcoming fishes
      flowtube[,2:lengthTube,] <- flowtube[,1:lengthTube-1,] 
      flowtube[,1,] <- NA 
      
      # Move the extracted to the tube (i.e. copy in the tube and turn the cell in the tank to NA) 
      flowtube[c(1:heightTube),1,] <- extracted
      tank[nrow(tank), extractedInd,] <- NA 
      
      # Increase the timestep
      t <- t + 1
      #print(flowtube[,1:10]) # We print the results
      
      
    } else if (sum(!is.na(tank[nrow(tank),,1])) < heightTube & sum(!is.na(tank[nrow(tank),,1])) >=1) { # If more less than 10 elements are present in the last row of the tank, we extract what is there
      
      cat(silver("Iteration:", t, " - ", "Fishes to sample in tank bottom (matrix row", tankRow, ") :", sum(!is.na(tank[nrow(tank),,1]))), "\n")
      
      # Select the cells with filled values in the tank
      noNAs <- which(!is.na(tank), arr.ind=T)
      
      # Select the columns of the cells with filled values in the bottom of the tank
      noNAs <- unique(noNAs[noNAs[,1] == nrow(tank),2])
      
      # Now we have the row of interest (tankHeight) and the columns filled, we sample a number of columns equal to the tube height to fill the tube
      extractedInd <- as.numeric(sample(as.character(noNAs))) # This is the column index
      extracted <- tank[nrow(tank), extractedInd,] # These are the records corresponding to the column extracted at the bottom of the tank. 
      
      # Shift plus one to the right the tube matrix to simulate flow and empty the first column to allocate upcoming fishes
      flowtube[,2:lengthTube,] <- flowtube[,1:lengthTube-1,] # Here we 
      flowtube[,1,] <- NA 
      
      # Move the extracted to the tube (i.e. copy in the tube and turn the cell in the tank to NA) 
      flowtube[c(1:heightTube),1,] <- rbind(extracted, matrix("0", heightTube-length(noNAs), dim(tank)[3]))
      tank[nrow(tank), extractedInd,] <- NA 
      
      # Increase the timestep
      t <- t + 1
      
      
      if(plotFlow == 1 & t %/% plotFlowEach %in% seq(plotFlowEach, timeSteps, plotFlowEach)){ # This plot every plotFlowEach iteration as specified
        
        print(suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 1, sizeLabelText = .1))) # Try and plot results
        
      }
      
    }
    
    if (!all(is.na(tank[1:nrow(tank)-1,,1])) & sum(!is.na(tank[nrow(tank),,1])) == 0) { # It means that there are fishes in the tank but zero element are present in the last row of the tank, we need to replace this row with the one above
      
      cat(silver("Tank matrix row", tankRow, "completed. Proceeding with tank matrix row", tankRow - 1), "\n")
      
      tankRow <- tankRow - 1 # We speicify the row of the Tank we are sampling
      
      tank <- abind(list(array(c(rep(NA, ncol(tank))), c(1,ncol(tank),5)), tank[-tankHeight,,]), along = 1,  force.array=TRUE)
      
      # Increase the timestep
      t <- t + 1
      
    }
    
    if(all(is.na(tank[1:nrow(tank)-1,,1])) & sum(!is.na(tank[nrow(tank),,1])) == 0 & any(flowtube[,,]!="0")) { # It means that zero element are present in the last row of the tank it means that we need to replace this row with the one above
      
      cat(silver("Last fishes flowing in the tube, emptying tube column", min(which(flowtube[,,] != "0", arr.ind = T)[,2]),"/", lengthTube), "\n")
      flowtube[,2:lengthTube,] <- flowtube[,1:lengthTube-1,] 
      flowtube[,1,] <- rep("0", heightTube)
      
      # Increase the timestep
      t <- t + 1
    }
    
    if(t %% lengthTube == 0 | t == timeSteps){ # if t is multiplier of length tube
      ltb <- append(ltb, list(flowtube)) # We save the tube matrix, but only when we have the tube full (every t = lengthTube multiplier)
    }
    
    if(plotFlow == 1 & t %/% plotFlowEach %in% seq(plotFlowEach, timeSteps, plotFlowEach)){ # This plot every plotFlowEach iteration as specified
      print(suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 1, sizeLabelText = .1))) # Try and plot results
    }
    
    if(saveFlow == 1){
      
      # Save species plot
      ggsave(
        filename = paste0(
          supportResultsDir,
          "/flowTankTube/pngs/image-", 
          ifelse(
            t<=10, 
            "0000000", 
            ifelse(
              t<=100, 
              "000000",
              ifelse(
                t<=1000, 
                "00000",
                ifelse(
                  t<=10000, 
                  "0000",
                  ifelse(
                    t<=100000, 
                    "000",
                    "00"
                  )
                )
              )
            )
          ),
          t-1,
          ".png"
        ),
        plot = suppressMessages(drawFlow(tank, flowtube, type = "species", multipleVars = 1, pIndicator = 1, sizeLabelText = .1)),
        width = 20, 
        height = 10,
        dpi = quality
      )
      
    }
    
    
    if(all(is.na(tank[1:nrow(tank)-1,,1])) & sum(!is.na(tank[nrow(tank),,1])) == 0 & all(flowtube[,,]=="0") & (!t %% lengthTube == 0)){
      
      cat("\n", "\n", "\n", green("Completed"), "\n", "\n", "\n")
      
    }
    
  }  

}

# Some element in the tube list still resulting NULL - delete them. 
#ltb <- ltb[-which(lapply(ltb, function(x) is.null(x)) %>% unlist)]

## P2: Save/load the resulting matrix
# The following line save the flow generated in the loop above, one element for an entire tube flown
#save(ltb, file = paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_mtx.RData"))

# The following line load the flow generated in the loop above, one element for an entire tube flown
load(paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_mtx.RData"))


########################################################################################
#
# Visualize the flow results ----
# Here we save some elements visualizing the flow created at the prior step. 
#
########################################################################################

## P1: Produce an animation of the simulation
# To produce an animation of the results it is important to 
# i.  Go to terminal and rename the single frames in subsequent order, since apparently there is still a gap in the naming  every 50 frames we use the command: 
#    
#     (once in the directory containing the frames) ls -v | cat -n | while read n f; do mv -n "$f" "$n.png"; done # Credits: https://stackoverflow.com/questions/3211595/renaming-files-in-a-folder-to-sequential-numbers
# 
# ii. Produce the video with 
#
#      (if 50gap frame is solved) ffmpeg -framerate 25 -i image-%08d.png   -c:v libx264 -pix_fmt yuv420p out.mp4  # Credit: https://stackoverflow.com/questions/24961127/how-to-create-a-video-from-images-with-ffmpeg
#      (if 50gap frame is not solved and i. was done) ffmpeg -framerate 25 -i %d.png   -c:v libx264 -pix_fmt yuv420p out.mp4  # Credit: https://stackoverflow.com/questions/24961127/how-to-create-a-video-from-images-with-ffmpeg
# 
#      Adjust framerate on needs 
#
# iii. Export video
##

## P2: Produce a representation of all the flow flown into the tube 
# First connect the matrices in the stored version of the flow 
flow <- abind(
  ltb[[14]], 
  ltb[[13]], 
  ltb[[12]], 
  ltb[[11]], 
  ltb[[10]],
  ltb[[9]], 
  ltb[[8]], 
  ltb[[7]], 
  ltb[[6]], 
  ltb[[5]], 
  ltb[[4]], 
  ltb[[3]], 
  ltb[[2]], 
  ltb[[1]],
  along = 2
)

# Then plot the result
# As a static plot in the same style as before
wholeFlow_static <- flow %>% 
  melt %>%
  filter(Var3 == 2) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    value = as.character(value)
  ) %>% 
  ggplot(
    aes(column, row, fill = value)
  ) +
  geom_tile() + 
  scale_fill_manual(
    labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
    values = c("1" = "blue", "2" = "red", "0" = "black")
  ) + 
  coord_equal() + 
  theme_bw() + 
  theme(
    axis.title = element_text(size = 8), 
    axis.text = element_text(size = 8), 
    plot.background = element_rect(fill = "white")
  )+ theme(
    legend.position = "none"
  )

# Save the plot 
ggsave(
  filename = paste0("flowTot_", "sim", simName,"_static.png"),
  plot = wholeFlow_static,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 30,
  height = 5,
  units = "cm",
  dpi = 500,
  bg = "white", 
  type = "cairo"
)

# As a static plot, full screen
wholeFlow_static_full <- flow %>% 
  melt %>%
  filter(Var3 == 2) %>% 
  dplyr::select(-Var3) %>% 
  dplyr::rename(
    column = Var2,
    row = Var1, 
    value = value
  ) %>% 
  mutate(
    value = as.character(value)
  ) %>% 
  ggplot(
    aes(column, row, fill = value)
  ) +
  geom_tile() + 
  scale_fill_manual(
    labels = c("1" = "herring", "2" = "sprat", "0" = "empty"), 
    values = c("1" = "blue", "2" = "red", "0" = "black")
  ) + 
  theme_bw() + 
  theme(
    axis.title = element_text(size = 8), 
    axis.text = element_text(size = 8), 
    plot.background = element_rect(fill = "white")
  )+ theme(
    legend.position = "none"
  )

# Save the plot 
ggsave(
  filename = paste0("flowTot_", "sim", simName,"_full_static.png"),
  plot = wholeFlow_static_full,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 30,
  height = 5,
  units = "cm",
  dpi = 500,
  bg = "white", 
  type = "cairo"
)


# As a dynamic (navigable plot)
palSpecies = data.frame(z=c(0,0.33,0.33,0.66,0.66,1),color=c("black","black","blue","blue","red","red"))
palSpecies$color <- as.character(palSpecies$color)

idD <- flow[,,1] %>% melt(value.name = "ID")
spD <- flow[,,2] %>% melt(value.name = "SP")
weD <- flow[,,3] %>% melt(value.name = "WE")
voD <- flow[,,4] %>% melt(value.name = "VO")
ifD <- cbind(idD, spD[3], weD[3], voD[3])

wholeFlow_dynamic <-plot_ly(
  data = ifD, 
  z = ifD$SP, 
  y = ifD$Var1, 
  x = ifD$Var2, 
  text = paste(
    paste0("Identifier:", ifD$ID),
    paste0("Species:", ifelse(ifD$SP==0, "Empty", ifelse(ifD$SP==1, "HER", "SPR"))),
    paste0("Weight:", ifD$WE),
    paste0("Volume:", round(as.numeric(ifD$VO),4)),
    sep = "<br>"
  ), 
  type = "heatmap",  
  colorscale=palSpecies,
  colorbar=list(
    ypad = 30, 
    tickvals=c(0,1,2), 
    ticktext=c(0,1,2), 
    width = 5000, 
    height = 100
  ),
  hovertemplate = paste(
    "<b>%{text}</b>", "<extra></extra>", sep = "<br>"
  )
) 

## Save the output
htmlwidgets::saveWidget(as_widget(wholeFlow_dynamic), selfcontained = F, paste0("results_SIMPLE/Simulation4/flowTot_", "sim", simName,"_full_dynamic.html"))

## Run into terminal to compress 
#tar -czvf ~/Public_Eros/SIMPLE/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/flowTot_sim4_full_dynamic.tar.gz ~/Public_Eros/SIMPLE/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/flowTot_sim4_full_dynamic.html
