########################################################################################
#
# sampling
# 
########################################################################################
#
# This script sample from a simulated flow of small pelagic catch flow 
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
#
########################################################################################
#
# Load data ----- 
# Here we load the flow as it was created in the SmallPelagicFlowSim.R script.
#
########################################################################################

## Load the flow generated
# This is the flow every n iteration equal to the tube length (i.e. the tube is captured as element in the ltb list every time is completely full.)
load(paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_mtx.RData"))

## Connect the flow matrices
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

## P1: Segment the flow into tonnes
# Add a dimension for labelling
# We need to convert everything to numeric
flow[,,5] <- ifelse(flow[,,5] == 0, flow[,,5], str_sub(flow[,,5], 6, 6))

# Then we two a dimensions to the array, specifying the ton interval to which they belong and the bucket they belong
flow <- array(c(
  as.numeric(flow[,,1]), # Identifier
  as.numeric(flow[,,2]), # Species
  as.numeric(flow[,,3]), # Weight 
  as.numeric(flow[,,4]), # Volume
  as.numeric(flow[,,5]), # Haul provenience
  matrix(NA, nrow(flow), ncol(flow)),
  matrix(NA, nrow(flow), ncol(flow))
), c(nrow(flow),ncol(flow),7) # Two additional dimensions, one for ton interval and the other for bucket belonged
)

## Develop the ton list
wTot = sum(flow[,,3])
tonSeq <- seq(0, wTot/1000, 1)

for(i in 1:(length(tonSeq))){
  tmp = data.frame(min = tonSeq[i], max = tonSeq[i+1], tonLabel = i)
  if(i == 1){
    tonIntervals = tmp
  } else if(i != 1 & i != length(tonSeq)) {
    tonIntervals = rbind(tonIntervals, tmp)
  } else if(i == length(tonSeq)){
    tonIntervals[length(tonSeq)-1,"max"] <- wTot
  }
}

## Develop the liters list
vTot = sum(flow[,,4])
litSeq <- seq(0, vTot, 5)
for(i in 1:(length(litSeq))){
  tmp = data.frame(min = litSeq[i], max = litSeq[i+1], litLabel = i)
  if(i == 1){
    litIntervals = tmp
  } else if(i != 1 & i != length(litSeq)) {
    litIntervals = rbind(litIntervals, tmp)
  } else if(i == length(litSeq)){
    litIntervals[length(litSeq)-1,"max"] <- vTot
  }
}


## For development purposes we recreate it only with the positive values (avoiding zeros/emptyness)
# To be deleted for the real simulation 
#flow <- flow[,1980:2980,]

## Prepare loop parameters
wCount <- 0 
lCount <- 0 
tLabel <- 1
cellCount = 0


f <- array(c(
  flow
),
c(nrow(flow),ncol(flow),7)
)  %>% 
  as.vector
f

f[100000 + (length(flow[,,1])*4)]

for(i in length(flow[,,1]):1){
  cellCount <- cellCount + 1
  cat(silver(paste0("Completed: ", round((cellCount/length(flow[,,1]))*100, 4), "%. On cell: ", cellCount, " of ", length(flow[,,1]), "\n")))
  
  if(flow[i,j,1] != 0){
    cat(silver(paste0("The cell hosts ", round(f[i], 4), " kilograms and ", round(f[i + (length(flow[,,1])*4)],4)," liters .", "\n")))
    wCount <- sum(f[i + (length(flow[,,1])*3)]) + wCount
    lCount <- sum(f[i + (length(flow[,,1])*4)]) + lCount
    
    for(intW in 1:nrow(tonIntervals)){
      #searchInterval <- between(wCount/1000, tonIntervals[intW,1], tonIntervals[intW,2])
      searchInterval <- between(wCount, tonIntervals[intW,1], tonIntervals[intW,2]) # To be deleted, just trial 
      if(searchInterval){
        tLabel <- tonIntervals[intW,"tonLabel"]
      }
    }
    
    for(intV in 1:nrow(litIntervals)){
      searchInterval <- between(lCount, litIntervals[intV,1], litIntervals[intV,2])
      if(searchInterval){
        litLabel <- litIntervals[intV,"litLabel"]
      }
    }
    
    # fill the matrix
    if(i = 1){
      tonVect <- tLabel
      litVect <- litLabel
    }else{
      tonVect <- append(tonVect, tLabel)
      litVect <- append(litVect, litLabel)
    }

    
    # Inform on completion
    cat(silver(paste0("The weight count is: ", round(wCount, 4), " kilograms", "\n")))
    cat(silver(paste0("The volume count is: ", round(lCount, 4), " liters", "\n")))
    cat(silver(paste0("The ton    count is: ", tLabel, "\n")))
    cat(silver(paste0("The bucket count is: ", litLabel, "\n \n")))
    
    
  } else{
    
    cat(silver(paste0("The cell is empty, thus skipped \n")))
    
  }
  
  
  }




append("2", c(2,3))



## Assign flow ton to each element
for(j in dim(flow)[2]:1){
  for(i in dim(flow)[1]:1){
    
    ## Count cells done 
    cellCount <- cellCount + 1
    ## Inform 
    cat(silver(paste0("Completed: ", round((cellCount/length(flow[,,1]))*100, 4), "%. On cell: ", cellCount, " of ", length(flow[,,1]), "\n")))
    
    
    if(flow[i,j,1] != 0){
      cat(silver(paste0("The cell hosts ", round(flow[i,j,3], 4), " kilograms and ", round(flow[i,j,4],4)," liters .", "\n")))
      wCount <- sum(flow[i,j,3]) + wCount
      lCount <- sum(flow[i,j,4]) + lCount
      
      for(intW in 1:nrow(tonIntervals)){
        #searchInterval <- between(wCount/1000, tonIntervals[intW,1], tonIntervals[intW,2])
        searchInterval <- between(wCount, tonIntervals[intW,1], tonIntervals[intW,2]) # To be deleted, just trial 
        if(searchInterval){
          tLabel <- tonIntervals[intW,"tonLabel"]
        }
      }
      
      for(intV in 1:nrow(litIntervals)){
        searchInterval <- between(lCount, litIntervals[intV,1], litIntervals[intV,2])
        if(searchInterval){
          litLabel <- litIntervals[intV,"litLabel"]
        }
      }
      
      # fill the matrix
      flow[i,j,6] <-tLabel
      flow[i,j,7] <-litLabel
      
      # Inform on completion
      cat(silver(paste0("The weight count is: ", round(wCount, 4), " kilograms", "\n")))
      cat(silver(paste0("The volume count is: ", round(lCount, 4), " liters", "\n")))
      cat(silver(paste0("The ton    count is: ", tLabel, "\n")))
      cat(silver(paste0("The bucket count is: ", litLabel, "\n \n")))
      
      
    } else{
      
      cat(silver(paste0("The cell is empty, thus skipped \n")))
      
    }
    
    if(j == 1 & i == 1){
      cat(green(paste0("completed with weight count equal to ", wCount, "\n")))
    }
    
  }
  
}

## Save the sampling
save(flow, file = paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_flowSoFar_iequal_",i,"_jequal_",j,".RData"))

table(flow[,,6])

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
  geom_tile() #+ 
  #theme(legend.position = "none")

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

## Extract a random number at the specified frequency 
sampleEachTon <- 25
randTon <- sample(1:sampleEachTon,1)
for(i in 1:(max(flow[,,6], na.rm = T)/sampleEachTon)){
  
  if(i == 1){
    
    tmp <- randTon
    tonVec <- tmp
    
  } else {
    
    tmp <- tmp + 25
    tonVec <- c(tonVec, tmp)
    
  }
  
}

## Extract the first bucket from each of the selected tons
buckVec <- sapply(1:length(buckVec), function(x){
  min(flow[,,7][which(flow[,,6] %in% tonVec[x], arr.ind=TRUE)])
})


## Visualise the tons selected 
f <- flow
f[,,6] <- ifelse(is.na(f[,,6]), NA, ifelse(f[,,6] %in% tonVec, 1, 0))
table(f[,,6], useNA = "al")

p <- f %>% 
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
  theme(legend.position = "none")

ggsave(
  filename = paste0("theplot.png"),
  plot = p,
  path = paste0("results_SIMPLE/Simulation", simName),
  width = 20,
  height = 20,
  units = "cm",
  dpi = 500,
  bg = "white"
)

## Visualise for the buckets selected 
f <- flow
f[,,7] <- ifelse(is.na(f[,,7]), NA, ifelse(f[,,7] %in% buckVec, 1, 0))
table(f[,,7], useNA = "al")

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
  theme(legend.position = "none")

ggsave(
  filename = paste0("theplot.png"),
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
fishesSelected <-  lapply(1:length(tonVec), function(x){
  fishesSelectedSp <-  flow[,,2][which(flow[,,7] %in% buckVec[x], arr.ind = T)]
  fishesSelectedW  <-  flow[,,3][which(flow[,,7] %in% buckVec[x], arr.ind = T)]
})
fishesSelected

## First create an empty list of samples
samplesList <- as.list(rep(NA, length(buckVec)))
names(samplesList) <- paste0("sample", 1:length(buckVec))

## Then create the list of the attributes, for those fishes that were selected 
fishesSelectedID <- sapply(1:length(buckVec),  function(x){flow[,,1][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})
fishesSelectedSP <- sapply(1:length(buckVec),  function(x){flow[,,2][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})
fishesSelectedW <- sapply(1:length(buckVec),  function(x){flow[,,3][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})
fishesSelectedV <- sapply(1:length(buckVec),  function(x){flow[,,4][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})
fishesSelectedH <- sapply(1:length(buckVec),  function(x){flow[,,5][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})
fishesSelectedT <- sapply(1:length(buckVec),  function(x){flow[,,6][which(flow[,,7] %in% buckVec[x], arr.ind = T)]})

for(i in 1:length(samplesList)){
  samplesList[[i]] = list(
    "bucket" = buckVec[i],
    "identifier" = fishesSelectedID[[i]],
    "species" = fishesSelectedSP[[i]], 
    "weight" = fishesSelectedW[[i]],
    "volume" = fishesSelectedV[[i]],
    "haul" = fishesSelectedH[[i]],
    "ton" = fishesSelectedT[[i]]
  )
}

## Transform into a df
samplesDf <- do.call(rbind, lapply(1:length(samplesList), function(x) do.call(cbind, samplesList[[x]])))
finalDf_long <- samplesDf %>% 
  as.data.frame() %>% 
  group_by(bucket, species) %>% 
  summarize(
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

finalDf_wide <- finalDf_long %>% 
  select(bucket, species, pWeight) %>% 
  pivot_wider(names_from = c(2), values_from = pWeight) #%>% 
#rename("Weight proportion of herring" = 1, "Weight proportion of sprat" = 2)


