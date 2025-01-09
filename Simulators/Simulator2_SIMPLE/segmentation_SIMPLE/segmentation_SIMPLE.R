########################################################################################
#
# segmentation
# 
########################################################################################
#
# This script segment a simulated flow of small pelagic catch flow in tons and buckets
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

## Clean env
rm(list=setdiff(ls(), c("supportResultsDir", "simName", "p_herring"))); gc()

## Load the flow generated
load(paste0("~/mnt/CNAS/SIMPLE_Auxiliary/flowTankTube/matrixes", "/Simulation", simName, "/flow.R"))

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


## Drop the empty regions (save time in the following steps)
# Here we extract where the emptyness ends and begin in the flow. 
colIndFull <- which(flow[,,1] == 0, arr.ind = T)[,2] %>% unique() %>% sort() %>% data.frame() %>% rename(colInd = 1) %>%  mutate(gap = abs(lag(colInd) - colInd)) %>% filter(gap > 1) %>% dplyr::select(colInd) %>% pull() %>% range
colIndFullMin <- min(colIndFull)
colIndFullMax <- max(colIndFull)

# Now filter 
if(sum(as.numeric(flow[,(colIndFullMin-100):colIndFullMax,3]), na.rm = T) == sum(as.numeric(flow[,,3]))){ # The -100 is a buffer to make sure no fish is lost
  flow <- flow[,(colIndFullMin-100):colIndFullMax,]
} else{
  stop("Check empty part removal")
}

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

## Assign flow ton and bucket to each element
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
        searchInterval <- between(wCount/1000, tonIntervals[intW,1], tonIntervals[intW,2])
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

## Save the segmentation
save(flow, file = paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_flowSoFar_iequal_",i,"_jequal_",j,".RData"))
save(flow, file = paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_flowBucketTonsAssigned.RData")
