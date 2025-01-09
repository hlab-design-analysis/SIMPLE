## Load the flow
load(paste0(supportResultsDir, "/flowTankTube/matrixes/Simulation", simName, "/Sim_", simName, "_mtx.RData"))

## Connect the flow matrices
flow <- abind(
  ltb[[4]], 
  ltb[[3]],
  along = 2
)

## Draw the tube
drawTube(flow, plot = 1, type = "species", legend = 1)

## Determine the flow to compress
# Find the column not completely filled 
colToCompress <- NULL
for(i in 1:ncol(flow)){
  if(length(which(flow[,i,1] != 0)) < 1000 & length(which(flow[,i,1] != 0)) != 0){
    if(i == 1){
      colToCompress = i
    } else {
      colToCompress = append(colToCompress, i)
    }
  }
}

# Include middle column possibly to be filled
colToCompress <- min(colToCompress):max(colToCompress)

# Filter the flow for the portion to compress
flowToCompress <- flow[,colToCompress,]

# Draw the flow to compress
drawTube(flowToCompress, plot = 1, type = "species", legend = 1)

# Proceed with the compression
# Sigmoidal just remove the emptyness between the fishes, more conservative on the order of the fishes. 
if(lateralCompression == 1 & typeLateralCompression == "sigmoidal"){

  # Move the cell to the right if the right neighbourhood is empty 
  for(dm in 1:dim(flowToCompress)[3]){
    
    for(ro in 1:nrow(flowToCompress)){
      
      flowToCompress[,,2] <- ifelse(flowToCompress[,,2] == 0, NA, flowToCompress[,,2])
      recFullRow <- flowToCompress[ro,!is.na(flowToCompress[ro,,2]),2]
      nrecFullRow <- length(recFullRow)
      flowToCompress[ro,,2] <- c(rep(NA, ncol(flowToCompress)-nrecFullRow), recFullRow)
      flowToCompress[,,2] <- ifelse(is.na(flowToCompress[,,2]), 0, flowToCompress[,,2])
      
      cat(silver(paste0("Compressing last fraction of the flow: part ", dm, " of ", dim(flowToCompress)[3], " advanced ", (ro/nrow(flowToCompress))*100, "%", "\n")))
      
      if(ro == nrow(flowToCompress) & dm == dim(flowToCompress)[3]){
        cat("\n", "\n", "\n", green("Completed"), "\n", "\n", "\n")
      }
      
    }
    
  }

}

# Rectangular create a sharp compression. 
if(lateralCompression == 1 & typeLateralCompression == "sigmoidal"){

  a <- flowToCompress 
  a <- ifelse(a == 0, NA, a)
  a_id <- a[,,1][!is.na(a[,,1])]
  a_sp <- a[,,2][!is.na(a[,,2])]
  a_we <- a[,,3][!is.na(a[,,3])]
  a_vo <- a[,,4][!is.na(a[,,4])]
  a_ha <- a[,,5][!is.na(a[,,5])]
  
  flowToCompress <- array(c(
    c(rep(NA, (nrow(a[,,1])*ncol(a[,,1]))-length(a_id)), a_id), # Identifier
    c(rep(NA, (nrow(a[,,1])*ncol(a[,,1]))-length(a_id)), a_sp), # Species
    c(rep(NA, (nrow(a[,,1])*ncol(a[,,1]))-length(a_id)), a_we), # Weight 
    c(rep(NA, (nrow(a[,,1])*ncol(a[,,1]))-length(a_id)), a_vo), # Volume
    c(rep(NA, (nrow(a[,,1])*ncol(a[,,1]))-length(a_id)), a_ha) # Haul provenience
  ), 
  c(
    nrow(a[,,1]), 
    ncol(a[,,1]), 
    5
  ) 
  )
  
  flowToCompress <- ifelse(is.na(flowToCompress), 0, flowToCompress)
  
}


# Check compression
drawTube(flowToCompress, plot = 1, type = "species", legend = 1)

# Replace
flow[,min(colToCompress):max(colToCompress),] <- flowToCompress

# Check the results
drawTube(flow, plot = 1, type = "species", legend = 1)
