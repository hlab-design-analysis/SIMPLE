########################################################################################
#
# makeHaulsList
# 
########################################################################################
#
# A function to create a list specifying the haul name and its features (catch, 
# proportion of herring and proportion of sprat).  
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
# 2024-03-13: Created 
# 
########################################################################################

## P1: Create a function to create a list specifying the haul name and its features (catch, proportion of herring and proportion of sprat).  
makeHaulsList <- function(
  nHaul, # Number of hauls
  p_herring, # Proportion of herring. If nHaul > 1, use vector with one value per each
  N # Catch of both species If nHaul > 1, use vector with one value per each
  ){
  if(nHaul == 1){
    
    # Create an empty list
    listHaul = list(
      rep(NA, nHaul)
    ) 
    
    # Create parameter list
    listHaul[[1]] = list(
      "catch_n" = N,
      "p_herring" = p_herring ,
      "p_sprat" = 1 - p_herring
    )
    
    # Assign names
    names(listHaul) <- "Haul_1"
    
  }else{
    
    # Create an empty list
    listHaul = split(rep(NA, nHaul), 1:nHaul)
    
    # Create parameter list
    for(i in 1:length(listHaul)){
      listHaul[[i]] = list(
        "catch_n" = N[i],
        "p_herring" = p_herring[i],
        "p_sprat" = 1 - p_herring[i]
      )
    }
    
    # Assign names
    names(listHaul) <- paste0("Haul_", 1:length(listHaul))
    
  }
  
  listHaul
  
}
