# Required packages
require(combinat)

isConnected <- function(thisM,N) {
  
  for (i in 1:(N-1)) {      
    thisM <- thisM %*% thisM
  }
  
  return(sum(thisM == 0) == 0) # Simple test for connectedness
  
}

makeAvGift <- function(thisM) {
  
  N <- dim(thisM)[1]
  
  allPerm <- permn(1:N)
  
  P <- length(allPerm)
  
  allGiftNum <- rep(NA,P)
  
  for (i in 1:P) {
    thisPerm <- allPerm[[i]]
    nPres    <- rep(0,N)
    for (j in 1:N) {
      thisConnection      <- thisM[j,]*thisPerm
      matchPresent        <- which(thisConnection==max(thisConnection))
      nPres[matchPresent] <- nPres[matchPresent]+1
    }      
    allGiftNum[i] = sum(nPres != 0)
  }
  
  return(mean(allGiftNum/N))
  
}

makeAnalysis <- function(N) {
  
  M <- matrix(data = rep(0,N^2),N,N)
  
  for (i in 1:N) {
    M[i,i] <- 1
  }
  
  botLeft            <- which(M==0,arr.ind = TRUE)
  valid              <- botLeft[,"row"] > botLeft[,"col"]
  botLeft            <- botLeft[valid,]
  topRight           <- botLeft[,c(2,1)]
  colnames(topRight) <- c("row","col")
  
  P <- sum(seq(1,N-1)) # Number of unique off-diagonals
  
  numPerm <- 2^P  # Number of permutations for off-diagonals
  
  listM  <- list()
  avGift <- c()
  
  i2bLength <- length(intToBits(0))
  
  counter <- 1
  
  for (i in 1:numPerm) {
    thisM    <- M
    thisPerm <- rev(intToBits(i-1))
    thisPerm <- as.integer(thisPerm[(i2bLength-P+1):i2bLength])
    
    thisM[botLeft]  <- thisPerm
    thisM[topRight] <- thisPerm
    
    if (isConnected(thisM,N)) {
      listM[[counter]] <- thisM # Adjaceny matrix    
      avGift[counter]  <- makeAvGift(thisM)
      counter          <- counter + 1
    }
  }
  
  return(list(listM=listM,avGift=avGift))
  
}

threshold <- 9/16

out <- makeAnalysis(3)
if(any(out$avGift>=threshold)) {
  print("Solution found for N = 3") # No solution found
}

out <- makeAnalysis(4)
if(any(out$avGift>=threshold)) {
  print("Solution found for N = 4") # Several solutions found
}


