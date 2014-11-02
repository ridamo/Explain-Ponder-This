# 201311
# http://domino.research.ibm.com/Comm/wwwr_ponder.nsf/Challenges/November2013.html

### --------------------------- ### 
# A three-dimensional cube has eight vertices, twelve edges, and six faces. 
# Let's call them 0-D, 1-D, and 2-D faces, respectively.
# Denote f(d,k) as the number of k-dimensional faces of an d-dimensional hyper
# cube, so f(3,1)=12.
# Find three cubes (with different dimensions d1, d2, and d3) such that the 
# number of k1, k2, and k3 dimension faces are the same, i.e. 
# f(d1,k1) = f(d2,k2) = f(d3,k3).
# We are looking for nontrivial solutions, so k1 should be less then d1.
# Bonus: Find more than three.
### --------------------------- ###

# See Wikipedia page on hypercubes: 
# http://en.wikipedia.org/wiki/Hypercube

# From this, we can work out that the number of k-dimension faces (f) for a 
# d-dimension hypercube is:
# f(d,k) = 2^(d-k) * d Choose k  = 2 ^(d-k) * d!/(k!*(d-k)!)


makeFaces <- function(d) {
  
  output <- list()
  
  for (k in 0:(d-1)) {
    output[[k+1]]        <- 2^(d-k)*choose(d,k)   
    names(output[[k+1]]) <- paste0(d,"_",k)
  }
  
  return(output)
}

appendFaces <- function(facesList,faces) {
  
  for (i in 1:length(faces)) {
    thisFaces <- faces[[i]]
    match     <- which(names(facesList)==thisFaces)
    l         <- length(facesList)
    if (length(match)==0) {
      facesList[[l+1]] <- names(thisFaces)
      names(facesList) <- c(names(facesList)[1:l], as.character(thisFaces))
    } else {
      facesList[[match]] <- c(facesList[[match]],names(thisFaces)) 
    }
  }
  
  return(facesList)
}

findSolution <- function(facesList) {
  
  validSolution <- 0
  counter       <- 1
  N             <- 3
  
  while (!validSolution && counter<length(facesList)) {
    if (length(facesList[[counter]])>=N) {
      validSolution <- counter
    } else {
      counter <- counter + 1
    }
  }
  
  return(validSolution)
}

validSolution = 0

facesList <- list()
d         <- 1

while(!validSolution) {
  
  faces <- makeFaces(d)
  
  facesList <- appendFaces(facesList,faces)
  
  validSolution <- findSolution(facesList)
  
  d <- d+1
}

solution <- facesList[validSolution]

print(paste0("Valid Solution: " , names(solution)))
print(paste0("Valid Solution Dimension_Faces: " , paste0(solution[[1]],collapse = ", ")))