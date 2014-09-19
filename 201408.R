# 201408
# http://domino.research.ibm.com/Comm/wwwr_ponder.nsf/Challenges/August2014.html

### --------------------------- ### 
# We are trying to solve an optimization problem, 
# Let's assume that correct answer is defined as an integer number between 51 to 
# 150 cm (inclusive). You can use a black box solver to answer the question: "is
# the correct answer at least X?" If the answer is positive - it costs you one
# cent, but if not - it costs you 10 cents. Assuming that the answer is 
# uniformly distributed (i.e., every number has the same probability) and using
# the most efficient strategy, how much will it cost, on average, to solve the 
# problem?
### --------------------------- ###

# 1. Write a function to partition the problem into two sections, determined by 
# frac. Later we'll sweep frac to determine the optimal solution. 

makeCost <- function(n=100,
                     frac=0.5,
                     costWrong=10,
                     costRight=1) {
  
  i = min(max(floor(n*frac),1),n-1)
  
  if (n==1) {
    return(0)
  } else {
    
    totalCost <- (i/n)*(makeCost(i  ,frac,costWrong,costRight)+costWrong) + # Prob-weighted cost of guessing wrong 
             ((n-i)/n)*(makeCost(n-i,frac,costWrong,costRight)+costRight)   # Prob-weighted cost of guessing right
    return(totalCost)
  }
  
}

possFracs <- seq(0.01,0.99,0.001) #Sweep this range
N         <- length(possFracs)
result    <- rep(NA,N)

for (i in 1:N) {
  result[i] <- makeCost(100,possFracs[i])
}

#plot(possFracs,result,type = "l")

minCost  <- min(result)
minIndex <- which(result==minCost)

print(paste0("*** Minimum Cost to Solve = ", minCost, " Cents ***"))
#print(paste0("*** Optimal Frac = (any of) ", paste(possFracs[minIndex],collapse = ", "),"***"))