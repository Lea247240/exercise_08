rm(list=ls())

#Cesta:
setwd('V:/MPA_PRG/exercise_08')
setwd('D:/VUT/4-5rocnik/moje/MPA-PRG/exercise_08') # home

# Synteny Blocks

## The Breakpoint Sort
#Implement the algorithm for the Breakpoint Sort that consists of three functions. First function `FindSorted()` will
#find an index where the unsorted part of the permutation starts e.i. it will mark the position of the first breakpoint. 
#The second function `IndicateAscending()` will create a vector of the same length as the permutation and mark ascending 
#parts by ones and descending parts by zeros. Finally, the last function `BreakpointSort()` will perform sorting by 
#reversals.

### Task 1
# In R, create a function `FindSorted()` to find an index, at which the unsorted part starts.

# Input:
  # A vector (permutation) of integers e.g. `0 1 2 3 6 7 4 5 8`.

# Output:
  # An index, at which the unsorted part starts e.g. `5`.

#> **Hint**: 
#> Compare successively values of the permutation with an increasing number starting at zero (`0`, `1`, `2` ...) 
#> and ending at length of the permutation - 1. The comparison ends when the value in permutation is not the same as 
#> the tested value or when the tested value is equal to the length of the permutation - 1.

permutation <- c(0,1,2,3,6,7,4,5,8,9)
start <- permutation[1]
end<- permutation[length(permutation)-1]

for (i in (1:(length(permutation)-1))){
  
  #print(permutation[i] + 1) # predchozi +1
  #print(permutation[i+1]) # nasledna
  
  if ((permutation[i] + 1) != permutation[i+1]){
    return(i+1)
  }
}

# .......... all function..................................
FindSorted <- function(permutation){
  start <- permutation[1]
  end<- permutation[length(permutation)-1]
  for (i in (1:end)){
    
    #print(permutation[i] + 1) # predchozi +1
    #print(permutation[i+1]) # nasledna
    
    if ((permutation[i] + 1) != permutation[i+1]){
      return(i+1)
    }
  }
}

FindSorted(permutation <- c(0,1,2,3,6,7,4,5,8,9))

#***************************************************************************************
### Task 2
# In R, create a function `IndicateAscending()` to mark ascending and descending parts of a permutation.

# Input:
  # A vector (permutation) of integers e.g. `0 4 5 3 2 1 6 7 8`.

# Output:
  # A vector of zeros and ones, where ascending parts are marked by `1` and descending by `0` e.g. `1 1 1 0 0 0 1 1 1`.

#> **Hint:**
#> Create an indication vector of the same length as the permutation containing only `0` values, and then set the first
#> and last values to `1`. The ascending parts of the permutation vector will be marked with `1` values in the indication
#> vector. Create a loop that iterates through the permutation and if two values next to each other are ascending, 
#> i.e. the second is the first + 1, then the indication vector is set to `1` at the given indexes.

vector <- numeric(length(permutation))
permutation <- c(0,4,5,3,2,1,6,7,8)

# always
vector[1] <- 1
vector[length(permutation)]<-1

for (i in (1:(length(permutation)-1))){
  if (permutation[i] < permutation[i+1]){ #there is 1 in vector
    print(i + 1)
    vector[i+1] <- 1
  }
}

#........................all function...................................
IndicateAscending <- function(permutation){
  
  vector <- numeric(length(permutation)) #vector with all 0
  
  # always
  vector[1] <- 1
  vector[length(permutation)]<-1
  
  for (i in (1:(length(permutation)-1))){
    if (permutation[i] < permutation[i+1]){ #there is 1 in vector ascendent
      #print(i + 1)
      vector[i+1] <- 1
    }
  }
  return(vector)
}

IndicateAscending(permutation <- c(0,4,5,3,2,1,6,7,8))

#*****************************************************************************************************

### Task 3
# In R, create a function `BreakpointSort()` to sort a permutation using Breakpoint Sort.

# Input:
  # A vector (permutation) of integers e.g. `5 1 4 3 7 8 9 2 6`.

# Output:
  # Sorted vector (permutation) of integers e.g. `1 2 3 4 5 6 7 8 9`.

#> **Hint:** Add marginal values to the permutation and the following steps are repeated in a loop:
#> * find the start of the unsorted region,
#> * mark ascending/descending parts,
#> * find the smallest value that is marked as descending part,
#> * reversal between the start of the unsorted region and the smallest value marked as descending part.
#>
#> The loop ends when the permutation is sorted. Watch out for collision situations i.e. no parts marked as descending 
#> or there is a single value marked as descending in front of the sorted part of the permutation.

permutation <- c(5,1,4,3,7,8,9,2,6)

start <- FindSorted(permutation)
mark <- IndicateAscending(permutation)
idx_0 <-c()

for (i in (1:(length(permutation)-1))){
  if (mark[i] == 0){
    #print(i)
    #print(permutation[i])
    idx_0 <- c(idx_0,i) # index of 0 in mark
  } 
}
number <- permutation[idx_0]

small <- min(number)

our_idx <- match(small,permutation) # get index of smallest value

#change the numbers
permutation[start] <-  permutation[our_idx] 
permutation[our_idx]  <- permutation[start]
permutation

BreakpointSort <- function(permutation){
  
  while (TRUE) {
    start <- FindSorted(permutation)
    mark <- IndicateAscending(permutation)
    idx_0 <-c()
    
    for (i in (1:(length(permutation)-1))){
      if (mark[i] == 0){
        #print(i)
        #print(permutation[i])
        idx_0 <- c(idx_0,i) # index of 0 in mark
      } 
    }
    number <- permutation[idx_0]
    
    small <- min(number)
    
    our_idx <- match(small,permutation) # get index of smallest value
    
    #change the numbers
    permutation[start] <-  permutation[our_idx] 
    permutation[our_idx]  <- permutation[start]
    permutation
    
    if (all(mark != 0)){
      return(permutation)
    }
  }
}

BreakpointSort(permutation <- c(5,1,4,3,7,8,9,2,6))


