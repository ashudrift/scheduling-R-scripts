## Modified Branch and Bound Technique for minimizing average lateness using SPT

modified_Branch_and_Bound <- function(data)
{

## Loading package for permutation operations
    library(combinat)
##  permn function to create random schedules
    Sequences=permn(data$Job)
##  creating empty vectors for storing values
    mod_Seq = list()
    AvgLateness = c()
    leng =length(Sequences)
##  Finding the minimum Process Time of operations
    y = which.min(data$ProcessTime)
    min = data$Job[y]
    i = 1
    
    n = length(data$Job)
##  For loop to population of schedules to reduce search space
    for (r in 1 : leng)
    {
      seq = Sequences[[r]]
      if ( seq[[1]] == min  )
           {
              mod_Seq[[i]] = seq
              i = i + 1
           } 
    }
    mod_leng = length(mod_Seq)
    
    
##  For loop to population of modified search space to find best schedule
    for (r in 1:mod_leng)
    {
      schedule=DecodeSeq_Single(mod_Seq[[r]])
      
      a = mean(schedule$Lateness)
      AvgLateness = c(AvgLateness,a)
    }
    g = which.min(AvgLateness)
    optimum = mod_Seq[[g]]
    BestShedule = DecodeSeq_Single(optimum)
    BestShedule
   
}