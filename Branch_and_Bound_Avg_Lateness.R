# Author : Ashutosh Kumar Singh
# Date   : 20-11-2015
# NIT Jamshedpur


## Enter the data about various operations
Branch_n_Bound_Avg_Lateness <- function(data)
{
  ## Calling library Combinat for performing permutation
  library(combinat)
  ##  permn creates all possible sequences
  Sequences=permn(data$Job)
  
  
  AvgLateness = c()
  leng =length(Sequences)
  
  
  ##  looping for number of schedules generated
  for (r in 1:leng)
  {
    ##   Decoding of single schedule to find lateness (objective function)
    schedule=DecodeSeq_Single(Sequences[[r]])
    
    a = mean(schedule$Lateness)
    ##   Creates a vector carrying average lateness values in all schedules
    AvgLateness = c(AvgLateness,a)
  }
  
  ##   Finds which shedule has the minimum maximum lateness
  g = which.min(AvgLateness)
  optimum = Sequences[[g]]
  ##   Creates the optimum schedule
  BestShedule = DecodeSeq_Single(optimum)
  BestShedule
}