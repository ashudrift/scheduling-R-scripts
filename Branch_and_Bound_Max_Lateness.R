# Author : Ashutosh Kumar Singh
# Date   : 20-11-2015
# NIT Jamshedpur


## Enter the data about various operations
Branch_n_Bound <- function(data)
{
## Calling library Combinat for performing permutation
   library(combinat)
##  permn creates all possible sequences
  Sequences=permn(data$Job)
  
  Lateness = c()
  MaxLateness = c()
  leng =length(Sequences)
  
  
##  looping for number of schedules generated
  for (r in 1:leng)
     {
##   Decoding of single schedule to find lateness (objective function)
           shedule=DecodeSeq_Single(Sequences[[r]])
           Lateness = c(Lateness,shedule$Lateness)
           a = which.max(Lateness)
##   Creates a vector carrying maximum lateness values in all schedules
           MaxLateness = c(MaxLateness,Lateness[a])
  }
  
##   Finds which shedule has the minimum maximum lateness
  g = which.min(MaxLateness)
  optimum = Sequences[[g]]
##   Creates the optimum schedule
  BestShedule = DecodeSeq_Single(optimum)
  BestShedule
}