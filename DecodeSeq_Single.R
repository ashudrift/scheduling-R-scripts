##  Takes input as a particular chromosome and decodes it to form a schedule

DecodeSeq_Single <- function (string)
{
## Creates empty vectors for storing values
  NewProcessTime = c()
  MakeSpan = 0
  StartingTime = c()
  FinishingTime = c()
  D_j = c()
  n= length(string)
  
##  Looping to find the chromosome individual component's data values from data.
  
  for(j in 1:n)
     {
        logic = data$Job == string[j]
        k = length(string)
          for(i in 1:k)
            {
            
## calculation of different parameters/objective function            
            
               if( logic[i]==TRUE)
                  { NewProcessTime=c(NewProcessTime,data$ProcessTime[i])
                    StartingTime = c(StartingTime,MakeSpan)
                    MakeSpan = MakeSpan + data$ProcessTime[i]
                    FinishingTime = c(FinishingTime, MakeSpan)
                    D_j = c(D_j,data$Dj[i])
                    Lateness =  FinishingTime - D_j
                  }    
            }
      }
   JobNo= string
   Shedule = data.frame(JobNo,NewProcessTime,StartingTime,FinishingTime,Lateness,D_j)
 
   
}
