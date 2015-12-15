probability_selection <- function(output,T)
{
  E = c()
  for(i in 1:2)
       {
        string = output[i,]
        
         
                 shedule=DecodeSeq_Single(string)
                 a=shedule$Lateness
  
                avg = sum(a)/length(string)
                g = 100-avg
                E = c(E,g)
      
       }
  del_E = E[1]-E[2] 
  del_E
  o = exp((del_E/T))
  prob = 1/(1+o)
  prob
 
}