## This function takes a string of chromosome as input. Calls the decode function which decodes the schedule.
## From the Schedule it calculates its objective function
## Since objective is minimization it returns a 1/avg value as GA by nature is maximization problem.

fitness<-function(string)
   {  
     shedule=DecodeSeq_Single(string)
     late=shedule$Lateness
     Tardiness = c()
     n=length(string)
##  loop to calculate tardiness from lateness  
     for(i in 1:n)
          {
            if (late[i]< 0) {Tj = 0}
            else  { Tj = late_vect[i] }
            Tardiness = c(Tardiness,late_vect[i])
          }
c= sum(Tardiness)/n
d = 1/c
return(d)
   }
