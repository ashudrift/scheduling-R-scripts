## This function takes a string of chromosome as input. Calls the decode function which decodes the schedule.
## From the Schedule it calculates its objective function
## Since objective is minimization it returns a 1/avg value as GA by nature is maximization problem.

fitness<-function(string)
{shedule=DecodeSeq_Single(string)
a=shedule$Lateness
avg = sum(a)/length(string)
g = 100-avg
return(g)}
