fitness<-function(string)
   {shedule=DecodeSeq_Single(string)
   a=which.max(shedule$Lateness)
   b = shedule$Lateness[a]
   c=1/b
   return(c)}
