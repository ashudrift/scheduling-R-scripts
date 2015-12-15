## Simulated Annealing Function using GA package
 SimulatedAnnealing <- function(data,T)
 {
   Jobs = length(data$Job)
   suggest = CreateBiasedPopulation(Jobs,1)
   schedule2 = DecodeSeq_Single(suggest) 
   best_obj = mean(schedule2$Lateness)
   best_seq  = suggest
   data = data
   optimization_obj = c()
   iterations = c()
   g = T
   for(gen in  1:g)
   {
   output = mutation(data,suggest)
   prob = probability_selection(output,T)
   r = rnorm(1,0.5,0.1666667)
   if(r <- prob)
   {suggest = output[2,]}
   else
    { suggest = output[1,]}
   T = T-1
    schedule = DecodeSeq_Single(suggest) 
    obj = mean(schedule$Lateness)
    
    if (obj<best_obj)  {best_seq = suggest}
   }
   best_seq
 }