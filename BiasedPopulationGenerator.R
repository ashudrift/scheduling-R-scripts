## Creates a biased population set
CreateBiasedPopulation <- function(Jobs,Population)
  
{
  
##  matrix to filled by parents  
  parents = matrix(nrow=Population,ncol=Jobs,byrow=TRUE)
  parent_chrom = c()
  ProcessTime = data$ProcessTime
##   Finding the SPT Job
  SPT = which.min(ProcessTime)
  
  pre_sol = 1:Jobs
  sol_job = Jobs - 1
  sol = c()
  
  for(l in 1:Jobs) 
     {
## The job  which don't correspond to SPT are ignored    
        if(l == SPT  )  {}
        else sol = c(sol,pre_sol[l])
## The  job not being spt is stored
            
  }
  
##  loop for number of parents required
  for(k in 1:Population)
  {
    Jobs = nrow(data)
    chrom = c()
 
## simulation by generating random   sequence of normal distribution    
    sim = rnorm(sol_job,1,1)
    sort_sim = sort(sim)

## to create a list of jobs as sorted    
    for(j in 1:sol_job)
    {
      logic = sim == sort_sim[j]
      for(i in 1:sol_job)
      {
        if( logic[i]==TRUE)
        {chrom=c(chrom,sol[i])}
        
      }
    
    }
## Saves a individual parent with SPT as its first element
    parent_chrom = c(parent_chrom,SPT,chrom)
  }
## creates a matrix of parents 
  parents = matrix(parent_chrom,nrow=Population,ncol=Jobs,byrow=TRUE)
  parents
}
