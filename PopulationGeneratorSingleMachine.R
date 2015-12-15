## Creates a random population set
CreatePopulation <- function(Jobs,Population)

{
##  matrix to filled by parents
parents = matrix(nrow=Jobs,ncol=Population,byrow=TRUE)
parent_chrom = c()
##  loop for number of parents required
  for(k in 1:Population)
    {
      Jobs = nrow(data)
      chrom = c()

## simulation by generating random   sequence of normal distribution 
      sim = rnorm(Jobs,1,1)
      sort_sim = sort(sim)

## to create a list of jobs as sorted
        for(j in 1:Jobs)
          {
            logic = sim == sort_sim[j]
               for(i in 1:Jobs)
                {
                   if( logic[i]==TRUE)
                   {chrom=c(chrom,i)}
                 }
        }
## Saves a individual parent
           parent_chrom = c(parent_chrom,chrom)
         }
## creates a matrix of parents 
     parents = matrix(parent_chrom,nrow=Jobs,ncol=Population,byrow=TRUE)
     parents
}
