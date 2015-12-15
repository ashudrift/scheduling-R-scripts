mutation <- function(data,suggest)
{
  fitness <- function(string) 
  {
    0.5
  }
  GA=ga(type="permutation",fitness=fitness,min=1,max = length(data$Job),selection =gaperm_rwSelection,popSize = 2,pcrossover = 0,pmutation = 1,elitism = 1,maxiter=1,suggestions = suggest)
  output = GA@solution
  output
}