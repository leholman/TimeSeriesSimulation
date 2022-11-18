### A simulation of changes down a sediment core 

###samples across the record

length.of.record <- 100


###how does the environmental variable change across time?

#type of change (does the environemnt change linearly or in a different pattern - two coded right now)
type <- "linear"
type <- "sigmoid"

#variable to pass to function to change shape of change (0-1)
shape <- 0.5

#variable to determine magnitude of variable (any value)
magnitude <- 100

#a magic sprinkle of random (0->1)
random <- 0.05


#This function returns a vector of size N representing an environmental variable that 
#varies across the space according to type, shape, magnitude and randomness  
SimulateEnv <- function(length.of.record,type,shape,magnitude,random){
  if(type=="linear"){
    envOut <- 1:length.of.record*shape+magnitude
    envOut <- envOut + rnorm(length.of.record,0,random*magnitude)
    return(envOut)
  }else if(type=="sigmoid"){
    envOut <- tanh(seq(-(shape*3),(shape*3),length.out=length.of.record))*magnitude
    envOut <- envOut + rnorm(length.of.record,0,random*max(envOut))
    envOut <- envOut-min(envOut)
    return(envOut)
  }else{stop("Variable type not defined")}
}

###Species

##How much does the environmental variable affect the species count?
strength.of.effect <- 0.8

#How big are the counts of the species across time?
rarity.of.species <- 0.5

#How much does the species randomly disappear across the time series?  
stochastic.of.species <- 0.1


## This function creates a timeseries vector that 
SimulateSpp <- function(length.of.record,strength.of.effect,rarity.of.species,stochastic.of.species,environmental.data){
  #make a little dataframe to catch the result
  output <- rep(0,length.of.record)
  #some initial data from a poisson distribution using the rarity of species parameter as input
  output <- output + rpois(length.of.record,rarity.of.species*100)
  #add in the effect of the env variable
  output <- (output + sum(output)*(environmental.data/sum(environmental.data)*strength.of.effect))/2
  #now we add some random drop outs
  output[rbinom(length.of.record,1,stochastic.of.species)==1] <- 0
  return(output)
  }

envData <- SimulateEnv(length.of.record=50,
                       type="sigmoid",
                       shape=1,
                       magnitude=100,
                       random=0.05)

sppData <- SimulateSpp(length.of.record=50,
                       strength.of.effect=0.8,
                       rarity.of.species=0.5,
                       stochastic.of.species=0.01,
                       environmental.data=envData) 


plot(sppData)







