# Bayesian comparison Of Properties of Two tests
# Anupam Singh
# 13/04/2015

# Primary outcome: composite of 30-day moderate or major complications and mortality
N1 <- 200
y1 <- 140
N2 <- 300
y2 <- 180
# N1 is total number of  people with Disease in population
# y1 is number True positives diagnosed by testA 
# N2 is total number of people  in population without Disease
# y2 is number of True negatives diagnosed by testA
# Sensitivity of TestA = y1/N1
# Specificity of TestA = y2/N2
# Consider a different TestB in same population with total number of true positives and true negatives as y3 and y4 respectively
# Sensitivity of TestB = y3/N1
# Specificity of TestB = y4/N2
y3 <- 190
y4 <- 210


# Bayesian Analysis Of Same Data

#------------------------------------------------------------------------------
source("~/DBDA2E-utilities.R")
source("~/plotPost.R")

require(rjags) # Kruschke, J. K. (2011). Doing Bayesian Data Analysis, Academic Press / Elsevier.
#------------------------------------------------------------------------------
# Important
# The model will be specified with completely uninformative prior distributions (beta(1,1,).
# This presupposes that no pre-exisiting knowledge exists as to whehther a difference
# may of may not exist between these two intervention. 

# Plot Beta(1,1)
# 3x1 plots
par(mfrow=c(3,1))
# Adjust size of prior plot
par(mar=c(5.1,7,4.1,7))
plot(seq(0, 1, length.out=100), dbeta(seq(0, 1, length.out=100), 1, 1), 
     type="l", xlab="Proportion",
     ylab="Probability", 
     main=" DiagnosticParameter\nPrior distribution", 
     frame=FALSE, col="red", oma=c(6,6,6,6))
legend("topright", legend="beta(1,1)", lty=1, col="red", inset=0.05)

# THE MODEL.
modelString = "
# JAGS model specification begins here...
model {
# Likelihood. Each test result is is Bernoulli. 
for ( i in 1 : N1 ) { y1[i] ~ dbern( theta1 ) }
for ( i in 1 : N2 ) { y2[i] ~ dbern( theta2 ) }
for ( i in 1 : N1 ) { y3[i] ~ dbern( theta3 ) }
for ( i in 1 : N2 ) { y4[i] ~ dbern( theta4 ) }
# Prior. Independent beta distributions.
theta1 ~ dbeta( 1 , 1 )
theta2 ~ dbeta( 1 , 1 )
theta3 ~ dbeta( 1 , 1 )
theta4 ~ dbeta( 1 , 1 )
}
# ... end JAGS model specification
" # close quote for modelstring

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")


#------------------------------------------------------------------------------
# THE DATA.

# Specify the data in a form that is compatible with JAGS model, as a list:
dataList =  list(
  N1 = N1 ,
  y1 = c(rep(1, y1), rep(0, N1-y1)),
  y3 = c(rep(1, y3), rep(0, N1-y3)),
  N2 = N2 ,
  y2 = c(rep(1, y2), rep(0, N2-y2)),
  y4 = c(rep(1, y4), rep(0, N2-y4))
  
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

# Can be done automatically in jags.model() by commenting out inits argument.
# Otherwise could be established as:
# initsList = list( theta1 = sum(dataList$y1)/length(dataList$y1) , 
#                   theta2 = sum(dataList$y2)/length(dataList$y2) ,
                    theta3 = sum(dataList$y3)/length(dataList$y3) ,
                    theta4 = sum(dataList$y3)/length(dataList$y4))

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "theta1" , "theta2" , "theta3" , "theta4")     # The parameter(s) to be monitored.
adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 1000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=200000           # Total number of steps in chains to save.
thinSteps=1                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "model.txt" , data=dataList , # inits=initsList , 
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )
# resulting codaSamples object has these indices: 
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]

#------------------------------------------------------------------------------
# EXAMINE THE RESULTS.

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )

theta1Sample = mcmcChain[,"theta1"] # Put sampled values in a vector.
theta2Sample = mcmcChain[,"theta2"] # Put sampled values in a vector.
theta3Sample = mcmcChain[,"theta3"] # Put sampled values in a vector.
theta4Sample = mcmcChain[,"theta4"] # Put sampled values in a vector.


# Plot the chains (trajectory of the last 500 sampled values).
par( pty="s" )
chainlength=NROW(mcmcChain)
plot( theta1Sample[(chainlength-500):chainlength] ,
      theta2Sample[(chainlength-500):chainlength] ,theta3Sample[(chainlength-500):chainlength] , theta4Sample[(chainlength-500):chainlength] ,type = "o" ,
      xlim = c(0,1) , xlab = bquote(theta[1]) , ylim = c(0,1) ,
      ylab = bquote(theta[2]) , main="JAGS Result" , col="skyblue" )

# Display means in plot.
theta1mean = mean(theta1Sample)
theta2mean = mean(theta2Sample)
theta3mean = mean(theta1Sample)
theta4mean = mean(theta4Sample)
if (theta1mean > .5) { xpos = 0.0 ; xadj = 0.0
} else { xpos = 1.0 ; xadj = 1.0 }
if (theta2mean > .5) { ypos = 0.0 ; yadj = 0.0
} else { ypos = 1.0 ; yadj = 1.0 }
if (theta3mean > .5) { ypos = 0.0 ; yadj = 0.0
} else { ypos = 1.0 ; yadj = 1.0 }
if (theta4mean > .5) { ypos = 0.0 ; yadj = 0.0
} else { ypos = 1.0 ; yadj = 1.0 }
text( xpos , ypos ,
      bquote(
        "M=" * .(signif(theta1mean,3)) * "," * .(signif(theta2mean,3))
      ) ,adj=c(xadj,yadj) ,cex=1.5  )

# Plot a histogram of the posterior differences of theta values.
SensitivityA = theta1Sample  # SensitivityA
SpecificityA = theta2Sample # SpecificityA
PositiveLikelihoodRatioA = theta1Sample/ (1-theta2Sample)
NegativeLikelihoodRatioA = (1-theta1Sample)/theta2Sample
DiagnosticOddsRatioA = PositiveLikelihoodRatioA/NegativeLikelihoodRatioA
SensitivityB = theta3Sample # SensitivityB
SpecificityB = theta4Sample # SpecificityB
PositiveLikelihoodRatioB = theta3Sample/(1-theta4Sample)
NegativeLikelihoodRatioB = (1-theta3Sample)/theta4Sample
DiagnosticOddsRatioB = PositiveLikelihoodRatioB /NegativeLikelihoodRatioB
SensitivityDiff = SensitivityB - SensitivityA # SensDiff
SpecificityDiff = SpecificityB - SpecificityA # SpecDiff
PLRDiff = PositiveLikelihoodRatioB - PositiveLikelihoodRatioA
NLRDiff = NegativeLikelihoodRatioA - NegativeLikelihoodRatioB


par(mar=c(4.1, 3.1, 3.1, 1.1))
plotPost( SensitivityA , xlab= expression(paste("SensA (", theta[1], ")")) , 
          compVal=0.7, ROPE=c(0.4, 0.6),main="SensitivityA\nPosterior distribution ")
plotPost( SpecificityA , xlab= expression(paste("SpecA (", theta[2], ")")) , 
                    compVal=0.7, ROPE=c(0.4, 0.6),
                    main="SpecificityA\nPosterior distribution ") 
plotPost(  PositiveLikelihoodRatioA, xlab= expression(paste("PLRA (", theta, ")")) , 
          compVal=5, ROPE=c(1, 3),
          main="PLRA\nPosterior distribution ")

plotPost(  NegativeLikelihoodRatioA, xlab= expression(paste("NLRA (", theta, ")")) , 
           compVal=0.5, ROPE=c(0.7, 1),
           main="NLRA\nPosterior distribution ") 
plotPost( DiagnosticOddsRatioA , xlab= expression(paste("DORA (", theta, ")")) , 
           compVal=3, ROPE=c(3, 5),
           main="DORA\nPosterior distribution ") 

# Plots for Test B

plotPost( SensitivityB , xlab= expression(paste("SensB (", theta[3], ")")) , 
          compVal=0.7, ROPE=c(0.4, 0.6),main="SensitivityB\nPosterior distribution ")

plotPost( SpecificityB , xlab= expression(paste("SpecA (", theta[4], ")")) , 
          compVal=0.7, ROPE=c(0.4, 0.6),
          main="SpecificityB\nPosterior distribution ")
plotPost(  PositiveLikelihoodRatioB, xlab= expression(paste("PLRB (", theta, ")")) , 
           compVal=5, ROPE=c(1, 3), main="PLRB\nPosterior distribution ") 
plotPost(  NegativeLikelihoodRatioB, xlab= expression(paste("NLRB (", theta, ")")) , 
                      compVal=0.5, ROPE=c(0.7, 1),
                      main="NLRB\nPosterior distribution ") 
plotPost( DiagnosticOddsRatioB , xlab= expression(paste("DORB (", theta, ")")) , 
          compVal=3, ROPE=c(3, 5),
          main="DORB\nPosterior distribution ") 

# Differences in Tests



plotPost( SensitivityDiff , xlab= expression(paste("SensDiff (", theta[3]-theta[1], ")")) , 
          compVal=0.1, ROPE=c(-0.1, 0.1),
          main="SensitivityDiff\nPosterior distribution ")

plotPost( SpecificityDiff , xlab=expression(paste("SpecDiff (", theta[4]-theta[2], ")")) ,
          compVal=0.0, ROPE=c(-0.1, 0.1),
          main="SpecificityDiff\nPosterior distribution ")

plotPost( PLRDiff , xlab=expression(paste("PLRDiff (", theta, ")")) ,
          compVal=0.0, ROPE=c(2, 5),
          main="PLRDiff\nPosterior distribution ")

plotPost( NLRDiff , xlab=expression(paste("NLRDiff (", theta, ")")) ,
          compVal=0.0, ROPE=c(0.2,0.5),
          main="NLRDiff\nPosterior distribution ")
#-----------------------------------------------------------------------------
