## ----setup, cache=FALSE, include=FALSE-----------------------------------
knitr::opts_chunk$set(collapse=TRUE, cache=TRUE, autodep=TRUE, prompt=FALSE, comment = "#>", strip.white=TRUE)
library(knitr)
library(pander)
# quiet = function(x) { 
#   sink(tempfile()) 
#   on.exit(sink()) 
#   invisible(force(x)) 
# } 

## ------------------------------------------------------------------------
#### Load the castControlGE package and set the random seed
library("caseControlGE")
set.seed(979)

#### Generate data with beta0 = -3 as a starting point
tmp = simulateCC(ncase=1000, ncontrol=1000, beta0 = -3, betaG_normPRS=0.450,
                 betaE_bin=0.143, betaE_norm=-0.019, betaGE_normPRS_bin=-0.195,
                 betaGE_normPRS_norm=-0.040, E_bin_freq=0.745)
#### Disease rate too high, try beta0 = -4
tmp = simulateCC(ncase=1000, ncontrol=1000, beta0 = -4, betaG_normPRS=0.450,
                 betaE_bin=0.143, betaE_norm=-0.019, betaGE_normPRS_bin=-0.195,
                 betaGE_normPRS_norm=-0.040, E_bin_freq=0.745)
#### Continue guessing, increasing sample size as we get closer (not run)
## tmp = simulateCC(ncase=1000, ncontrol=1000, beta0 = -3.5, ...
## tmp = simulateCC(ncase=10000, ncontrol=10000, beta0 = -3.4, ...
## tmp = simulateCC(ncase=100000, ncontrol=100000, beta0 = -3.4, ...
## tmp = simulateCC(ncase=100000, ncontrol=100000, beta0 = -3.41, ...
rm(tmp)

## ------------------------------------------------------------------------
#### Set the random seed for reproducability
set.seed(70)

#### Generate a synthetic data set that has similar properties to the PLCO data
dat = simulateCC(ncase=658, ncontrol=753, beta0 = -3.41, betaG_normPRS=0.450,
                 betaE_bin=0.143, betaE_norm=-0.019, betaGE_normPRS_bin=-0.195,
                 betaGE_normPRS_norm=-0.040, E_bin_freq=0.745)

## ------------------------------------------------------------------------
#### Check the simulated data and tabulate the number of cases & controls
kable(list(head(as.data.frame(dat)), table(dat$D, dnn="D")), cap="Simulated PLCO data", book=T)

## ------------------------------------------------------------------------
#### Save the indices of all controls in dat
controls = which(dat$D == 0)

#### t-test of G over the levels of E1
pander(t.test(dat$G[controls] ~ dat$E[controls, 1]), split.cells=11)
#### correlation test between G and E2
pander(cor.test(dat$G[controls], dat$E[controls, 2]))

## ------------------------------------------------------------------------
#### Fit the spmle to the simulated PLCO data
spmleFit1 = spmle(D=D, G=G, E=E, pi1=0.0372, data=dat)

#### Print coefficient estimates from spmle and the logistic model returned by spmle
kable(summary(spmleFit1)$coefficients, caption="spmle, known pi1", digits=4)
kable(summary(spmleFit1$glm_fit)$coefficients, caption="logistic regression", digits=4)

## ------------------------------------------------------------------------
kable(summary(spmle(D=D, G=G, E=E, pi1=0, data=dat))$coef, cap="spmle, rare disease", dig=4)

## ------------------------------------------------------------------------
#### Fit the reduced spmle with bad starting values
spmleRed = spmle(D=D, G=G, E=E[,1], pi1=0.0372, data=dat, 
                 startvals=rep(NA, 4), control=list(use_hess=FALSE))

## ------------------------------------------------------------------------
#### Check convergence and parameter estimates
summary(spmleRed)

## ------------------------------------------------------------------------
#### Likelihood ratio test for reduced vs full model
anova(spmleRed, spmleFit1)
kable(anova(spmleRed, spmleFit1))
pander::pander(anova(spmleRed, spmleFit1))

