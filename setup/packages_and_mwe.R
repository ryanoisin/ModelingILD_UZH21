# --------------------------------------------------------------------------------------------
# ------------------------------- Package Installation ---------------------------------------
# --------------------------------------------------------------------------------------------

# Recommended R version 4.0.4

# Part 1 Packages
install.packages("rjags")
install.packages("coda")
# NOTE: rjags needs a separate program, Jags, which can be downloaded from 
# https://sourceforge.net/projects/mcmc-jags/files/

# Part 2 Packages 
install.packages("ctsem", dependencies = TRUE)
install.packages("expm")
# note - installation of ctsem can be quite involved
# - if throwing errors, you may need to follow the vignette foud on: https://github.com/cdriveraus/ctsem
install.packages("pkgconfig")
install.packages("ggplot2")
install.packages("devtools") # you need this to install packages from github
# Note - you also need Rtools to use devtools: https://cran.r-project.org/bin/windows/Rtools/
devtools::install_github("ryanoisin/ctnet")
install.packages("qgraph")


# --------------------------------------------------------------------------------------------
# ------------------------------------- Part 1: MWE ------------------------------------------
# --------------------------------------------------------------------------------------------

#load packages#
library(rjags)
library(coda)

#load datafile for test analyses#
load(file = "DataJane.Rdata")

### Run some code to test if packages and JAGS are working as they should###
### Example of what the output should look like in OutputExample.txt.
# Exact numbers in the output can differ each time the code is run.


dat_Jane <- data.frame(mydata_Jane$scale)
dat_Jane <- as.matrix(dat_Jane)
colnames(dat_Jane) <- c("mood")

ins = list(
  list(phi = .6, Ivar = 100, mu = 50),
  list(phi = .3, Ivar = 60, mu = 60),
  list(phi = -.2, Ivar = 80, mu = 70)
)
nt = length(dat_Jane)

# After this code a model should be compiled and initialized if JAGS was installed#

jags_AR1 <- jags.model('AR1model.txt',
                       data = list('nt' = nt, 
                                   'y' = as.numeric(dat_Jane)),
                       inits = ins,
                       n.chains = 3) 
# Update iterative procedure should be started after this#
update(jags_AR1, 2000)
# Update iterative procedure should be started after this#
samps_AR1 <- coda.samples(model = jags_AR1, 
                          variable.names = c('phi','mu','Ivar'),
                          n.iter = 2000) 

## produce analysis results ##
gelman.diag(samps_AR1)
summary(samps_AR1) 

# --------------------------------------------------------------------------------------------
# ------------------------------------- Part 2: MWE ------------------------------------------
# --------------------------------------------------------------------------------------------


# Load necessary packages
library(ctsem)
library(ctnet)

# load object to test proper working of packages
load("simfit.rda")

# test correct working of ctsem and ctnet (minimal working example)
ctsem_results <- summary(simfit)
drift_est <- ctStanContinuousPars(simfit)$DRIFT

testobj <- ctCentrality(drift = drift_est, dt = 1)

# testobj should be a named numerical vector of length 8
# TEC_eta1     TEC_eta2     TEC_eta3     TEC_eta4     IEC_eta1     IEC_eta2 
# -0.031355995  0.037057973 -0.026180320 -0.005563353  0.001776633 -0.051701761 
# IEC_eta3     IEC_eta4 
# -0.068699611 -1.056678710 

# Below tests should all return "true"
length(testobj) == 8
names(testobj)[1] == "TEC_eta1"
all.equal(testobj[1], -0.031355995, use.names = FALSE)
# last output should be along the lines of "names for target but not for current"  "Mean relative difference: 1.52054e-08"
