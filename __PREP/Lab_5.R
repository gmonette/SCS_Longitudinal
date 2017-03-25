
##
##  SPIDA 2012
##  Mixed Models with R
##  G. Monette
##  May 2012
##
##  Introduction to Generalized Linear Mixed Models with MCMC
##

##
## This file is http://scs.math.yorku.ca/index.php/SPIDA_2012:_Mixed_Models_with_R/Lab_5
##

library(spidadev)
library(p3ddev)
?migraines
head(migraines)
xqplot( migraines )

ds <- migraines
head( ds )


xyplot( ha ~ time | id, ds)
xyplot( ha ~ time | id, ds, strip = FALSE,
        panel = function ( x , y , ...){
          panel.xyplot( x, y , ...)
          panel.loess (x , y , ..., family = 'gaussian')
          panel.abline(v=0)
        })
# experiment with layout = c( 8, 8 ), etc.

xyplot( jitter(ha) ~ time | id, ds, strip = FALSE,
        layout = c( 8, 8 ),
        panel = function ( x , y , ...){
          panel.xyplot( x, y , ...)
          panel.loess (x , y , ..., family = 'gaussian', lwd = 2 , col = 'red')
          panel.abline(v=0)
        })

ds$time.first <- with(ds, capply( time, id, min, na.rm = T))
ds$ido <- with(ds, reorder( id, time.first))
windows()
xyplot( jitter(ha,.5) ~ time | ido, ds, strip = FALSE,
        layout = c( 7, 19 ),
        panel = function ( x , y , ...){
          panel.xyplot( x, y , ...)
          panel.loess (x , y , ..., family = 'gaussian', lwd = 2 , col = 'red')
          panel.abline(v = 0, lty = 2)
        },
        xlab = "days after onset of treatment",
        ylab = "migraine",
        scales = list( y = list( at = 0:1, labels = c("no","yes"))))


xyplot( ha ~ time , ds, strip = FALSE, groups = id,
        panel = panel.superpose,
        panel.groups = function ( x , y , ...){
          panel.loess (x , y , ..., family = 'gaussian')         
        })
# 45 degree rule
xyplot( ha ~ time , ds, strip = FALSE, groups = id,
        xlim = c(-10,50),
        panel = panel.superpose,
        panel.groups = function ( x , y , ...){
          panel.loess (x , y , ..., family = 'gaussian')
          
        })
xyplot( ha ~ time | hatype * medication, ds, strip = TRUE, groups = id,
        xlim = c(-10,50), lwd = 2,
        strip.left = TRUE,
        panel = panel.superpose,
        panel.groups = function ( x , y , ...){
          panel.loess (x , y , ..., family = 'gaussian')
          
        })


'
=== Migraine data with transformed asymptotic time model ===
'
# Is the treatment effective

ds$treat <- ds$time > 0

# Fitting an asymptotic model with a transformation of time


fit1 <- glmmPQL ( ha ~ treat, subset(ds, time < 30),
                  family = binomial,
                  random = ~ 1 | id )
summary( fit1 )

fit2 <- glmmPQL ( ha ~ treat + I( exp( - time / 13) * treat),
                  ds,
                  family = binomial,
                  random = ~ 1 | id)
summary( fit2 )

pred <- data.frame( time = seq(-15, 60, .1))
pred$treat <- pred$time >0

pred$log.odds <- predict( fit2, pred , level = 0)

xyplot( log.odds ~ time , pred, type = 'l')

probs <- function ( lo ) exp(lo)/(1+exp(lo))
lo <- function( p ) log( p / (1 - p ))

xyplot( log.odds ~ time , pred, type = 'l',
        ylab = 'probability of headache',
        scales = list( y = list( at = lo( seq(.1,.9,.05)),
                                 labels = seq( .1, .9, .05) )))

# is the improvement significant ?
summary( fit2 )

# How big is the spike?
wald( fit2, rbind( spike = c( 0, -1, 1)))



fit3 <- glmmPQL ( ha ~ treat + I( exp( - time / 13) * treat),
                  ds,
                  family = binomial,
                  random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)
summary( fit3 )
some(ranef( fit3 ))

# merge ranef with Level 2 variables ( this should work with augPred = T but does not)
ra <- ranef(fit3)
some(ra)
ra$id <- factor( rownames( ra ) )
some( ra )

ra <- merge( ra , up( ds, ~ id ))    # merge
some( ra)

Init3d(family = 'serif', cex = 1.2)

## HINT:
## Don't type the variable names in the next line:
## just cut and paste from names
## printed with some(ra) and then add backquotes, etc.

Plot3d(   `(Intercept)`~  `treatTRUE` + `I(exp(-time/13) * treat)` | hatype, ra)
Ell3d()
Plot3d(   `(Intercept)`~  `treatTRUE` + `I(exp(-time/13) * treat)` | medication, ra)
Ell3d()
summary( fit3 )

#  add medication

fit4<- glmmPQL ( ha ~ (treat + I( exp( - time / 13) * treat)) * medication ,
                 ds,
                 family = binomial,
                 random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)

summary( fit4 )
wald( fit4, 'medication' )   # seems quite significant

### with lme4

fit4.r <- glmer(ha ~ (treat + I( exp( - time / 13) * treat)) * medication 
                + (1 + treat +I( exp( - time / 13) * treat)| id),
                data = ds,
                family = binomial)
wald(fit4.r)
VarCorr(fit4.r)

"
=== Annual Fourier analysis: seasonal effect for migraines ===
"

Sin <- function( x ) cbind( ".c" = cos( 2* pi * x /365.25),
                            ".s" = sin( 2* pi * x /365.25))


fit4<- glmmPQL ( ha ~ (treat + I( exp( - time / 13) * treat)) *  medication
                 + dos +Sin( dos) + Sin (2*dos) + Sin( 3*dos ),
                 ds,
                 family = binomial,
                 random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)

summary( fit4 )
wald(fit4, "Sin")

wald(fit4, ":")
wald ( fit4, "medication")

'
=== Compare with glmer ===
'  
library(lme4)
fit4.g<- glmer ( ha ~ (treat + I( exp( - time / 13) * treat)) *  medication
                 + dos +Sin( dos) + Sin (2*dos) + Sin( 3*dos )
                 + (1 + treat +I( exp( - time / 13) * treat)| id), # ONLY DIFF
                 ds,
                 family = binomial)
#random = ~ 1 + treat +I( exp( - time / 13) * treat)| id) 
summary(fit4.g)
wald(fit4.g, "Sin")


##
##
##

fit4cor<- glmmPQL ( ha ~ (treat + I( exp( - time / 13) * treat)) * medication
                    + dos +Sin( dos) + Sin (2*dos) + Sin( 3*dos ),
                    ds,
                    family = binomial,
                    correlation = corCAR1( form = ~ dos | id),
                    random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)

## Problem with data
tab( ds, ~ id + dos)
max( apply( tab( ds, ~ id + dos, tot= FALSE), 1, max))      # find value > 1


fit4cor<- glmmPQL ( ha ~ (treat + I( exp( - time / 13) * treat)) * medication
                    + dos +Sin( dos) + Sin (2*dos) + Sin( 3*dos ),
                    subset(ds, id != 90),
                    family = binomial,
                    correlation = corCAR1( form = ~ dos | id),
                    random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)

# note that the actual value for the correlation coeffiecient would probably
# vary from person to person because it is relatied to migraine persistence which would
# be different.

# Other approach: Use weekly averages as binomials (AR will show up as
# overdispersion, AR weaker from week to week


summary( fit4cor )
wald ( fit4cor, "medication")
wald ( fit4cor, "Sin")

# Exercises:
# 
# 1. ANALYZE EFFECT OF medication: PRE TREATMENT DIFFERENCES
#    - AMOUNT OF IMPROVEMENT
#    - POST -TREATMENT
# 2. Problem with Time/ 13 Not that this is a way of discounting early spikes
#    Try other ways, e.g. cutting out short term data (e.g. 0 -15)
#     OR  a spline
#     OR  Different half times than 13
#     OR non-linear estimation for half life (use package 'gnm')
# 3. Do the above with glmer in lme4

#? zzz <- ranef( fit4 , aug = T)
#? names( zzz ) <- c( 'int','treat','spike')
"
=== Seasonal effects for migraines ===
"

Sin <- function( x ) cbind( ".c" = cos( 2* pi * x /365.25),
                            ".s" = sin( 2* pi * x /365.25))

fit4 <- glmmPQL ( ha ~ treat + I(exp( - time / 13) * treat)
                  + Sin( dos ),
                  ds,
                  family = binomial(link="probit"),
                  random = ~ 1 + treat | id)

summary( fit4 )
wald(fit4, "Sin")

fit4 <- glmmPQL ( ha ~ treat + I(exp( - time / 13) * treat)
                  + Sin( dos )+ Sin( 2*dos )
                  + Sin( 3*dos ) + Sin( 4*dos ),
                  ds,
                  family = binomial,
                  random = ~ 1 + treat | id)
summary( fit4 )
wald( fit4, "Sin\\(4")

fit4 <- glmmPQL ( ha ~ treat + I(exp( - time / 13) * treat)
                  + Sin( dos )+ Sin( 2*dos )
                  + Sin( 3*dos ) + Sin( 4*dos ) + airq + I(airq^2),
                  ds,
                  family = binomial,
                  random = ~ 1 + treat | id)
summary( fit4 )
wald( fit4, -1)  # test of model
wald( fit4, 'airq')
wald( fit4, 'Sin')   # tough call
summary(ds$airq)

pred <- expand.grid( treat=0, time=0, dos=0, airq=3:70)
pred$loha <- predict( fit4, pred, level = 0)
pred$prha <- with( pred, exp(loha)/(1+exp(loha)))
xyplot( prha ~ airq, pred)

# Exercise: Is higher value at low airq an artifact of
# extrapolating a quadratic? Or is it real?
# Try exploring with  a spline

# Exercises:
# Define L matrices to estimate log(odds) of HA under various conditions
# Use  wald.transform to turn estimates to probabilities

'
=== Magic of MCMC ===
'

###
### How MCMC works? Based on one of the statistical wonders of 
###    the 2nd half of the 20th century
###

#
"
=== What's a Markov Chain? ===
"
#   Sequence of random variables/vectors where 
#   the distribution for the future depends on the 
#   past only through the present -- i.e. if you know
#   the present, there is no additional information
#   about the future from knowing the past values
#   of the process.
#   MC in the real world (Markov Process is cont. time MC) 
#   Anything that depends on the past through the present
#   -- sometimes we can encode the past into the present 
#   to turn a non-Markov random variable into a 
#   Markov Chain for a random vector
#   

# What's a Monte Carlo Markov Chain:
# One you generate with a random number generator on
#  your computer i.e. obtained by using
#  Markov Chain Monte Carlo.

# MCMC Principle:
# If you don't know something, make it up!!!
# Or: How to generate random numbers when you can't
# 

# Suppose you want to generate bivariate normal (Y1,Y2)
# -- But you don't know how to do marginal normals
# -- But you do know how to generate Y1|Y2
#    and Y2 | Y1
# If only you could generate Y1 then you could use Y2 | Y1
#   and done
# But you can't
# Is this a realistic situation?
# Missing data: If you knew Ymis you could get random beta
#               If you knew random beta you could 
#                 generate Ymiss
# But you can't get started 
# Solution: 
#      Make up your first number
#      Then keep generating
#      conditional random numbers -- back and forth
# What price do we pay?
# - May take a long time to shake the effect of the starting value
#     **burnin**
# - Obervations are not independent - so to get close to
#   indendence we need to **thin** -- or at least recognize
#   that we have less information than we seem to.
#

# Here a conditional normal random number generator
rcnorm <- function(last, which.random , mean, var) {
  # generate part of a random normal vector
  # conditional on other values
  # Warning: bad numerical method
  # Arguments:
  # last:    current full vector
  # which.random:  which to generate
  # mean of full
  # variance of full
  w <- which.random
  cvar <- var[w,w] - var[w,-w] %*%
    solve( var[-w,-w], var[-w,w])
  cmean <- mean[w] + var[w,-w] %*%
    solve( var[-w,-w], last[-w] - mean[-w])
  require(MASS)
  ran <- mvrnorm( 1, cmean, cvar)
  last[w] <- ran
  last
}

## MCMC (here: Gibbs sampling) approach to conundrum

# 0. Make up a Y1, it needs to have positive density
# 1. Generate Y2 | Y1 
# 2. Generate Y1 | Y2
# 3. If tired stop, if not go back to 1

# Example: 
Mean <- c(5,5) 
V <- cbind(c(4,3.8),c(3.8,4))
cov2cor(V)

plot( ell(Mean, V),
      xlim = c(-1,11), ylim = c(-1,11), type = 'l',
      ylab = list(~Y[2],cex = 1.2),
      xlab = list(~Y[1],cex = 1.2))

lines( ell(Mean, V, radius = 2:3), type = 'l')

# Reg of y2 on Y1
# E(y2|y1) = 5 + (3.9/4) (y1-5) = 5- 5*3.9/4 + (3.9/4) y1
# E(y1|y2) = 5 + (3.9/4) (y2-5) = 5- 5*3.9/4 + (3.9/4) y2
# 
# If y = a + b x
# then x = (y - a)/b = -a/b + y/b

# abline( a = 5*(1-3.9/4), b = 3.9/4)
# abline( a = -5*(1-3.9/4)/(3.9/4), b = 1/(3.9/4))

# reg of Y2 on Y1
abline( a = Mean[2] - (V[2,1]/V[1,1])*Mean[1], b = V[2,1]/V[1,1])
# reg of Y1 on Y2
# "a" = Mean[1] - (V[1,2]/V[2,2])*Mean[2]
# "b" = V[1,2]/V[2,2]
abline (
  a = -(Mean[1] - Mean[2]*V[2,1]/V[1,1])/(V[2,1]/V[1,1]),
  b = 1/(V[2,1]/V[1,1]))



set.seed(2341)
# Step 0: Guess
# Start with Y1 = 10
Y.0 <- c(10,NA)
Y.last <- Y.0
Y.sample <- c()  # empty vector to collect sample


# Step 1:
abline(v=Y.last[1])
Y.1 <- rcnorm( Y.last, 2, Mean, V)
points( rbind( Y.1),col = 'red', pch = 20)

# Step 2:
abline( h = Y.1[2])
Y.2 <- rcnorm( Y.1, 1, Mean, V)
points( rbind( Y.2),col = 'red', pch = 20)
Y.sample <- c(Y.sample, Y.2)

# Step 3: if tired stop
# Otherwise 
Y.last <- Y.2
# go back to 1

##
##  OR, if tired write a program
##  I'm going to use a loop:
##

iter <- 1:10
for ( i in iter){
  Y.1 <- rcnorm( Y.last, 2, Mean, V)
  points( rbind( Y.1),col = 'red', pch = 20)
  Y.2 <- rcnorm( Y.1, 1, Mean, V)
  points( rbind( Y.2),col = 'red', pch = 20)
  Y.sample <- c(Y.sample, Y.2)
  Y.last <- Y.2
}

## Sample

Ysample <- matrix( Y.sample, ncol = 2, byrow = T)
plot(Ysample)
dim(Ysample)

# What sample looks like relative to population?

# Draw an invisible ellipse big enough that it should hold everything:
plot( ell(Mean,V,radius =3.5), type = 'n')
# Draw the population standard ellipse
lines( ell(Mean,V), type = 'l', lty = 1, col = 'red')
# Plot the sample
points(Ysample,  pch = '.')
# Plot the sample ellipse
lines( dell(Ysample), col = 'blue', lwd=3)


dim(Ysample)
apply( Ysample, 2, mean)

2/sqrt(83)
.9/0.219
effectiveSize( Ysample)
Ysample <- mcmc(Ysample)
plot(autocorr.diag(Ysample))
qqmath(Ysample)
densityplot(Ysample)
HPDinterval(Ysample)


# Exercise:
# Get 10,000 and use all
# What is the effectiveSize?
# Keep every tenth. [How?] What is the effectiveSize?
# Lessons: 
#    If you want an 'independent' sample for some
#    reason, you may need to thin a lot
#    Otherwise, keep everything but take reduced 
#    effective sample size into account.
#

#
# Exercise: Try again with:
#  1) Lower dependence  V <- 4* diag(2)
#  2) Higher dependence V <- cbind( c(4,3.99),c(3.99,4))
#
# We've already seen MCMC in action for missing data.
# Now we'll see it in action with Mixed Model

# MCMCglmm
# Advantages:
# - can handle mulivariate
# - broad range of distributions for response:
#     
#   "gaussian", 
#   "poisson",     # model includes possible overdispersion
#   "categorical", # includes binomial logistic
#   "multinomial", 
#   "ordinal",      # includes binomial probit
#   "exponential",  
#   "geometric", 
#   "cengaussian", 
#   "cenpoisson", 
#   "cenexponential", 
#   "zipoisson", 
#   "zapoisson", 
#   "ztpoisson", 
#   "hupoisson" and 
#   "zibinomial" 
#   
#   are supported, where the  prefix 
#   "cen" means censored, e.g. ceiling or floor effect, 
#   "zi" means zero inflated, i.e. excess 0 for a different reason
#   "za" means zero altered, excess or incess 0's'
#   "zt" means zero truncated (not observed) 
#   "hu" means hurdle: a binomial model to determine 
#   whether it's 0 or greater that 0, then a zt model for 
#    those that are >0
#   
#   Note: no negative binomial: can take care of overdispersion
#   with random effects, but???
#   
#   Can handle multivariate with different distributions!!
#   E.g. one component gaussian, other binomial

# Non-mixed example

?Traffic
head(Traffic)
tab( Traffic, ~ year + limit)
histogram( ~ y, Traffic)
histogram( ~ y|year*limit, Traffic)

Traffic$yr <- factor(Traffic$year)
fit <- glm( y ~ year + limit + day, Traffic, 
            family = 'poisson')
summary(fit)

fit.q <- glm( y ~ year + limit + day, Traffic, 
              family = 'quasipoisson')
summary(fit.q)

Traffic$id <- factor( 1:nrow(Traffic))
fit.glmer <- glmer( y ~ year + limit + day + (1|id), 
                    data = Traffic, family = 'poisson')
summary(fit.glmer)


# True count data is almost almost always more dispersed 
# than Poisson
# You expect Poisson under ideal constant conditions
# Any departure (e.g. uncontrolled variation 'heterogeneity') 
# with add Dispersion.
# Poission does not have a separate dispersion parameter
# THe var = mean
# Using mean to estimate var when OD will underestimate
# the var. 
#
# Not like the normal. Since the normal has a separate
# variance parameter, AND since uncontrolled factors
# especially if many relative small relative independent
# will have add a normal component, an incomplete model
# does not take you out of 'normality'
#
# But with the Poisson any missing predictor will make
# your model fishy.
#
# balancing iti's 

# MCMCglmm model

#   eta = XB + epsilon
#   Y ~ Poisson( exp(eta))

fitmc <- MCMCglmm( y ~ yr + limit + day, 
                   data = Traffic, 
                   family = 'poisson')
summary(fitmc)  # much more similar to quasipoisson than to poisson
# look for effective sample size
# for final results, get more
summary(fit.q)
# pdf()
par(mfrow=c(2,2))
plot(fitmc, auto.layout = FALSE )
# dev.off()

# We can increase eff.samp size for 'units'
library(MCMCglmm)
fitmc.u <- MCMCglmm( y ~ yr + limit + day, 
                     data = Traffic, 
                     family = 'poisson',
                     nitt = 24000)
summary(fitmc.u)
Z <- matrix(rnorm(2000),ncol=2)
head(Z)
plot( Z <- mcmc(Z))



autocorr.diag(Z)
thin(Z)
window(Z)
summary(Z)
acfplot(Z)
qqmath(Z)
HPDinterval(Z)

rejectionRate(Z)

time(Z)
xyplot(Z)

# burnin
# thin

# ways of choosing?
# MCMC : use default; burnin 6000
# MCMC : thin = 10
# the report using diagnostics the eff.sample size

?mcmc
z <- mcmc(Z, thin = 100)
xyplot(z)
plot(z)


# Example with hs

fitlme <- lme(
  mathach ~ (Sex+ses)*Sector,
  data = hs,
  random = ~ 1 | school)
summary(fitlme)


# same model in MCMCglmm

hs$school <- factor(hs$school)

fitmc <- MCMCglmm(
  mathach ~ Sex*ses*Sector,
  data = hs,
  random = ~ school, 
  family = 'gaussian' )

summary(fitmc) 
prior <- list(  B = list(mu = rep(0,8), V = diag(8) *1e+10 ), 
                G = list(G1 = list(V = 1, nu = 0.02)), 
                R = list(V = 1, nu = 0.02))



fitmc <- MCMCglmm(
  mathach ~ Sex*ses*Sector,
  data = hs,
  random = ~ school, 
  prior = prior,
  family = 'gaussian' )
summary(fitmc)

plot(fitmc)


## Medication example



fit4<- glmmPQL ( ha ~ (treat + I( exp( - time / 13) * treat)) * medication ,
                 ds,
                 family = binomial,
                 random = ~ 1 + treat +I( exp( - time / 13) * treat)| id)

# running in MCMCglmm

1) fixed formula same
2) family?
3) random very different
4) For binomial need fixed sigma^2

head(ds)
ds $ treat <- 1*ds$treat
ds $ time.eff <- with(ds, exp(-time/13)*treat)
"

=== Fixing a variance ===
"  
prior <- list( R = list(V = .05, nu = 0, fixed = 1),
               G = list(G1=list(V=diag(3), nu = .02)))

ds $ id <- factor( ds $ id )
fit4mc<- MCMCglmm ( ha ~ (treat + I( exp( - time / 13) * treat)) * medication ,
                    data = ds,
                    family = "categorical",
                    random = ~ us( 1 + treat + time.eff):id,
                    prior = prior)

# why is R: sigma^2 = .05 something that works.
# Otherwise, if a phat gets close to 1 or 0, the variance of that point
# becomes close to 0 which makes the model stick to the point. 
# Keeping R as a minimal value prevents the model from sticking to points


'
=== Fitting a multi-response model to fit a MAR model with missing data ===
'  
# Recreate the Treat-Weness-Sat data set.

set.seed(243)
dd <- data.frame( Treat = rnorm( 300, 50, 10 ))
dd$Weness <- .8*(dd$Treat - 50) +  .6 * rnorm(300,0,10) + 50
dd$Sat <-   .8*(dd$Weness - 50) +  .6 * rnorm(300,0,10) + 50

ddmiss <- dd
ddmiss $ Sat [ ddmiss$Weness > 50 ] <- NA

# 
# MCMCglmm makes it possible to fit multivariate model with cases
# that have missing data for the response, in contrast with traditional
# multivariate linear models where all cases must have complete data
# on variables in the model
# 
# We use MI to adjust for mediator-based missingness in the Weness 
# example. Here we will use a MAR model.
# 

fit <- MCMCglmm( cbind(Sat,Weness) ~ trait/Treat -1, data = ddmiss, 
                 rcov = ~us(trait):units,
                 family = c('gaussian','gaussian'))
plot(fit)  # needs more iteratoins
summary(fit)
HPDinterval(fit$Sol)
