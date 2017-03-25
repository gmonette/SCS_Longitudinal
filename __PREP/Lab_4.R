##  
##
##  Missing Data 
##  SPIDA 2012
##
##



library(p3ddev)
library(spidadev)  # note: loads 'car' + others
library(mice)     # load most important package last so its functions are not masked

# Additional function that will be incorporated in spidadev  

# misscode added May 20, 2012
# recodes NAs as a value below the range of observed
# values to produce 3-D marginal value plots

misscode <- function(x,...) UseMethod('misscode')
misscode.default <- function(x,...,offset = .1) {
  rr <- range(x, na.rm = TRUE)
  vmiss <- min(x,na.rm = TRUE) - offset * diff(rr)
  nas <- is.na(x)
  x[nas] <- vmiss
  attr(x,'nas') <- nas
  x
}
misscode.factor <- function(x, ...) {
  nas <- is.na(x)
  x <- addNA(x, ifany = TRUE)
  attr(x,'nas') <- nas
  x
}
misscode.data.frame <- function(x,...) {
  x[] <- lapply(x[],misscode,...)
  isna <- lapply( x, function(x) attr(x,'nas'))
  #   disp(isna)
  isna <- do.call(cbind,isna)
  isna <- apply(isna, 1, sum)
  #   disp(isna)
  x$.nmiss <- isna
  x
}


'
== Example ==
'

# Example to illustrate issues

# X: Treatment Duration
# Z: Weness 
# Y: Satisfaction

# Z is a mediator for the Treatment effect  
#
# generate variables scaled as T scores
# -- Treatment is 'intensity or duration of therapy'
#    and can be considered to have been randomized
# -- Sat is 'relationship satisfaction' the ultimate 
#    criterion for success of therapy
# -- Weness (couple identity) is the mediator of the
#    the effect of Treatment on Sat
#

set.seed(243)
dd <- data.frame( Treat = rnorm( 300, 50, 10 ))
dd$Weness <- .8*(dd$Treat - 50) +  .6 * rnorm(300,0,10) + 50
dd$Sat <-   .8*(dd$Weness - 50) +  .6 * rnorm(300,0,10) + 50

dim(dd)
head(dd)
pairs( dd ) # note that pairwise plots don't reveal 
# some relevant aspects of the 3d structure of mediation

# Try added-variable plots

Init3d(font = 4, cex = 1.2)

# Some parenthetical R code:
# interacting with rgl with a single letter function
.f <- TRUE
f <- function(stay=.f) {
  rgl.bringtotop(stay=stay)  # doesn't work with OS/X
  assign('.f', !.f,envir = .GlobalEnv)
  invisible(stay)
}
class(f) <- c('fun', class(f))
print.fun <- function(fun,...) {
  do.call(fun,list())
  invisible(fun)
}
# Pressing f will bring the 3d window to the foreground,
# pressing it again, allows it to go to the backgroud


Plot3d( Sat ~ Weness + Treat, dd, col = 'blue')
f
Ell3d(alpha = .25, partial = 1, partial.col = 'red', partial.lwd=2)
Fit3d( fitm <- lm( Sat ~ Treat, dd))
summary(fitm)
Fit3d( fitfull <- lm(Sat ~ Treat + Weness, dd), col = 'red')

summary(fitfull)
'
=== Missingness due to Y ===
'
# Suppose missing in Y due to Y
# All Ys over 50 missing

# Let's try complete CC analysis

Init3d(font=4, cex = 1.2)
dd$miss.Y <- with(dd, Sat > 50)   # MNAR
Plot3d( Sat ~ Weness + Treat | miss.Y, dd, col = c('blue', 'gray')) ; k()
Fit3d( fitm.c <- lm( Sat ~ Treat, dd), col='lightblue', alpha= .5, lwd = 2)
Fit3d( fitm.m <- lm( Sat ~ Treat, subset(dd, miss.Y == F)), col='blue', lwd = 2)

Fit3d( fittw.c <- lm( Sat ~ Treat + Weness, dd), col='pink', alpha= .5, lwd = 2)
Fit3d( fittw.m <- lm( Sat ~ Treat + Weness, subset(dd, miss.Y == F)), col='red', lwd = 2)
summary(fitm.c)
summary(fitm.m)
summary(fittw.c)
summary(fittw.m)

'
=== Missingness due to X ===
'
# Suppose missing in Y due to Treat 
# Therefore MCAR for Sat ~ Treat
# and for Sat ~ Treat + Weness
# All Treat over 50 missing

# Let's try complete CC analysis

Init3d(font=4, cex = 1.2)
dd$miss.Treat <- with(dd, Treat > 50)   # MCAR for both models
Plot3d( Sat ~ Weness + Treat | miss.Treat, dd, col = c('blue', 'gray')); k()
Fit3d( fitm.c <- lm( Sat ~ Treat, dd), col='blue', alpha= .2, lwd = 2)
Fit3d( fitm.m <- lm( Sat ~ Treat, subset(dd, miss.Treat == F)), col='blue', lwd = 2)

summary(fitm.c)
summary(fitm.m)
Fit3d( fitfull.c <- lm( Sat ~ Treat + Weness, dd), col='red', alpha= .2, lwd = 2)
Fit3d( fitfull.m <- lm( Sat ~ Treat + Weness, subset(dd, miss.Treat == F)), col='red', lwd = 2)
summary(fitfull.c)
summary(fitfull.m)


# No problem because Missingness is MCAR wrt to both models
# because variable on which missingness depends is in the model

'
=== Missingness due to W ===
'

# Suppose missing in Y due to Weness 
# Therefore MCAR for Sat ~ Treat + Weness
# but perhaps not for Sat ~ Treat 
# All Weness over 50 missing

# Let's try complete CC analysis

Init3d(font=4, cex = 1.2)
dd$miss.Weness <- with(dd, Weness > 50)   # MNAR
Plot3d( Sat ~ Weness + Treat | miss.Weness, dd, col = c('blue', 'gray')); k()
dd$miss.cat <- with(dd, paste( "miss.Treat = " , miss.Treat, "| miss.Weness = ", miss.Weness ))
dd$miss.cat <- with(dd, paste(ifelse( miss.Treat , "Missing dt Treat", ""),
                              ifelse( miss.Weness , "Missing dt Weness", "")))
Plot3d( Sat ~ Weness + Treat | miss.cat, dd)

Fit3d( fitm.c <- lm( Sat ~ Treat, dd), col='blue', alpha= .2, lwd = 2)
Fit3d( fitm.m <- lm( Sat ~ Treat, subset(dd, miss.Weness == F)), col='blue', lwd = 2)

Fit3d( fitfull.c <- lm( Sat ~ Treat + Weness, dd), col='red', alpha= .2, lwd = 2)
Fit3d( fitfull.m <- lm( Sat ~ Treat + Weness, subset(dd, miss.Weness == F)), col='red', lwd = 2)

summary(fitm.c)
summary(fitm.m)

summary(fitfull.c)
summary(fitfull.m)


'
=== Using imputation to estimate a model that is not MCAR ===
'

## Let's see what happens if we impute

# Idea: we need to use Sat ~ Treat to assess causal effect
# But missingness is not MCAR for this model 

# However we can use Treat and Weness for imputation model



dmiss <- dd
dmiss$Sat <- with(dmiss, ifelse(miss.Weness == F, Sat, NA))

head( dmiss)
dmiss <- dmiss[,!grepl("^miss",names(dmiss))]
head( dmiss)

# dmiss $ miss <- T
# dd $ miss <- F
# 
# dcomb <- merge(dmiss, dd[,-(4:6)], all = T)  # with all = F, merge only keeps records with common values
# some(dcomb)


library("mice")   # to make sure

# Multiple imputation for dmiss
#
# 1. Explore missingness patterns
# 2. Create imputed data sets
# 3. Perform analyses
# 4. Pool analyses
# 5. Perform inferences on pooled analyses
#
'
=== Exploring pattern of missingness ===
'
# 1. Explore missingness pattern
dmiss
md.pattern(dmiss)
install.packages("vmv")
library(vmv)
tablemissing(dmiss)
p <- md.pairs(dmiss)
p
plot(p)


install.packages("VIM")
library("VIM")
help(p="VIM")
marginplot(dmiss[,c("Sat","Weness")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)
marginplot(dmiss[,c("Treat","Weness")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)
marginplot(dmiss[,c("Treat","Sat")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)

# Use 'misscode' in spidadev to replace NA with a value lower than non-missing values

dmissna <- misscode(dmiss)

Plot3d( Sat ~ Treat + Weness | .nmiss, dmissna,
        col = c('blue','red'))

# Notes: 
# -- Only one variable, Y, has missing values
# -- Multivariate distribution of non-missing appears
#    multivariabe normal

# get information on imputation
'
=== Set up parameter for imputation ===
'
imp <- mice(dmiss, maxit = 0)
imp  

# Show that only Sat needs imputation, proposes 'pmm' =
# predictive mean matching

imp <- mice(dmiss, seed=23109)
imp
head(complete(imp,1))
head(complete(imp,3))
'
=== Imputation ===
'
# What did 'mice' do?
# -- It created m = 5 complete data frames maxit = 5 times
# -- Each data frame has original non-missing data plus
#    imputed data for each missing value
#
# If more than one variable missing, mice keeps running through the variables
# one at a time imputing from all the other previously imputed variables
#
'
=== Methods for imputation ===
'

#
# Built in methods are:
#
# Numeric:
# pmm      Predictive Mean Matching: choose an oberved value with closest predicted mean
# norm     Bayesian linear regression with normal to generate 'error'
# norm.nob Normal error but no Bayesian 'error' for predictive parameters
# mean     Unconditional mean
# 2L.norm  Two level normal imputation
#
# Factors:
# logreg   Logistic regression for factor with two 'levels' i.e. two values (not related to hierarchical levels)
# polyreg  Polytomous logistic regression (factor >= 2 levels)
# polr     Proportional odds models (factor >= 2 levels)
# lda      Linear discriminant analysis (factor >= 2 levels)
#
# Any distribution:
# sample   Random sample from observed values 
#
# Can add your own as 'mice.impute.newfun'
# and use name 'newfun' for method
#
# Also see 'passive imputation' in ?mice
#


'
=== What happens at each iteration ===
'
#
# What happened at each iteration
#
# Uses Gibbs sampling (a form of MCMC)
# 0. Makes naive imputations for missing data
# 1. Fits imputation model Y on X and W
# 2. Generates random parameters from fitted imputation model
#       using Bayesian posterior distribution
# 3. Uses random parameters to generated predicted imputed value
# 4. Depending on method, adds random error to imputed value
# 5. Back to 1 using imputed and original data

# Actually, 'mice' does m chains of above, each to maxit iterations.
# the final iteration of each chain gives you the m 'complete'
# data frames for analysis

# Why does this work?
# We are generating pretend parameters from pretend data, and
# then pretend data from pretend parameters.
# Shouldn't we be wandering off into Fantasyland??
#
# Magic of Gibbs sampling (also the fundamental theorem
# of copulas 
# If you want to generate a sample from two RVs 
# (here betas and imputed values)
# and you don't know their joint distribution
# but you can generate each conditional on the other
# If they are not too dependent on each other, then
# if you repeat this often enough (V1 from ~V1|V2 and
# then V2 from ~V2|V1) the final values of (V1,V2) will
# be like they were sampled from the joint distribution.
# Like copulas: if you know all the conditionals and THERE
# is a joint distribution with these conditionals, then
# the joint is unique
#

# Now that we understand this, we should be worried

# Did we use enough iterations to converge?

'
== Imputation diagnostics ==
'

# Looking at data and comparing imputed data with complete data
# You don't expect it to be highly similar -- otherwise why bother?

stripplot(imp)
stripplot(imp, pch=20, cex=1.2)
densityplot( imp, scales = list(x = list(relation = "free")))
?stripplot.mids

xyplot( imp, Sat ~ Weness|.imp, pch=20, cex=1.4)  
xyplot( imp, Treat ~ Weness|.imp, pch=20, cex=1.4)  

str(imp)
imp$pad
# can use .imp = imputation number and .id
plot(imp)
methods( class = 'mids')

zm <- imp$chainMean
zm[,,]
matplot(imp$chainMean[,,], type = 'l')  # See below if more than one variable imputed  
matplot(imp$chainVar[,,], type = 'l')

#
# Note: if more than one variable has been imputed (i.e. more than one variable has
# missing value), you can achieve the above more easily with, e.g.,
# plot( imp, c('Sat','Weness'))
# if Sat and Weness had missing data. The fact that this does not work with a single
# imputed variable appears to be a bug in mice (TODO: report the bug!)
#


imp20 <- mice(dmiss, maxit = 20)
matplot(imp20$chainMean[,,], type = 'l') 
matplot(imp20$chainVar[,,], type = 'l')

zz <- complete(imp20, 'long', include=TRUE)
zz$isna <- is.na(subset(zz, .imp ==0)$Sat)  # recycles is.na(Sat) of original data
Plot3d( Sat ~ Treat + Weness | isna, subset(zz, .imp == 1), col = c('blue','red'))

xyplot(imp, Sat~Treat|.imp, pch=20, cex=1.4)   # shows complete and imputed values
xyplot(imp, Sat~Weness|.imp, pch=20, cex=1.4)

'
=== Choosing imputation methods ===
'


# Hmmmm
# mice uses 'safe' pmm which makes minimal assumptions about relationship
# among variables
# Let's see what happens if use multivariate normal
# Note: I thought I would write a 'complete' function in spidadev to do this
# more automatically but I thought I would leave it as is to show an example of
# how you often need to tweak the tools in a package to make them obey your will
#

'
=== Changing the method ===
'
#
# Using a different method for imputation
#
# method
meth <- imp$meth
meth

pred <- imp$pred
pred

# see available methods
?mice
# you can invent your own

# Controlling the choice of methods

meth[meth=='pmm'] <- 'norm'
meth

# We could change 'pred' the same way but no need to
'

=== Effective imputation ===
'
pred

imp.n <- mice(dmiss, meth = meth, pred = pred, m = 10, maxit = 20)
zz.n <- complete( imp.n, 'long', include = T)
zz.n$isna <- is.na(subset(zz.n, .imp ==0)$Sat)  # recycles is.na(Sat) of original data
Plot3d( Sat ~ Treat + Weness | isna, subset(zz.n, .imp == 1), col = c('blue','red'))

# Words fail

'
== The Analysis Phase ==
'

# very short

fit.n <- with( imp.n, lm( Sat ~ Treat))
fit.n

names(fit.n)
lapply(fit.n$analyses, summary) # note consistency with full data analysis

lapply(fit.n$analyses, plot)


'
== Pooling analyses ==
'

pool.n <- pool(fit.n)
summary(pool.n)

# Fraction of missing information fairly high
# Increasing m
#
# How many?
# Early suggestions: 5
# But need more if high proportion has missing variables
#   m = 20 if 10-30% missing
#   m > 40 if half missing
# And need more if we plan to perform multiparameter Wald tests
#   multiply m by k(k+1)/2 where k is number of dfs in test
#


'
=== Pooling more analyses ===
'

imp.n100 <- mice(dmiss, meth = meth, pred = pred, m = 100, maxit = 20)
fit.n100 <- with( imp.n100, lm( Sat ~ Treat))  # a complex mixed model might take much more time
pool.n100 <- pool(fit.n100)

summary(pool.n100)  # compare with full data analysis

'
=== Post-pooling analysis ===
'

# more: can use 'wald' on 'pool.n100'

# Example: plot predicted value +/- 2 se for Treat from 20 to 80

predf <- expand.grid( Treat = 20:80)
Lm <- model.matrix( ~ Treat, data = predf)
ww <- wald(pool.n100, Lm)
ww
predf <- cbind(predf, coef( ww, se = 2))
head(predf)
td(lwd = 2, lty = c(1,2,2), col='black')
xyplot(coef + coefp + coefm ~ Treat, predf, type = 'l' )

'

== Using Multiple Imputation for Multilevel Data ==
'

#  To illustrate what can be gained from MI even when 
#  missingness is MCAR in many variable, we use the 
#  familiar 'hs' data set

head(hs)

#  Consider the model mathach ~ ses * Sex * Sector 
#  where mathach, ses or Sex can be missing

dim(hs)
hsm <- hs
set.seed(1247)

hsm$mathach [ sample(nrow(hs), 200)] <- NA
hsm$ses[ sample(nrow(hs), 200)] <- NA
hsm$Sex [ sample(nrow(hs), 200)] <- NA

tablemissing(hsm)
hsm.mc <- misscode(hsm[,c('mathach','ses','Sex')])
head(hsm.mc)
Plot3d( mathach ~ ses + as.numeric(Sex) | .nmiss, hsm.mc )

# full data

fit.full <- lme( mathach ~ (ses + Sex + Sector)^2, hs, random = ~ 1 |school)
summary(fit.full)
wald(fit.full,'Sex')
wald(fit.full,':Sex|Male:')

fit.cca <- lme( mathach ~ (ses + Sex + Sector)^2, hsm, random = ~ 1 |school,
                na.action = na.omit)

summary(fit.cca)
summary(fit.full)

'
=== Imputation Model ===
'

# Impute

# dry run
imp <- mice( hsm, maxit = 0)
imp

# For level 1 normally distributed variables we can use 2L.norm
?mice.impute.2L.norm
# The cluster variable is identified with a -2
# Level 1 variables random related to the imputed variable are identified with a 2

pred <- imp$pred
meth <- imp$meth

pred
meth

meth["mathach"] <- "2l.norm"

pred["mathach",]
pred["mathach","school"] <- -2
pred["mathach","ses"] <- 2


meth["ses"] <- '2l.norm'
pred["ses",]
pred["ses","school"] <- -2
pred["ses","mathach"] <- 2

pred

imp <- mice( hsm, pred = pred, meth = meth) 
plot(imp, c('mathach','ses','Sex')) # didn't look as if it had mixed

imp <- mice( hsm, pred = pred, meth = meth, maxit = 30) 
plot(imp, c('mathach','ses','Sex'))
stripplot(imp, pch=20, cex=1.2)


'
=== Analysis ===
'  
##
## Analysis
##

fit <- with ( imp, lme( mathach ~ (ses+Sex+Sector)^2, random = ~ 1 | school))
lapply( fit$an , summary)
'
=== Pooling ===
'
pl <- pool(fit)
summary(pl)

'
=== Post-pooling analysis ===
'
#
# Pooling produces a Wald-type object with estimated coefficients, 
# their estimated variance-covariance matrix and estimated degrees
# of freedom.
#
# This is ideally suited to Wald-type estimation and testing based on
# linear combinations of coefficients. The wald method for 'mipo' objects
# currently uses the minimum degrees of freedom for a component in a
# linear combination. An improvement would be to explore using a better
# method. 
#
# Also, the between-imputation contribution to the pooled variance may
# lead to less stable tests of hypotheses with multiple degrees of 
# freedom. Increasing m, the number of imputations, in these cases is
# desirable.

wald(pl, ":")
wald(fit.full, ":")
