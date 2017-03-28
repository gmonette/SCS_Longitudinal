#' ---
#' title: "R Stan examples: Shortitudinal Data"
#' author:
#' - name: Georges Monette
#'   affiliation: York University
#' date: "`r format(Sys.time(), '%B %d, %Y at %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 6
#'      toc_float: true
#' ---
#'
#' ## The problem with shortitudinal data
#'
#' When the number of observations per cluster is very small there is a bias
#' in the estimation of the compositional effect, which will be attenuated
#' (i.e. biased towards 0) because the mean of the observed x's in the sample
#' constitutes a measurement of the 'true mean x' in each cluster with error.
#' Fortunately the amount of error can be estimated from the within-cluster
#' variability in x and we can use this to get an unbiased estimate by
#' taking measurement error into account.
#'
#' This is a simulation to illustrate these biases and to show how to correct for
#' them using a measurement error model in Stan.
#'
#' ## Simulating a data set
#'
#' We will use an example that parallels the mathach-ses example but with very small
#' samples from each school: $n=2$. The compositional effect is 1 and the within effect is -.5.
#' Thus the contextual effect is 1.5.
#'
#' This example also illustrates how to simulate a multilevel data set, which can be
#' useful for power analyses.
#'
#+ include=FALSE
# note that include=FALSE will evaluate but not show code
interactive <- FALSE  # do not run interactive code
knitr::opts_chunk$set(comment='')
#+ include=FALSE, eval=FALSE
# we don't want to evaluate this when running the code for Markdown
interactive <- TRUE  # run this when running code by hand
#'
#' Loading packages:
#' 
#+ packages
library(spida2)
library(magrittr, pos = 1000) 
library(lattice)
library(latticeExtra)

#'
#' ### Parameters for data
#'
J <- 1000 # number of schools
n <- 2   # cluster size
N <- J*n  # number of observations
beta_comp <- 1
beta_within <- -.5
#'
#' ### A simulation function
#'
#' Wrapping the code that generates the data set in a function makes
#' it easy to rerun.
#'
sim <- function() {  # put everything in a function so we can repeat easily
  # not that all assignments in the function don't change anything
  # outside the function
  sim_school <- data.frame(xmean = 3* rnorm(J), id = 1:J) # bad style -- should pass J, etc., as arguments
  sim <- data.frame(xdev = rnorm(N), id = rep(1:J, each = n))
  sim <- merge(sim, sim_school)
  sim <- within(sim, {  # note the curly brace in which statements are listed
    x <- xmean +  xdev
    y <- beta_comp * xmean + beta_within * xdev + rnorm(N)
    }
  )
  sim <- within(sim, {  # dropping 'invisible' variables
    xdev <- NULL
    xmean <- NULL
  })
  sim # return dd
}

set.seed(2132453)  # to make sure this looks the same
dd <- sim()
head(dd)
#'
#'
#' ## Multilevel analysis using group mean and CWG variables
#'
library(nlme)
fit <- lme(y ~ cvar(x,id) + dvar(x,id), dd, random = ~ 1 + dvar(x,id) | id)
summary(fit)
#'
#' ### Biased estimates of compositional effects
#'
wald(fit)
#'
#' Note that the 'within' CI covers the true value but the compositional CI is
#' far from the true value.
#'
#' Why? Measurement error: cvar(x, id) is equal to 'true' xmean + error
#' from the random selection of 2 observations in the school.
#'
#' Why doesn't this affect dvar(x,id)? Because, x is 'true' x for the individual
#' and dvar(x,id) is orthogonal to cvar(x,id).
#'
#' Can we fix this? Let's try a model in Stan that treats 'true' 
#' x_mean as a latent variable.
#'
#' ## Stan model with measurement error model for 'contextual' variable
#'
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#'
print(readLines('shortitudinal_1.stan'), quote = F)
#' We can write a quick function to make this easier
pr <- function(x) print(readLines(x), quote = FALSE) # write a function for repetitive tasks
stanfile <- 'shortitudinal_1.stan' 
# pr(stanfile)
system.time(
  shor_1_dso <- stan_model(stanfile)
)
#'
#' Let's try it on our data.
#'
head(dd)
dat <- list( N = nrow(dd),
             id = idn <- as.numeric(as.factor(dd$id)),
             J = max(idn),
             x = dd$x,
             y = dd$y
            )
mod <- sampling(shor_1_dso, dat)
traceplot(mod)
#' Note the irony that 'true sigma_u' is zero and that's the one
#' giving us trouble
pars <- grepv('^u|^xm',names(mod), invert = T)
traceplot(mod, pars = pars)
pairs(mod, pars = pars)
#'
#' But look at b_contextual and b_compositional! The bias is gone. 
#'
print(mod, pars = pars)
#'
#' ## Dealing with variance parameters that are close to 0 without dropping them
#'
#' Here, we'll put a gamma prior on sigma_u. We can add parameters
#' through data so we can experiment without having to recompile.
#' 
stanfile <- 'shortitudinal_2.stan' 
pr(stanfile)
system.time(
  shor_2_dso <- stan_model(stanfile)
)
#'
mod2 <- sampling(shor_2_dso, c(dat, gamma_par = 4, gamma_scale = .2))
pars <- grepv('^u|^xm',names(mod2), invert = T)
#'
#' Note how the log-posterior trace is almost an inversion of the sigma_u trace
#'
traceplot(mod2, pars = pars)
pairs(mod2, pars = pars)
#' 
#' ### Bias is gone!
#' 
print(mod2, pars = pars)
