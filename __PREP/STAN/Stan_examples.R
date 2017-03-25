#' ---
#' title: "R Stan examples"
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
#+ setup, include=FALSE
knitr::opts_knit$set()
knitr::opts_chunk$get(cache=TRUE,eval=FALSE)
#'
#' # Installing Stan
#' 
#' TODO
#' 
#' # Simple examples
#'
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
windowsFonts(Arial=windowsFont("TT Arial")) # you might not need to do this
help(p=rstan)
library(loo)  # 
help(p=loo)
#' 
#' 
#' 
library(spida2)
data(hw)
library(p3d)
Init3d()
Plot3d(Health ~ Weight + Height | Outlier, hwoutliers)
fg()
Id3d(labels = hwoutliers$Outlier, pad = '   ')
fit <- lm(Health ~ Weight + Height, hwoutliers, 
          subset = Outlier == 'none')
summary(fit)
Fit3d(fit, alpha = .5)
fg()

fit3 <- lm(Health ~ Weight + Height, hwoutliers, 
           subset = Outlier %in% c('Type 3', 'none'))

Fit3d(fit3, col = 'magenta')
#' 
#' Generic Stan model for regression with 
#' improper uniform prior on betas
#' and uniform on sigma
#'

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
windowsFonts(Arial=windowsFont("TT Arial")) 


reg_model <- 
"
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] y;  // response
}
parameters {
  vector[P] beta;   // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  y ~ normal( X * beta, sigma ); // note that * is matrix mult.
                                 // For elementwise multiplication use .*
}
"

# Create reg_model 'dynamic shared object module' which is compiled C++ code
# that generates HMC samples from the posterior distribution

system.time(
reg_model_dso <- stan_model( model_code = reg_model)
)

#
# Prepare the data list
# striplevels
head( dat <- subset(hwoutliers, Outlier == 'none') )
head( Xmat <- model.matrix(Health ~ Weight*Height*Outlier, dat) )

dat_list <- list(N = nrow(Xmat), P = ncol(Xmat), 
                 X = Xmat, y = dat$Health)
#
# Sample from the posterior distribution
#
system.time(
  fit_stan <- sampling(reg_model_dso, dat_list)
)
summary(fit_stan)
plot(fit_stan)
pairs(fit_stan,pars=c('beta','sigma','lp__')) # note log-lik profiles
traceplot(fit_stan)
get_posterior_mean(fit_stan)[,5]

# Define a function that returns the posterior mean prediction

fun <- function(stanfit) {
  post <- get_posterior_mean(stanfit)
  beta <- post[,ncol(post)]  # use last column (all chains)
  function(Weight, Height) beta[1] + beta[2] * Weight + beta[3] * Height
}
fun(fit_stan)  # this is a closure
fun(fit_stan)(2,3)
Fit3d(fun(fit_stan), col = 'black')

#
# Including Type 3 outlier
#
dat3 <- subset(hwoutliers, Outlier %in% c('none','Type 3'))
dat3

head( Xmat3 <- model.matrix(Health ~ Weight + Height, dat3) )

dat3_list <- list(N = nrow(Xmat3), P = ncol(Xmat3), 
                 X = Xmat3, y = dat3$Health)

system.time(
  fit3_stan <- sampling(reg_model_dso, dat3_list)  # same dso
)
summary(fit3_stan)
plot(fit3_stan)
pairs(fit3_stan,pars=c('beta','sigma','lp__'))
traceplot(fit3_stan)
get_posterior_mean(fit3_stan)[,5]

Fit3d(fun(fit3_stan), col = 'black')

#
#  So far very boring
#  -- nothing new, MCMC with normal error and uniform prior
#     like OLS
#
#  It's as easy as pi to use a different family of distributions
#  for error.
# 
#  Exactly the same except for the error distribution and
#  add nu for degrees for freedom for t distribution

robust_model <- 
"
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] y;  // response
  int nu;   // degrees for freedom for student_t
}
parameters {
  vector[P] beta;   // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  y ~ student_t(nu, X * beta, sigma ); 
}
"

system.time(
  robust_model_dso <- stan_model(model_code = robust_model)
)

fit3_stan_6 <- sampling(robust_model_dso, c(dat3_list, nu = 6))

fit3_stan_6
pairs(fit3_stan_6, pars = c('beta','sigma','lp__'))
traceplot(fit3_stan_6)

Fit3d(fun(fit3_stan_6), col = 'orange')

# Let's try more kurtosis

fit3_stan_3 <- sampling(robust_model_dso, c(dat3_list, nu = 3))
pairs(fit3_stan_3, pars = c('beta','sigma','lp__'))
traceplot(fit3_stan_3)
fit3_stan_3

Fit3d(fun(fit3_stan_3), col = 'orange')

## Adding fit indices -----------------------------------------

robust_model <- 
  "
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] y;  // response
  int nu;   // degrees for freedom for student_t
}
parameters {
  vector[P] beta;   // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  y ~ student_t(nu, X * beta, sigma); 
}
generated quantities { 
  // computes log likelihood at each point to compute WAIC
  vector[N] log_lik;
  for(n in 1:N) {  // index in for loop need not be declared
    log_lik[n] = student_t_lpdf(y[n] | nu, X[n,] * beta , sigma);
  }
}
"
system.time(
  robust_model_dso <- stan_model(model_code = robust_model)
)

fitlist <- list(
fit3_stan_3 = sampling(robust_model_dso, c(dat3_list, nu = 3)),
fit3_stan_6 = sampling(robust_model_dso, c(dat3_list, nu = 6)),
fit3_stan_100 = sampling(robust_model_dso, c(dat3_list, nu = 100))
)

lapply(fitlist, print, pars = c('beta','sigma'))

library(loo)
loo(extract_log_lik(fitlist[[1]]))
loo(extract_log_lik(fitlist[[2]]))
loo(extract_log_lik(fitlist[[3]]))

## Hierarchical model ----------------------------------------------------------------------
data(package='spida2')

### Generic mixed model in Stan -----------------------------------------------

# with flat priors for hyperparameters


mixed_model <- "
data {
  int N; 
  int P;
  int Q;
  int J;   // number of clusters
  matrix[N,P] X;
  matrix[N,Q] Z;
  vector[N] y;
  int id[N];
}
transformed data {
  vector[Q] zero;
  for(q in 1:Q) zero[q] = 0.0;
}
parameters {
  vector[P] gamma; // fixed effects
  matrix[J,Q] u;  // between cluster random effects -- centered
  cov_matrix[Q] G;  // G matrix: variance of u
  real<lower=0> sigma;   // within-cluster variance
}
model {
  vector[N] eta;
  vector[1] z;
  z[1] = 0.0;
  for(j in 1:J) u[j] ~ multi_normal(z, G);
  eta = X * gamma + rows_dot_product(Z, u[id,]);
  y ~ normal(eta, sigma);
}
"
mixed_model_dso2 <- stan_model(model_code = mixed_model)

head(hs)

head( Xmat <- model.matrix(~(ses+cvar(ses,school))*Sector,hs) )
head( Zmat <- model.matrix(~1 + ses, hs) )
id <- as.numeric(as.factor(hs$school))  # this works for the reason as.character of a factor does not!

dat <- list(N = nrow(Xmat),
            X = Xmat,
            Z = Zmat,
            id = id,
            J = max(id),
            N = nrow(Xmat),
            P = ncol(Xmat),
            Q = ncol(Zmat),
            y = hs$mathach)

system.time(
  mod <- sampling(mixed_model_dso , dat)
)  # takes about 120 secs
print(mod, pars = c('gamma','G','sigma'))
pairs(mod, pars = c('gamma','G','sigma','lp__'))

#
# Different parametrization for the G matrix
#
# Note that this does not work well at all without
# hyperpriors.  See (cholesky parametrization fails.pdf)

mixed_model_2 <- "
data {
  int N; 
  int P;
  int Q;
  int J;   // number of clusters
  matrix[N,P] X;
  matrix[N,Q] Z;
  vector[N] y;
  int id[N];
}
transformed data {
  vector[Q] zero;
  for(q in 1:Q) zero[q] = 0.0;
}
parameters {
  vector[P] gamma; // fixed effects
  matrix[J,Q] u;  // between cluster random effects -- centered
  cholesky_factor_corr[Q] Lg;  // correlation matrix: variance of u
  vector<lower=0>[Q]  g; // sdd of random effects
  real<lower=0> sigma;   // within-cluster variance
}
transformed parameters {
  cholesky_factor_cov[Q] L;
  cov_matrix[Q] G;

  L = diag_pre_multiply(g, Lg);
  G = L * L';
}
model {
  vector[N] eta;
  // here we can add a prior on g and sigma
  // sigma ~ cauchy(0,2.5);
  // gamma ~ 
  Lg ~ lkj_corr_cholesky(2.0);
  for(j in 1:J) u[j] ~ multi_normal_cholesky(zero, L);
  eta = X * gamma + rows_dot_product(Z, u[id,]);
  y ~ normal(eta, sigma);
}
"
mixed_model_2_dso <- stan_model(model_code = mixed_model_2)

system.time(
  mod_2 <- sampling(mixed_model_2_dso , dat)
)  # takes about 180 secs
#
# Viewing predicted model on new values
# assuming that fixed effects are called gamma
#
# with flat priors for hyperparameters
#
mod_2
pairs(mod_2, pars = c('gamma','g','Lg','sigma','lp__'))
#
# Creating the prediction data frame 
#
pred.school <- expand.grid(Sector = levels(hs$Sector), 
                           ses.mean = c(-1,0,1))
pred.school$school <- 1:nrow(pred.school)
pred <- expand.grid( school = 1:nrow(pred.school),
                     ses.dev = seq(-1.1,1.1,.1))
pred <- merge(pred, pred.school)
pred$ses <- with(pred, ses.mean + ses.dev)
Xpred <- model.matrix(~(ses+cvar(ses,school))*Sector,pred)
head(Xpred)
get_posterior_mean(mod)[1:6,5]
pred$y <- Xpred %*% get_posterior_mean(mod)[1:6,5]

library(lattice)
xyplot(y ~ ses | Sector, pred, groups = school, type = 'l')
class(e)
str(mod)
spida2:::getFix.lme




###############################  MORE ------------------------------------
- how to test multiple df hypothesis
- implement wald for stanfit objects: 
  - allow renaming of coefficients thru wald
  - partial L matrices
  - etc.
- Add easy effect displays



ses.dev = seq(-1.1,1.1,.1))
pred.school
pred <- expand.grid(school = 1:6, Sector = levels(hs$Sector), 
                    dev = seq(-1.1,1.1,.1))
pred$ses 

## END -------------------------------------------------------------------

Fit3d
fit_stan
get_fix
#' 'Fit'  the model:
fit_stan <- sampling(model_1, dat)
#' Show the model:
print(fit_stan)
summary(fit_stan)
show(fit_stan) # same as print
plot(fit_stan)
pairs(fit_stan)
#' from help for plot-methods in help(p=rstan)

### CLEAN UP FOLLOWING: DO ONE OR THE OTHER

plot(fit_stan, show_density = TRUE, ci_level = 0.5, fill_color = "purple")
plot(fit_stan, plotfun = "hist")
plot(fit_stan, plotfun = "hist", pars = c('beta_Coffee','beta_Stress'))
plot(fit_stan, plotfun = "trace", pars = c('beta_Coffee','beta_Stress'), inc_warmup = TRUE)

plot(fit_stan, plotfun = "rhat") + ggtitle("Example of adding title to plot")
stan_plot(fit_stan)
stan_trace(fit_stan)
stan_hist(fit_stan)
stan_scat(fit_stan, pars = c('beta_Coffee','beta_Stress'))
stan_diag(fit_stan)  ##################  how to interpret
stan_rhat(fit_stan)
stan_ess(fit_stan)
stan_mcse(fit_stan) ###################### how to interpret
stan_ac(fit_stan)

pairs(fit_stan, pars = c('beta_Coffee','beta_Stress','sigma'))
pairs(fit_stan)  ###################  RICH INTERPRETATION

plot(fit_stan, pars = c('beta_Coffee','beta_Stress'))
class(fit_stan)

library(loo)
log_lik1 <- extract_log_lik(fit_stan)
waic(fit_stan)
?waic
#'
#' ## Using Stan for robust fitting
#'

model_hw <- (
"
data {
  int <lower=0> N;
  vector[N] Health;
  vector[N] Weight;
  vector[N] Height;
}
parameters {
  real beta_0;
  real b_Weight;
  real b_Height;
  real <lower=0> sigma;
}
model {
  Health ~ normal( beta_0 + b_Weight * Weight + b_Height * Height, sigma );
}
"
)
system.time(
  modelo_hw <- stan_model( model_code = model_hw, verbose = TRUE )
)
# Prepare data without outliers
dat_hw <- with(hw, list(N = nrow(hw), Health = Health, Weight = Weight, Height = Height))

fit_hw <- sampling(modelo_hw, dat_hw)
fit_hw
plot(fit_hw)
pairs(fit_hw)

#'
#' ### With TYpe III outlier
#'
library(magrittr)
z <- subset(hwoutliers, Outlier %in% c('none','Type 3')) 
dat_hw3 <- with(z,list(N = nrow(z), Health = Health, Weight = Weight, Height = Height))

fit_hw3 <- sampling(modelo_hw, dat_hw3)
fit_hw3
plot(fit_hw3)
pairs(fit_hw3)
?extract
z <- extract(fit_hw3, pars = c('b_Weight','b_Height')) 
str(z)
with(z, plot(dell(b_Weight,b_Height)))
par( mar =  c(5, 5, 4, 2) + 0.1)
eqscplot( 0,0, xlim = c(-2,2),ylim= c(-2,2), type = 'n',
          xlab = list( ~beta[Weight], cex =2), ylab=list(~beta[Height], cex = 2))
abline( h = 0 )
abline( v = 0 )

points(rbind(coef(fit0)[c('Weight','Height')]), pch = 16 , col ='blue')
lines( cell(fit0, df=1 ), col = 'blue', lwd=2)

points(rbind(coef(fit3)[c('Weight','Height')]), pch = 16 , col ='magenta')
lines( cell(fit3, df=1 ), col = 'magenta', lwd=2)

with(z, lines(dell(b_Weight,b_Height, 
                   radius = qnorm(.975)),
              col = 'darkgreen', lwd = 2))

#'
#' ### Robust model
#' 
#' Using a kurtotic student t distribution with small
#' degrees of freedom we can see whether the outlier
#' is accomodated in the tail of the student t 
#' distribution.
#'

model_hw_t <- (
"
data {
  real nu;
  int <lower=0> N;
  vector[N] Health;
  vector[N] Weight;
  vector[N] Height;
  }
parameters {
  real beta_0;
  real b_Weight;
  real b_Height;
  real <lower=0> sigma;
  }
model {
  Health ~ student_t(nu, beta_0 + b_Weight * Weight + b_Height * Height, sigma );
  }
"
)
system.time(
  modelo_hw_t <- stan_model( model_code = model_hw_t, verbose = TRUE )
)
# Prepare data with outlier of type 3
dat_hw3_t <- with(dat_hw3, list(nu = 6,
                          N = length(dat_hw3$Health), 
                          Health = Health, 
                          Weight = Weight, 
                          Height = Height))

fit_hw3_t <- sampling(modelo_hw_t, dat_hw3_t)
fit_hw3_t
plot(fit_hw3_t)
plot(fit_hw3_t)

pairs(fit_hw3_t)

### robust nu = 2 ---------------------------------------
dat_hw3_t_nu2 <- with(dat_hw3, list(nu = 2,
                                N = length(dat_hw3$Health), 
                                Health = Health, 
                                Weight = Weight, 
                                Height = Height))

fit_hw3_t_nu2 <- sampling(modelo_hw_t, dat_hw3_t_nu2)
fit_hw3_t_nu2
plot(fit_hw3_t_nu2)
#' note how likelihood profiles are far from quadratic
#' but no longer bi-modal.
#' 
#' b_Weight is now significantly negative
pairs(fit_hw3_t_nu2, condition = 'lp__')
#'
#'
#' TODO: plot Confidence regions on common plot
#' 
#' TODO: how to plot non-elliptical regions



## Random intercept model -------------------------------

model_ri <- (
"
data {
   int N; //number of observations
   int K; //number of schools
   int school[N];  // school id: 1 to K
   int sector[N];  // sector: 1 catholic, 2 public
   vector[N] mathach;
   vector[N] ses;
   vector[N] ses_mean;
}
parameters {
   vector[K] u_0;  //random intercept
   vector[2] gamma_0;
   vector[2] g_ses;
   vector[2] g_ses_mean;

   real<lower=0> sigma; //within school error
   real<lower=0> tau; //between-school error

}
transformed parameters {
   vector[N] fixed;
   vector[N] eta;
   fixed = gamma_0[sector] + g_ses[sector] .* ses + g_ses_mean[sector] .* ses_mean;
   eta = fixed + u_0[school];
}
model {
   u_0 ~ normal(0, tau);
   mathach ~ normal(eta, sigma);
}
generated quantities {
   vector[N] log_lik;
   for(n in 1:N) {
          log_lik[n] = normal_lpdf(mathach[n] | eta[n], sigma);
   }
}
"
)

system.time(
  model_ri_o <- stan_model( model_code = model_ri)
)
library(spida2)
head(hs)
dat <- with(hs,
    list(
      N = nrow(hs),
      K = length(unique(school)),
      school = as.numeric(as.factor(school)), # normally this is a no-no
      sector = as.numeric(as.factor(Sector)),
      mathach = mathach,
      ses = ses,
      ses_mean = capply(ses, school, mean)
    )
)

fit <- sampling(model_ri_o, dat)
plot(fit, pars = grepv("^g|sigma|tau",names(fit)))

pairs(fit, pars = grepv("^g|sigma|tau|lp", names(fit)),
      las =1)
library(loo)
help(p=loo)
log_lik_1 <- extract_log_lik(fit)
loo_1 <- loo(log_lik_1)
print(loo_1)


#'
#' ## What does mean lpd look like?
#'

### playing with lpd ---------------------------------------

sum(rep(log(1/10)*1/10,10))
sum(rep(log(1/100)*1/100,100))
sum(rep(log(1/1000)*1/1000,1000))

dens <- 1:1000
dens <- dens/sum(dens)
sum( dens*log(dens))

dens <- rep(1,1000)
dens <- dens/sum(dens)
sum( dens*log(dens))

x <- seq(-5,5,.001)
sum(dnorm(x)*log(dnorm(x))*.001)

## Generic linear model code -------------------------------

# Generic linear model with improper uniform priors
# on betas
#
# Note that the prior is not invariant with respect to
# reparametrization of ....
#
#
generic_linmod <- ("
data {
  int<lower=0> N;             // number of data points
  int<lower=0> P;             // number of predictors (including intercept)
  matrix[N,P] X;              // predictors (including 1s for intercept)
  vector[N] y;  // outcome
}
parameters {
  vector[P] beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(X * beta, sigma);
}
generated quantities {
  // this is used only to get WAIC and LOO
  vector[N] log_lik;
  for(n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n] * beta, sigma);
  }
}
")

generic_linmod_dso <- stan_model(model_code = generic_linmod)

# Health weight example
generic_linmod_dat <- with(hw,
      list(y = Health,
           X = . <- model.matrix(~Height + Weight),
           N = nrow(.), P = ncol(.)))
fit <- sampling(generic_linmod_dso , generic_linmod_dat)
pairs(fit, pars = c('beta','sigma','lp__'))

library(loo)
help(p=loo)
log_lik <- extract_log_lik(fit)
(waic <- waic(log_lik))
summary(loo(log_lik))
####   outlier type 3 -----------------------------------
library(spida2)

hw3 <- subset(spida2::hw, Type == 3)
hw3$id <- 1:nrow(hw3)
rownames(hw3) <- hw3$id
fitlm3 <- lm(Health ~ Weight + Height, hw3)
summary(fitlm3)
plot(fit3,5)
fitlm3a <- update(fitlm3, . ~ . + I(id == 15))
summary(fitlm3a)

##### Robust model -------------------------------

generic_linmod_t <- ("
data {
  real<lower=0> nu;      // degrees of freedom for t distribution
  int<lower=0> N;             // number of data points
  int<lower=0> P;             // number of predictors (including intercept)
  matrix[N,P] X;              // predictors (including 1s for intercept)
  vector[N] y;  // outcome
}
parameters {
  vector[P] beta;
  real<lower=0> sigma;
}
model {
  y ~ student_t(nu, X * beta, sigma);
}
generated quantities {
  // this is used only to get WAIC and LOO
  vector[N] log_lik;
  for(n in 1:N) {
    log_lik[n] = student_t_lpdf(y[n] | nu, X[n] * beta, sigma);
  }
}
")

generic_linmod_t_dso <- stan_model(model_code = generic_linmod_t)

# Health weight example
generic_linmod_t_dat <- with(hw3,
                           list(nu = 6,
                             y = Health,
                                X = . <- model.matrix(~Height + Weight),
                                N = nrow(.), P = ncol(.)))
fit6 <- sampling(generic_linmod_t_dso , generic_linmod_t_dat)
pairs(fit6, pars = c('beta','sigma','lp__'))

generic_linmod_t_dat$nu <- 3
fit3 <- sampling(generic_linmod_t_dso , generic_linmod_t_dat)
pairs(fit3, pars = c('beta','sigma','lp__'))

generic_linmod_t_dat$nu <- 2
fit2 <- sampling(generic_linmod_t_dso , generic_linmod_t_dat)
pairs(fit2, pars = c('beta','sigma','lp__'))

ll_6 <- extract_log_lik(fit6)

ll_3 <- extract_log_lik(fit3)

ll_2 <- extract_log_lik(fit2)
loo_6 <- loo(ll_6)
loo_3 <- loo(ll_3)
loo_2 <- loo(ll_2)
loo_6
loo_3
loo_2
?loo
compare(loo_2,loo_3,loo_6)
library(loo)
print(fit6,pars=c('beta','sigma'))
print(fit3,pars=c('beta','sigma'))
print(fit2,pars=c('beta','sigma'))

help(p=loo)
log_lik <- extract_log_lik(fit)
(waic <- waic(log_lik))
summary(loo(log_lik))
####   outlier type 3 -----------------------------------
library(spida2)

hw3 <- subset(spida2::hw, Type == 3)
hw3$id <- 1:nrow(hw3)
rownames(hw3) <- hw3$id
fit3 <- lm(Health ~ Weight + Height, hw3)
summary(fit3)
plot(fit3,5)
fit3a <- update(fit3, . ~ . + I(id == 15))
summary(fit3a)






## END --------------------------------
))))

## PLAY -----------------------------------
?rstan:::plot.stanfit
modelString = " 
data { 
  int <lower = 0> N; 
  int y[N]; // y is a length-N vector of integers 
} 
parameters { 
  real <lower = 0, upper = 1> theta; 
} 
model { 
  theta ∼ beta(1, 1); 
  y ∼ bernoulli(theta); 
} 
" 

stanDso <- stan_model( model_code = modelString )

N <- 50
z <- 10
y <- c( rep( 1, z), rep( 0, N-z)) 
dataList <- list( y = y , N = N ) 
stanFit <- sampling( object = stanDso , 
                     data = dataList , 
                     chains = 4 , 
                     iter = 1000 , warmup = 200 , thin = 1 )
windowsFonts(Arial=windowsFont("TT Arial"))
plot(stanFit)
summary(stanFit)
show(stanFit)
trace <- traceplot(stanFit, inc_warmup =T)

trace
trace + scale_color_discrete() + theme(legend.position = "top")
trace + theme(legend.position = "top")



data { 
  … declarations … 
} 
transformed data { … declarations … statements … } 
parameters { … declarations … } 
transformed parameters { … declarations … statements … } 
model { … declarations … statements … } 
generated quantities { … declarations … statements … }


#'
#' ## Leave-one-out predictive validity
#'

install.packages('loo')
