#' ---
#' title: "R Stan examples: IQ recovery after TBI"
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
#+ knitr_setup, include=FALSE
knitr::opts_knit$set()
knitr::opts_chunk$get(cache = TRUE, eval = FALSE)
#'
#' 
TODO ============= Add random deficit
#' 
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
windowsFonts(Arial=windowsFont("TT Arial")) 

library(spida2)
library(magrittr, pos = 1000) # so it won't mask 'extract' in rstan
library(car)
library(lattice)
library(latticeExtra)

data(iq)
?iq
head(iq)
names(iq) <- tolower(names(iq))

dd <-iq

(p <- xyplot(piq ~ dayspc | sex, dd, groups = id, type = 'b'))
update(p, xlim = c(0,4000))
ids <- numeric(0)
# can repeat:
trellis.focus()
ids <- c(ids, panel.identify(labels=dd$id))
# end
trellis.unfocus()
ids
iq[ids,] %>% sortdf(~dcoma+dayspc)
# id = 2600 retested 4 days apart
# Create a long file wrt iq
dd$iq__verbal <- dd$viq
dd$iq__perf <- dd$piq
dl <- tolong(dd, sep = "__", idvar = 'row', timevar = 'test')
head(dl)
library(p3d)
Init3d()
dd$dcoma.cat <- cut(dd$dcoma, c(-1,2,5,10,20,50,Inf))
Plot3d( viq ~ piq + log(dayspc) |
          dcoma.cat, dd, groups = id,
        col = heat.colors(6))
Plot3d( viq ~ log(dcoma+2) + log(dayspc) |
          dcoma.cat, dd, groups = id,
        col = heat.colors(6))
Plot3d( piq ~ log(dcoma+2) + log(dayspc) |
          dcoma.cat, dd, groups = id,
        col = heat.colors(12)[1:6])
fg()
Id3d()

asymp_model <- "
data {
  int N;
  int J;
  vector[N] y;
  vector[N] time;
  vector[N] coma;
  int id[N];
}
transformed data{
  real ln2;
  ln2 = log(2);
}
parameters {
  real hrt;
  real asymp;
  real bcoma;
  real init_def;
  vector[J] u;
  real <lower=0> sigma;
  real <lower=0> sigma_u;
}
model {
  u ~ normal(0,sigma_u);
  y ~ normal(asymp + u[id] + bcoma * coma + init_def * exp(-time/(hrt*ln2)), sigma);
}
"

system.time(
asymp_model_dso <- stan_model(model_code = asymp_model,
                              model_name = 'asymptotic model')
)

names(dd)
dat <- list(
  N = nrow(dd),
  id = nid <- as.numeric(as.factor(dd$id)),
  J = max(nid),
  y = dd$piq,
  time = dd$dayspc,
  coma = sqrt(dd$dcoma)
)

mod <- sampling(asymp_model_dso, dat, chains = 6, iter = 2000, seed = 789723)

traceplot(mod)
library(shinystan)
names(mod)
pars <- grepv('^u', names(mod), invert = T)
pars
traceplot(mod, pars = pars)
pairs(mod, pars = pars)
mod_sso <- launch_shinystan(mod)


# can do most of this in shiny:
mod_df <- as.data.frame(extract(mod, permuted = F, pars = pars))
head(mod_df)
dim(mod_df)
names(mod_df)
mod_dl <- tolong(mod_df, sep = '.', reverse = T, timevar = 'chain')
head(mod_dl)
library(p3d)
Init3d()
Plot3d(lp__ ~ sigma + sigma_u | chain, mod_dl)
Axes3d()
Plot3d(lp__ ~ sigma + sigma_u | chain, mod_dl, groups = chain)
tab(mod_dl, ~ time)
Plot3d(hrt ~ asymp + init_def | chain, mod_dl)
Axes3d()
#'
#'  
#'  Test square root for dcoma
#'
asymp_model_coma_pow <- "
data {
int N;
int J;
vector[N] y;
vector[N] time;
vector[N] coma;
int id[N];
}
transformed data{
vector[N] logcoma;
real ln2;
logcoma = log(coma);
ln2 = log(2);
}
parameters {
real hrt;

real asymp;
real bcoma;
real <lower=0.1,upper=2> power; 
real init_def;
real <lower=0> sigma;
real <lower=0> sigma_u;
vector[J] u;
}
model {
u ~ normal(0,sigma_u);
y ~ normal(asymp + u[id] + bcoma * exp(power*logcoma) + init_def * exp(-time/(hrt*ln2)), sigma);
}
"

system.time(
  asymp_model_coma_pow_dso <- stan_model(model_code = asymp_model_coma_pow,
                                model_name = 'asymptotic model: power on coma')
)

names(dd)
dat <- list(
  N = nrow(dd),
  id = nid <- as.numeric(as.factor(dd$id)),
  J = max(nid),
  y = dd$piq,
  time = dd$dayspc,
  coma = sqrt(dd$dcoma+1)
)

mod_coma_pow <- sampling(asymp_model_coma_pow_dso, dat, chains = 4, iter = 2000, seed = 789723)

pars <- grepv('^u', names(mod_coma_pow), invert = T)
traceplot(mod_coma_pow, pars = pars)
pairs(mod_coma_pow, pars = pars)

#'
#'
#'  Let's try a different parametrization from the Box-Cox tranform
#'  
#'  
#'




library(shinystan)
names(mod)
pars
traceplot(mod, pars = pars)
pairs(mod, pars = pars)
mod_sso <- launch_shinystan(mod)

#'
#'
#' Maybe some chains start in the wilderness and never find
#' their way to the mountain because the posterior is too flat
#' 
#' Note 
#' 1. the lp peak for values of dcoma close to 0
#' 2. asymp and init_def and hrt seem highly related 
#'
#' Let's try plausible initial values 
#'
names(mod)
inits <- rep(0, length(names(mod)))
names(inits) <- names(mod)
inits[c('sigma','sigma_u')] <- c(15,5)
inits['hrt'] <-  360
inits['asymp'] <- 100
inits['init_def'] <- -20
inits <- inits[-grep("^lp__$", names(inits))]    # removing 'lp__' which isn't a parameter
inits <- lapply(1:6,function(x) as.list(inits))
str(inits)
system.time(
mod2 <- sampling(asymp_model_dso, dat, chains = 6, iter = 2000, seed = 789723,
                init = inits)
)

traceplot(mod2, pars = pars)
pairs(mod2, pars = pars)

#'
#' Using initial values only helped a bit. 
#'
#' There's very high collinearity between b_asymp and b_init_def which
#' might be related to b_hrt: Show absolute collinearity between init_def and asymp
#' 
#' We are spending a lot of time in the tails of dist
#' 
#' Try 3 things:
#' 
#' 1. put origin at 1 year so it's in the data and reduce collinearity between 
#'    init_def and asymp
#' 2. change dayspc to monthspc
#' 3. use informative priors
#'  


asymp_model_2 <- "
data {
  int N;
  int J;
  vector[N] y;
  vector[N] time;
  vector[N] coma;
  int id[N];
}
transformed data{
  real ln2;
  ln2 = log(2);
}
parameters {
  real hrt;
  real asymp;
  real bcoma;
  real init_def;
  vector[J] u;
  real <lower=0> sigma;
  real <lower=0> sigma_u;
}
model {
  u ~ normal(0,sigma_u);
  y ~ normal(asymp +uuuuu bcoma * coma + init_def * exp(-time/(hrt*ln2)), sigma);
}
"



mod2_sso <- launch_shinystan(mod2) # have a look at three variables



asymp_model <- "
data {
  int N;
  int J;
  vector[N] y;
  vector[N] time;
  vector[N] coma;
  int id[N];
}
transformed data{
  real ln2;
  ln2 = log(2);
}
parameters {
  real <lower=1,upper=10000> hrt;
  real <lower=0,upper=200> asymp;
  // real <lower=-100,upper=100> bcoma;
  real <lower-100,upper=100> init_def;
  vector[J] u;
  real <lower=0,upper=sigma;
  real sigma_u;
}
model {
  u ~ normal(0,sigma_u);
  y ~ normal(asymp +uuuuu init_def* exp(-time/(hrt*ln2)), sigma);
}
"


## viq and piq ------------------------------------------------------------------

mult_model <- "
data {
  int N;
  int J;
  matrix[N,2] iq;
  vector[N] time;
  vector[N] tcoma;
  int id[N];
  }
transformed data{
  real ln2;
  vector[2] zero;
  ln2 = log(2);
  for ( i in 1:2) zero[i] = 0.0;
}
parameters {
  vector <lower=1,upper=10000>[2] hrt;
  vector <lower=0,upper=200>[2] asymp;
  vector <lower=-100,upper=100>[2] init_def;
  vector [2] bcoma;
  vector[2] u[J];
  cov_matrix[2] Sigma;
  cov_matrix[2] Sigma_u;
}
model {
 vector[2] eta[N];
 for(j in 1:J) u[j] ~ multi_normal(zero, Sigma_u);
 for(n in 1:N) {
    eta[n,1] =   asymp[1] + u[id[n],1] + bcoma[1] * tcoma[n] + init_def[1] * exp(-time[n]/(hrt[1]*ln2));
    eta[n,2] =   asymp[2] + u[id[n],2] + bcoma[2] * tcoma[n] + init_def[2] * exp(-time[n]/(hrt[2]*ln2));
    iq[n,] ~ multi_normal(eta[n], Sigma);
 }
}
"

mult_dso <- stan_model(model_code = mult_model)


names(dd)
dat <- list(
  N = nrow(dd),
  id = nid <- as.numeric(as.factor(dd$id)),
  J = max(nid),
  iq = cbind(dd$piq,dd$viq),
  time = dd$dayspc,
  tcoma = sqrt(dd$dcoma)
)

mult_mod <- sampling( mult_dso, dat)

# some numerical problems with covariances

pars <- grepv('^u' ,names(mult_mod), invert = T)
print(mult_mod, pars = pars)
traceplot(mult_mod, pars = pars)
pairs(mult_mod, pars = pars)

## multivariate experiment ---------------------------

zd <- data.frame( x = rnorm(100))
zd$y1 <- 3*zd$x + rnorm(100)
zd$y2 <- zd$y1 -2 *zd$x + rnorm(100)
zd <- with(zd, list(N = 100, x = x, y = cbind(y1,y2)))



zmod <- '
data{
  int N;
  vector[N] x;
  matrix[N,2] y;
}
parameters {
 vector[2] mu;
 vector[2] beta;
 cov_matrix[2] Sigma;
}
model {
 // matrix[N,2] eta;
 // for(n in 1:N) y[n,] ~ multi_normal(mu + beta * x[n], Sigma);
 y ~ multi_normal(mu + beta .* x, Sigma);
}
'

zdso <- stan_model(model_code = zmod)
zsam <- sampling(zdso, zd)
traceplot(zsam)
pairs(zsam)
