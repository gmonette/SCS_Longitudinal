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
  y ~ normal(asymp + init_def * exp(-time/(hrt*ln2)), sigma);
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
  coma = dd$dcoma
)

mod <- sampling(asymp_model_dso, dat)
library(shinystan)
traceplot(mod)
names(mod)
pars <- grepv('^u', names(mod), invert = T)
pars
traceplot(mod, pars = pars)
pairs(mod, pars = pars)
mod_sso <- launch_shinystan(mod)

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
#' Maybe some chains start in the wilderness and never find
#' their way to the mountain because the posterior is too flat
#' 
#' Note 
#' 1. the lp peak for values of dcoma close to 0
#' 2. asymp and init_def and hrt seem highly related 
#'
#' Adding constraints to hyperparameters
#'



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
  real <lower=-100,upper=100> bcoma;
  real <lower-100,upper=100> init_def;
  vector[J] u;
  real <lower=0,upper=sigma;
  real sigma_u;
}
model {
  u ~ normal(0,sigma_u);
  y ~ normal(asymp + init_def* exp(-time/(hrt*ln2)), sigma);
}
"
