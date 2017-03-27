#' ---
#' title: "R Stan examples: Treatment for Migraines"
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

data(migraines)
dd <- migraines
?migraines

head(dd)

length(unique(dd$id))
dd <- sortdf(dd, ~ time)
xyplot(jitter(ha) ~ time | id, dd,
       type = 'l',
       layout = c(8,9)) + layer(panel.abline(v = 0))

dd$treat <- 1*(dd$time>0)




if(TRUE) {
  mig_model <- "
     data {
       int N; 
       int J;
       int id[N];
       int ha[N];
       real time[N];
       real treat[N];
       
     }
     transformed data{
       real ln2;
       real pretreat[N];
       ln2 = log(2);
       for(n in 1:N) pretreat[n] = 1 - treat[n];
     }
     parameters {
       real<lower=0,upper=1> pre;
       real<lower=0,upper=1> long_post;
       real<lower=0,upper=1> short_post;
       real<lower=0> sigma_pre;
       real<lower=0> sigma_long_post;
       real<lower=0> sigma_short_post;
       real<lower=0,upper=1000> half_adjust_time;
       real logit_pre[J];
       real logit_long_post[J];
       real logit_short_post[J];
     }
     transformed parameters {
       
     }
     model {
       real eta[N];
       logit_pre ~  logistic(logit(pre), sigma_pre);
       logit_short_post ~  logistic(logit(short_post), sigma_short_post);
       logit_long_post ~  logistic(logit(long_post), sigma_long_post);
       eta = pretreat * logit_pre[id] +
         treat * ( logit_long_post[id] +
                      (logit_short_post[id] - logit_long_post[id])*
                     exp(- time / (half_adjust_time*ln2)));
       ha ~ bernoulli_logit(eta);
     }
"
  mig_model_dso <- stan_model(model_code = mig_model)
}


data(iq)
?iq
head(iq)
names(iq) <- tolower(names(iq))