
// Asymptotic recovery model
// - random intercept
// - 'Bayesian' prior on sigma_idef 

data {
  int N;
  int J;
  vector[N] y;
  vector[N] time;
  vector[N] coma;
  int id[N];
  int Np;  // predictor values for prediction
  vector[Np] time_p;
  vector[Np] coma_p;
}
transformed data{
  real ln2;
  ln2 = log(2); // factor for half-recovery time
}
parameters {
  real hrt;    // half-recovery time
  real asymp;  // mean asymptotic recovery if dcoma = 0
  real bcoma;  // duration of coma parameter
  real init_def_mean; // initial deficit
  vector[J] u;   // random intercept residual
  vector[J] u_init_def; // random initial deficit
  real <lower=0> sigma;   // sd within
  real <lower=0> sigma_u; // sd between
  real <lower=3,upper=30> sigma_idef; // uniform prior from 3 to 30
}
transformed parameters {
  real estd_reliability;
  estd_reliability = sigma_u^2 / (sigma_u^2 + sigma^2) ;
}
model {
  // Gamma prior
  // sigma_idef ~ gamma(gamma_df, 1/gamma_scale); // commenting out the gamma prior
  // note few changes from non-random init_def
  u ~ normal(0,sigma_u);
  // add an empirical prior for u_init_def:
  u_init_def ~ normal(init_def_mean, sigma_idef);
  // add u_init_def[id] and element-wise multiplication 
  // of vectors (.*):
  y ~ normal(asymp + u[id] + bcoma * coma + 
             (init_def_mean + u_init_def[id])
                .* exp(-time/(hrt*ln2)), sigma);
}
generated quantities {
  vector[Np] y_fit;
  y_fit = asymp + bcoma * coma_p + 
             init_def_mean * exp(-time_p/(hrt*ln2)); 
}
