
// Asymptotic recovery model
// - random intercept

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
  real init_def; // initial deficit
  vector[J] u;   // random intercept residual
  real <lower=0> sigma;   // sd within
  real <lower=0> sigma_u; // sd between
}
transformed parameters {
  real estd_reliability;
  estd_reliability = sigma_u^2 / (sigma_u^2 + sigma^2) ;
}
model {
  u ~ normal(0,sigma_u);
  y ~ normal(asymp + u[id] + bcoma * coma + 
             init_def * exp(-time/(hrt*ln2)), sigma);
}
generated quantities {
  vector[Np] y_fit;
  y_fit = asymp + bcoma * coma_p + 
             init_def * exp(-time_p/(hrt*ln2)); 
}
