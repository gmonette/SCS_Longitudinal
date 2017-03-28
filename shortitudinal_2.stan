
// Shortitudinal data # 2
// Accounting for measurement error in 
// contextual variables derived from a small cluster
// - with a Gamma prior on the between cluster variance

data {
  int N;
  int J;
  int id[N];
  vector[N] x;
  vector[N] y;
  int gamma_par;
  real gamma_scale;
}
parameters {
  real b_contextual;
  real b_within;
  real alpha;
  real<lower=0> sigma;
  real<lower=0> sigma_u;
  real<lower=0> sigma_x_between;
  real<lower=0> sigma_x_within;
  real mu_x;
  vector[J] u;  // random intercept
  vector[J] xmean;
}
transformed parameters {
  real b_compositional;
  b_compositional = b_within + b_contextual;
}
model {
  sigma_u ~ gamma(gamma_par, 1/gamma_scale); // prior for sigma_u
  xmean ~ normal(mu_x, sigma_x_between);
  x ~ normal(xmean[id], sigma_x_within);
  u ~ normal(0, sigma_u);
  y ~ normal(alpha + u[id] + b_contextual * xmean[id] + b_within * x, sigma );
}
