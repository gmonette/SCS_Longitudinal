
// asymp_model

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
