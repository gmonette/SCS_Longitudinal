//
//  Multivariate model for VIQ and PIQ
//  Using Cholesky parametrization
//

data {
  int N;
  int J;
  matrix[N,2] iq;
  vector[N] time;
  vector[N] coma;
  int <lower=1,upper=J> id[N];
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
  
  cholesky_factor_corr[2] L;
  vector <lower=0>[2] sd;
  cholesky_factor_corr[2] L_u;
  vector <lower=0>[2] sd_u;
}
transformed parameters {
  real hrt_diff;
  real bcoma_diff;
  hrt_diff = hrt[2] - hrt[1];
  bcoma_diff = bcoma[2] - bcoma[1];
}
model {
  vector[2] eta;
  matrix[2,2] Lf;
  matrix[2,2] Lf_u;
  Lf = 
  for(j in 1:J) u[j] ~ multi_normal_cholesky(zero, Sigma_u);
  for(n in 1:N) {
    eta[1] = asymp[1] + u[id[n],1] + bcoma[1] * coma[n] + 
            init_def[1] * exp(-time[n]/(hrt[1]*ln2));
    eta[2] = asymp[2] + u[id[n],2] + bcoma[2] * coma[n] + 
            init_def[2] * exp(-time[n]/(hrt[2]*ln2));
    iq[n,] ~ multi_normal(eta, Sigma);
  }
}

