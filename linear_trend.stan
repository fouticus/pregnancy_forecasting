data { 
  int n_train;
  int n_test;
  int n_fore;
  vector[n_train] Y;
  vector[n_train] week_train;
  vector[n_test] week_test;
  vector[n_fore] week_fore;
} 
 
parameters { 
  real b1;
  real<lower=0> sigma; 
} 
 
model { 
  b1 ~ normal(0, 1);
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(b1 * week_train, sigma); 
} 

generated quantities {
  real y_tilde[n_test] = normal_rng(b1*week_test, sigma);
  real y_hat[n_train]  = normal_rng(b1*week_train, sigma);
  real y_fore[n_fore]  = normal_rng(b1*week_fore, sigma);
}
