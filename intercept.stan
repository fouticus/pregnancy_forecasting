data { 
  int n_train;
  int n_test;
  int n_fore;
  vector[n_train] Y;
} 
 
parameters { 
  real<lower=0> sigma; 
} 
 
model { 
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(0, sigma); 
} 

generated quantities {
  real y_tilde[n_test] = rep_array(normal_rng(0, sigma), n_test);
  real y_hat[n_train]  = rep_array(normal_rng(0, sigma), n_train);
  real y_fore[n_fore]  = rep_array(normal_rng(0, sigma), n_fore);
}
