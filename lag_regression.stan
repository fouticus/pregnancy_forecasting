data { 
  int n_lags;
  int n_train;
  int n_test;
  int n_fore;
  vector[n_train] Y;
  matrix[n_train, n_lags] X;
  matrix[n_test, n_lags] X_test;
  matrix[n_fore, n_lags] X_fore;
} 
 
parameters { 
  vector[n_lags] a;
  real<lower=0> sigma; 
} 
 
model { 
  a ~ normal(0, 1); 
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(to_vector(X * a), sigma); 
} 

generated quantities {
  real y_hat[n_train] = normal_rng(X * a, sigma);
  real y_tilde[n_test] = normal_rng(X_test * a, sigma);
  real y_fore[n_fore]  = normal_rng(X_fore * a, sigma);
}
