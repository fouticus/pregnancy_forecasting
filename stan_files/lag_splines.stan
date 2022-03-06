data { 
  int n_lags;
  int n_splines;
  int n_train;
  int n_test;
  int n_fore;
  vector[n_train] Y;
  matrix[n_train, n_lags] X;
  matrix[n_test, n_lags] X_test;
  matrix[n_fore, n_lags] X_fore;
  matrix[n_lags, n_splines] B; 
} 
 
parameters { 
  vector[n_splines] a;
  real<lower=0> sigma; 
} 
 
model { 
  a ~ normal(0, 1); 
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(to_vector(X * B * a), sigma); 
} 

generated quantities {
  real y_hat[n_train] = normal_rng(X * B * a, sigma);
  real y_tilde[n_test] = normal_rng(X_test * B * a, sigma);
  real y_fore[n_fore] = normal_rng(X_fore * B * a, sigma);
}
