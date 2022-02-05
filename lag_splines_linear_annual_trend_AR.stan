data {
  int n_lags;
  int n_splines;
  int n_train;
  int n_test;
  int n_fore;
  int ar_k;
  vector[n_train] Y;
  vector[n_train] week_train;
  vector[n_test] week_test;
  vector[n_fore] week_fore;
  matrix[n_train, n_lags] X;
  matrix[n_test, n_lags] X_test;
  matrix[n_fore, n_lags] X_fore;
  matrix[n_lags, n_splines] B; 
} 
 
parameters { 
  real b1;
  real s1;
  real s2;
  real c1;
  real c2;
  vector[n_splines] a;
  real<lower=0> sigma; 
  vector[ar_k] ds;
} 

transformed parameters {
  vector[n_train] mu = to_vector(X * B * a) + b1 * week_train + 
    s1*sin(week_train/52*1*2*pi()) + c1*cos(week_train/52*1*2*pi()) + 
    s2*sin(week_train/52*2*2*pi()) + c2*cos(week_train/52*2*2*pi());
}
 
model { 
  b1 ~ normal(0, 0.01);
  s1 ~ normal(0, 1);
  s2 ~ normal(0, 1);
  c1 ~ normal(0, 1);
  c2 ~ normal(0, 1);
  a  ~ normal(0, 1);
  ds ~ normal(0, 1);
  sigma ~ inv_gamma(1, 1);
  //Y[1:ar_k] ~ normal(mu[1:ar_k], sigma);
  for (i in (ar_k+1):n_train) {
    real mu_i = mu[i];
    for (k in 1:ar_k)
      mu_i += ds[k] * Y[i-k];
    Y[i] ~ normal(mu_i, sigma);
  }
}

generated quantities {
  vector [n_test] mu_test = to_vector(X_test * B * a) + b1 * week_test + 
    s1*sin(week_test/52*1*2*pi()) + c1*cos(week_test/52*1*2*pi()) + 
    s2*sin(week_test/52*2*2*pi()) + c2*cos(week_test/52*2*2*pi());
  
  vector [n_fore] mu_fore = to_vector(X_fore * B * a) + b1 * week_fore + 
    s1*sin(week_fore/52*1*2*pi()) + c1*cos(week_fore/52*1*2*pi()) + 
    s2*sin(week_fore/52*2*2*pi()) + c2*cos(week_fore/52*2*2*pi());
  
  real y_tilde[n_test] = normal_rng(mu_test, sigma);
  real y_hat[n_train] = normal_rng(mu, sigma);
  real y_fore[n_fore] = normal_rng(mu_fore, sigma);
  
  for(k in 1:ar_k){
    y_tilde[1] += Y[n_train+1-k] * ds[k];
  }
  for(i in 2:(ar_k-1)) {
    for(k in 1:(i-1))
      y_tilde[i] += y_tilde[i-k] * ds[k];
    for(k in i:ar_k)
      y_tilde[i] += Y[n_train+i-k] * ds[k];
  }
  for(i in (ar_k+1):n_test){
    for(k in 1:ar_k){
      y_tilde[i] += y_tilde[i-k] * ds[k];
    }
  }
  
  for(i in (ar_k+1):n_train){
    for(k in 1:ar_k)
      y_hat[i] += Y[i-k] * ds[k];
  }
  
  for(k in 1:ar_k){
    y_fore[1] += Y[n_train+1-k] * ds[k];
  }
  for(i in 2:(ar_k-1)) {
    for(k in 1:(i-1))
      y_fore[i] += y_fore[i-k] * ds[k];
    for(k in i:ar_k)
      y_fore[i] += Y[n_train+i-k] * ds[k];
  }
  for(i in (ar_k+1):n_fore){
    for(k in 1:ar_k){
      y_fore[i] += y_fore[i-k] * ds[k];
    }
  }
}
