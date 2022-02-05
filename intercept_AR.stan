data { 
  int n_train;
  int n_test;
  int n_fore;
  int ar_k;
  vector[n_train] Y;
  vector[n_train] week_train;
  vector[n_test] week_test;
  vector[n_fore] week_fore;
} 
 
parameters { 
  real<lower=0> sigma; 
  vector[ar_k] ds;
} 
 
model { 
  ds ~ normal(0, 1);
  sigma ~ inv_gamma(1, 1);
  //Y[1:ar_k] ~ normal(0, sigma);
    for (i in (ar_k+1):n_train) {
      real mu_i = 0;
      for (k in 1:ar_k)
        mu_i += ds[k] * Y[i-k];
      Y[i] ~ normal(mu_i, sigma);
    }
}

generated quantities {
  real y_tilde[n_test] = rep_array(normal_rng(0, sigma), n_test);
  real y_hat[n_train]  = rep_array(normal_rng(0, sigma), n_train);
  real y_fore[n_fore]  = rep_array(normal_rng(0, sigma), n_fore);
  // autoregressive  
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
