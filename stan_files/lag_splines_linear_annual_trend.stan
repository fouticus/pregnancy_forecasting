data {
  int n_lags;
  int n_splines;
  int n_train;
  int n_test;
  int n_fore;
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
} 
 
model { 
  b1 ~ normal(0, 0.01);
  s1 ~ normal(0, 1);
  s2 ~ normal(0, 1);
  c1 ~ normal(0, 1);
  c2 ~ normal(0, 1);
  a ~ double_exponential(0, 0.1);
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(to_vector(X * B * a) + b1 * week_train + 
    s1*sin(week_train/52*1*2*pi()) + c1*cos(week_train/52*1*2*pi()) + 
    s2*sin(week_train/52*2*2*pi()) + c2*cos(week_train/52*2*2*pi()), sigma);
} 

generated quantities {
  real y_tilde[n_test] = normal_rng(X_test * B * a + b1 * week_test + 
    s1*sin(week_test/52*1*2*pi()) + c1*cos(week_test/52*1*2*pi()) + 
    s2*sin(week_test/52*2*2*pi()) + c2*cos(week_test/52*2*2*pi()), sigma);
  real y_hat[n_train] = normal_rng(X * B * a + b1 * week_train + 
    s1*sin(week_train/52*1*2*pi()) + c1*cos(week_train/52*1*2*pi()) + 
    s2*sin(week_train/52*2*2*pi()) + c2*cos(week_train/52*2*2*pi()), sigma);
  real y_fore[n_fore] = normal_rng(X_fore * B * a + b1 * week_fore + 
    s1*sin(week_fore/52*1*2*pi()) + c1*cos(week_fore/52*1*2*pi()) + 
    s2*sin(week_fore/52*2*2*pi()) + c2*cos(week_fore/52*2*2*pi()), sigma);
}
