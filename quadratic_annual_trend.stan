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
  real b2;
  real s1;
  real s2;
  real c1;
  real c2;
  real<lower=0> sigma; 
  real mu;
} 
 
model { 
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  s1 ~ normal(0, 1);
  s2 ~ normal(0, 1);
  c1 ~ normal(0, 1);
  c2 ~ normal(0, 1);
  sigma ~ inv_gamma(1, 1);
  Y ~ normal(b1 * week_train + b2*week_train .* week_train + 
    s1*sin(week_train/52*1*2*pi()) + c1*cos(week_train/52*1*2*pi()) + 
    s2*sin(week_train/52*2*2*pi()) + c2*cos(week_train/52*2*2*pi()), sigma);
} 

generated quantities {
  real y_tilde[n_test] = normal_rng(b1*week_test + b2*week_test .* week_test + 
    s1*sin(week_test/52*1*2*pi()) + c1*cos(week_test/52*1*2*pi()) + 
    s2*sin(week_test/52*2*2*pi()) + c2*cos(week_test/52*2*2*pi()), sigma);
  real y_hat[n_train] = normal_rng(b1*week_train + b2*week_train .* week_train + 
    s1*sin(week_train/52*1*2*pi()) + c1*cos(week_train/52*1*2*pi()) + 
    s2*sin(week_train/52*2*2*pi()) + c2*cos(week_train/52*2*2*pi()), sigma);
  real y_fore[n_fore] = normal_rng(b1*week_fore + b2*week_fore .* week_fore + 
    s1*sin(week_fore/52*1*2*pi()) + c1*cos(week_fore/52*1*2*pi()) + 
    s2*sin(week_fore/52*2*2*pi()) + c2*cos(week_fore/52*2*2*pi()), sigma);
}
