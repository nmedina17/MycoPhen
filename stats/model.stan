data {
  int I;
  int N;
  int T;
  real X[T];
  real X_2[T];
  real Y[I];
  int<lower=1, upper=N> N_I[I];
  int<lower=1, upper=T> T_I[I];
}
parameters {
  real b[N,T];
  real beta[T];
  real beta_2[T];
  real<lower=0> s1;
  real<lower=0> s2;
  real<lower=0> s3;
  real<lower=0> sY;
}
transformed parameters{
  real y[N,T];
  for (n in 1:N) {
    for (t in 1:T) {
      y[n,t] = b[n,t] + beta[t] * X[t] + beta_2[t] * X_2[t]; 
      //b=qPCR change w/o Temp
    }
  }
}
model {
  for (t in 2:T){
    for (n in 1:N) {
      b[n,t] ~ normal(b[n,t-1], s1); //s1=distribution parameter
      //(avg, variance)
    }
    beta[t] ~ normal(beta[t-1], s2);
    beta_2[t] ~ normal(beta_2[t-1], s3);
  }
  for (i in 1:I){
    Y[i] ~ normal(y[N_I[i], T_I[i]], sY); //real predicted by estimated
  }
  s1 ~ normal(0.5, 0.5); //optional, good for convergence
}
generated quantities {
  real y_base[T];
  for (t in 1:T){
    y_base[t] = 0;
    for (n in 1:N){
      y_base[t] += y[n,t]/N;
    }
  }
}

