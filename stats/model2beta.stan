//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  //int<lower=0> N;
  //vector[N] y;
  int I;
  int N;
  int T;
  int S_T[T]; //=1,1,1,2,2,2,3,3,3
  real X[T];
  // real X_2[T];
  real Y[I];
  int<lower=1, upper=N> N_I[I];
  int<lower=1, upper=T> T_I[I];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  //real<lower=0> sigma;
  real b[N,T];
  real beta[3];
  // real beta_2[T];
  real<lower=0> s1;
  real<lower=0> s2;
  real<lower=0> s3;
  real<lower=0> sY;
}

transformed parameters{
  real y[N,T];
  for (n in 1:N) {
    for (t in 1:T) {
      y[n,t] = b[n,t] + beta[S_T[t]] * X[t]; // + beta_2[t] * X_2[t]; 
      //b=qPCR change w/o Temp
    }
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //y ~ normal(mu, sigma);
  for (t in 2:T){
    for (n in 1:N) {
      b[n,t] ~ normal(b[n,t-1], s1); //s1=distribution parameter
      //(avg, variance)
    }
    // beta[t] ~ normal(beta[t-1], s2);
    // beta_2[t] ~ normal(beta_2[t-1], s3);
  }
  for (i in 2:3) {
    beta[i] ~ normal(beta[i-1], s2); // ~ LAG time
  }
  for (i in 1:I){
    Y[i] ~ normal(y[N_I[i], T_I[i]], sY); //real predicted by estimated
  }
  s1 ~ normal(0.5, 0.5); //optional, good for convergence
  //s2 ~ normal(0.5, 0.5);
  //s3 ~ normal(0.5, 0.5);
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

