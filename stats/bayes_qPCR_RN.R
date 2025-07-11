# example of state-space model for time-varying coefficients--------------

# packages----
library(here); library(tidyverse); library(rstan); library(bayesplot)

# data set----
set.seed(22)
# t <- 40 # length(time) #9
# b <- rnorm(t,0,1) %>% cumsum() %>% c(.) + 20 # latent var
# beta <- c(rep(1,14), seq(1,0,by=-0.2), rep(0,20)) #coef #optional
# X <- sin(pi*seq(0,t-1)/(t-1))*20 # explanatory variable e.g. Temp
# E <- beta*X
# Y <- b + E + rnorm(t,0,2) # observation variable

# plot(Y)

# d <- data.frame(t=seq(1,t), x=X, y=Y) #df #, beta=beta,
# l <- list(T = t, X = X, Y = Y, T2_T = rep(seq(1,20),each=2))


#new_data----

qPCRfullQC_EM <- vroom::vroom(here('data/qPCRfullQC_EM.csv')) %>% 
  rename(copies_log = `Log copies final`)
plot(qPCRfullQC_EM$copies_log)
qPCRfullQC_EM_QUBI <- qPCRfullQC_EM %>% 
  filter(PLOT == 'QUBI') #%>% #& SUBPLOT == 1
  # distinct(SESSION, .keep_all = T)
plot(qPCRfullQC_EM_QUBI$copies_log)
# Y <- qPCRfullQC_EM_QUBI1$`Log copies final`
qPCRfullQC_EM_QUBI1_list <- qPCRfullQC_EM_QUBI1 %>% 
  select(SESSION, copies_log, Moisture1_7d_avg, Temp1_7d_avg) %>% 
  # mutate(t = 1:9)
  # as.data.frame() %>% 
  as.list()
l <- list(I = nrow(qPCRfullQC_EM_QUBI), #NEW
          N = qPCRfullQC_EM_QUBI$SUBPLOT %>% unique %>% length(), #REPLICATION
          T = qPCRfullQC_EM_QUBI$SESSION %>% unique() %>% length(), 
          # X = qPCRfullQC_EM_QUBI$Moisture1_7d_avg %>% unique(), 
          X = qPCRfullQC_EM_QUBI$Temp1_7d_avg %>% unique(), #NO-DUPLICATION
          # X_2 #incase...
          Y = qPCRfullQC_EM_QUBI$copies_log,
          # P = qPCRfullQC_EM_QUBI$SUBPLOT %>% unique %>% length()
          # T2_T = rep(seq(1,20),each=2)
          N_I = qPCRfullQC_EM_QUBI$SUBPLOT, #VECTOR OF 'SUBPLOT' COL
          T_I = qPCRfullQC_EM_QUBI$SESSION #VECTOR OF 'SESSION' COL
          )
# d <- as.data.frame(l); head(d)

ggplot(data=qPCRfullQC_EM_QUBI) +
  geom_point(aes(x=SESSION, copies_log, col=SUBPLOT %>% as.factor()))


#y=predictedY
#transParm=MAIN
#model=assumptions, optional if B assumed fixed / not time-varying
#restricts var estimate where it is similar to previous time

#if assume Bdiff=Bdiff-1, estimated line should be more smooth
#common for tseries analysis
#search "state-space model"

# Model----
Model <- "
  data {
    int T;
    int P;
    real Y[P,T];
    real X[T];
    real X_1[T];
  }
  parameters {
    real b[T];
    real beta[T];
    real beta_1[T];
    real<lower=0> s1;
    real<lower=0> s2;
    real<lower=0> s3;
  }
  transformed parameters{
    real y[T];
    for (t in 1:T) {
      y[t] = b[t] + beta[t] * X[t] + beta_1[t] * X_1[t];
    }
  }
  model {
    for (t in 2:T){
        b[t] ~ normal(b[t-1], s1);
        beta[t] ~ normal(beta[t-1], s2);
        beta_1[t] ~ normal(beta_1[t-1], s3);
      }
    for (t in 1:T){
        Y[t,p] ~ Normal()
      }
    Y ~ normal(y, 2);
    s1 ~ normal(1, 0.1);
    s2 ~ normal(1, 0.1);
    s3 ~ normal(1, 0.1);
    // P ~ normal()
  }
  "

# model estimation----
fit_stan <- stan(
  # model_code = Model,
  file = "model.stan",
  data = l,
  iter = 4000,
  seed = 1
) #MAIN!

# convergence of MCMC sampling----
#optional
mcmc_rhat(rhat(fit_stan)) #R^<1.1 req
rhat(fit_stan) %>% sort(decreasing = T) %>% head(50)

# extraction of MCMC samples----
sample_stan <- rstan::extract(fit_stan)
sample_stan %>% names()

# figures----
## Y
qua <- apply(sample_stan$y_base, 2, quantile,
             probs=c(2.5, 25, 50, 75,97.5)/100, na.rm = T) %>%
  t() %>% as.data.frame()

ggplot() +
  geom_point(data = qPCRfullQC_EM_QUBI, aes(x=SESSION, y=copies_log, 
                                            col = SUBPLOT %>% as.factor())) +
  geom_line(aes(x=c(1:l$T), y=qua$`50%`)) + #median
  #BayesCriticalInterval(CI)
  geom_ribbon(data=qua, aes(x=c(1:l$T), ymin = `2.5%`, ymax = `97.5%`), 
              alpha = 0.2)

## beta
qua <- apply(sample_stan$beta, 2, quantile,
             probs=c(2.5, 25, 50, 75,97.5)/100, na.rm = T) %>%
  t() %>% as.data.frame()

ggplot() +
  # geom_point(data = d, aes(x=c(1:T), y=beta)) +
  geom_line(aes(x=c(1:9), y=qua$`50%`)) + #median
  geom_ribbon(data=qua, aes(x=c(1:9), ymin = `2.5%`, ymax = `97.5%`), 
              alpha = 0.2)
#ifCI !overlap w/ 0 line, significant!

