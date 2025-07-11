library(here); 
here::i_am('stats/run_ts_bayes.R')

library(rstan); 
library(dplyr); 
library(bayesplot)


run_ts_bayes <- function(tsdata) {
  
  
  
  l <- list(I = nrow(tsdata), #NEW
            N = tsdata$SUBPLOT %>% unique() %>% length(), #REPLICATION
            T = tsdata$SESSION %>% unique() %>% length(), 
            X = tsdata$Moisture1_7d_avg %>% unique(),
            # X_2 = tsdata$Temp1_7d_avg %>% unique(), #NO-DUPLICATION
            Y = tsdata$abund_sum_prop,
            N_I = tsdata$SUBPLOT, #VECTOR OF 'SUBPLOT' COL
            T_I = tsdata$SESSION, #VECTOR OF 'SESSION' COL
            S_T = rep(1:3, each = 3)
  )
  
  # model estimation----
  fit_stan <- stan(
    file = here("stats/model2beta.stan"),
    data = l,
    iter = 10000, #10k
    seed = 1
  ) #MAIN!
  
  # convergence of MCMC sampling----
  #optional
  r_hat <- rhat(fit_stan)
  mcmc_rhat(r_hat) #R^<1.1 req
  rhat(fit_stan) %>% sort(decreasing = T) %>% head(50)
  
  # extraction of MCMC samples----
  sample_stan <- rstan::extract(fit_stan)
  sample_stan %>% names()
  
  # figures----
  ## Y
  qua_y <- apply(sample_stan$y_base, 2, quantile,
               probs=c(2.5, 25, 50, 75,97.5)/100, na.rm = T) %>%
    t() %>% as.data.frame()
  
  ## beta
  qua_beta <- apply(sample_stan$beta, 2, quantile,
               probs=c(2.5, 25, 50, 75,97.5)/100, na.rm = T) %>%
    t() %>% as.data.frame()
  
  ## beta
  # qua_beta2 <- apply(sample_stan$beta_2, 2, quantile,
  #              probs=c(2.5, 25, 50, 75,97.5)/100, na.rm = T) %>%
  #   t() %>% as.data.frame()
  
  
  return(list(fit_stan, 
              r_hat, 
              qua_y, 
              qua_beta
              # qua_beta2
              ))
  
  
  
}