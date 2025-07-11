# setup ----

library(here); 
here::i_am('stats/bayesEM.R')

source(here('stats/run_ts_bayes.R'))
# library(bayesplot)


#data-poor
Suillus <- qPCRabund_EM %>% filter(PLOT == 'QUBI' & Genus == 'Suillus') %>% 
  select(PLOT, SESSION, SUBPLOT, 
         Moisture1_7d_avg, Temp1_7d_avg, 
         abund_sum_prop)
# write.csv(Suillus, here('stats/Suillus.csv'))
# bayes_Suillus <- Suillus %>% na.omit() %>% run_ts_bayes()

#data-richer
Cortinarius <- qPCRabund_EM %>% 
  filter(PLOT == 'QUBI' & Genus == 'Cortinarius') %>% 
  select(PLOT, SESSION, SUBPLOT, 
         Moisture1_7d_avg, Temp1_7d_avg, 
         abund_sum_prop)
# write.csv(Cortinarius, here('stats/Cortinarius.csv'))

#more-variance
Russula <- qPCRabund_EM %>% 
  filter(PLOT == 'QUBI' & 
           Genus == 'Russula') %>% 
  select(PLOT, SESSION, SUBPLOT, 
         Moisture1_7d_avg, Temp1_7d_avg, 
         abund_sum_prop)
# write.csv(Russula, 
#           here('stats/Russula.csv'))

figRuss <- Russula %>% 
  ggplot(aes(SESSION,
             abund_sum_prop)) +
  geom_point(color = 'gray') +
  stat_summary(fun.data = ggpubr::mean_se_) +
  theme_classic() +
  labs(y = 'Russula (abund_sum_prop)')

ggsave(plot = figRuss,
       here('figs/figRuss.png'))

bayes_Russ <- Russula %>% 
  na.omit() %>% 
  run_ts_bayes()

bayes_Russ[[1]] %>% 
  rhat() %>% 
  mcmc_rhat() %>% 
  
  save(
    file = here('figs/bayes_Russ_Rhat.png')
  )

bayes_Russ_y <- ggplot() +
  geom_line(aes(x=c(1:9), 
                y=bayes_Russ[[3]]$`50%`)) + #median
  geom_ribbon(data=bayes_Russ[[3]], 
              aes(x=c(1:9), 
                  ymin = `2.5%`, 
                  ymax = `97.5%`), 
              alpha = 0.2)

ggsave(plot = bayes_Russ_y,
       here('figs/bayes_Russ_y.png'))


bayes_Russ_beta <- ggplot() +
  geom_line(aes(x=c(1:3), 
                y=bayes_Russ[[4]]$`50%`)) + #median
  geom_ribbon(data=bayes_Russ[[4]], 
              aes(x=c(1:3), 
                  ymin = `2.5%`, 
                  ymax = `97.5%`), 
              alpha = 0.2)

ggsave(plot = bayes_Russ_beta,
       here('figs/bayes_Russ_beta.png'))

## REG GLM BY SEASON INSTEAD FOR YEAR 1


#general
# qPCRabund_EM %>% 
#   tidyr::nest(data = !Genus) %>% 
#   mutate('bayes' = data %>% 
#            purrr::modify(
#              ~ .x %>% run_ts_bayes()
#            ))