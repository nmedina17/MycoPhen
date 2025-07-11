# setup ----


library(here); 
here::i_am('stats/statPCR.R')

# source(
#   here('data/import.R')
# )

source(
  here('analysis/commEM.R')
)
# ASVsITSrarFull_EMFfiltExploreSum

library(lubridate)
library(gamm4)
library(gratia)

library(furrr)
# future_map()
library(tictoc)

library(emmeans)
library(tibble)


# qPCR ----


cor.test(
  ~ Moisture1_7d_avg +
    Temp1_7d_avg,
  qPCRfullQC_EM
) #high=0.7
cor.test(
  ~ Moisture1_7d_avg +
    week(YYYYMMDD),
  qPCRfullQC_EM
) # sig but low=0.2 < 0.7
cor.test(
  ~ Temp1_7d_avg +
    week(YYYYMMDD),
  qPCRfullQC_EM
) # sig but low=0.2 < 0.7


## new ----

K = 7

stat_qPCR_data <- 
  qPCRfullQC_EM_SUBPLOT_unique %>% 
  mutate(
    LeafHabit = as.factor(LeafHabit),
    WEEK = week(YYYYMMDD),
    PLOT = as.factor(PLOT),
    copies_final = 
      10^`Log copies final` /1000 %>% 
      round(),
    'copies_ECM' = 
      copies_final * 0.17,
    copies_ECM = 
      copies_ECM %>% 
      round() %>% 
      as.integer(),
    copies_final =
      copies_final %>%
      as.integer(),
    
    copies_ECM_stat = 
      copies_ECM / 10,
    copies_ECM_stat =
      copies_ECM_stat %>% 
      round() %>% 
      as.integer()
  ) %>%
  droplevels() %>% 
  
  group_by(
    PLOT,
    SUBPLOT_unique,
    LeafHabit,
    PLOT_G.spp
  ) %>%

  mutate(
    CVp_med = 
      mad(copies_ECM) / 
      median(copies_ECM) * 
      100,
  )


#CVp----


library(lmerTest) #select()
# library(DHARMa) #select()


stat_qPCR_CVp <-
  stat_qPCR_data %>% 
  
  
  glmer.nb(
    formula = 
      copies_ECM_stat ~
    
      #overall fx
      # LeafHabit + 
      PLOT_G.spp +
    
      #random
      (
        1 |
          SUBPLOT_unique
      )#,
  
    # family = 'poisson' #no
    # family = 'gaussian' #no
    #glmer.nb()!
  )
  
stat_qPCR_CVp %>% 
  # DHARMa::simulateResiduals() %>% 
  # DHARMa::plotQQunif() #glmer.nb()!

  # summary()
  car::Anova()
  # anova()


##post----

library(emmeans)
# library(multcomp) #select()

stat_qPCR_CVp_post <-
  stat_qPCR_CVp %>% 
  emmeans(
    specs = 'PLOT_G.spp',
    method = 'pairwise'
  ) %>% 
  
  pairs()

# stat_qPCR_CVp_post %>%
  # emmeans::as.glht() %>% 
#   multcomp::cld(
#     Letters = letters
#     # adjust = 'tukey'
#   )

# library(multcompView)

# stat_qPCR_data %>% 
#   multcompView::multcompLetters4(
#     stat_qPCR_CVp_post
#   ) 



#full----


tic()
stat_qPCR <-
  gam(
    data = 
      stat_qPCR_data,
    #goals
    #inference
    #modelGI
    #Pedersen2019peerJ
    formula = 
      copies_ECM ~
      
      s(WEEK,
        k = K) +
      s(WEEK,
        by = LeafHabit,
        # by = PLOT,
        m = 1, #modelGI
        k = K) +
      # s(WEEK,
      #   # by = LeafHabit,
      #   by = PLOT_G.spp,
      #   m = 1, #modelGI
      #   k = K) +
      
      LeafHabit + #p=0.5
      # PLOT +
      
      s(SUBPLOT,#_unique,
        # k = K,
        bs = 're') +
      
    t2(Moisture1_7d_avg,
       Temp1_7d_avg,
       # by = LeafHabit,
       # m = 1, #modelGI
       # full = T,
       k = K) +
    # t2(Moisture1_7d_avg,
    #    Temp1_7d_avg,
    #    by = PLOT_G.spp,
    #    m = 1, #modelGI
    #    # full = T,
    #    k = K) +
      # s(Moisture1_7d_avg,
      #   k = K) +
      # s(Temp1_7d_avg,
      #   k = K),
    
    PLOT_G.spp, #p=0.5
    
    # family = 'poisson',
    # family = 'Gamma', #VtV<0def
    # family = negbin(1),
    # family = nb(), #good
    family = tw(),
    method = 'REML' #, #faster!
    # random = ~ (
    #   1 |
    #     # PLOT /
    #     # SUBPLOT
    #     SUBPLOT_unique
    # )
  ); toc()


##fx----

stat_qPCR %>% 
  gratia::appraise() #nb=0.877 #tw=1.903better
  # gratia::draw() #OK
  # summary()

##predicts----
stat_qPCR_fitted <-
  stat_qPCR_data %>% 
  left_join(
    stat_qPCR %>% 
      gratia::fitted_values()
  ) %>% 
  arrange(WEEK)
  
  #predict
  # predict(
  #   type = "response",
  #   se.fit = T
  # )
  #smooth_estimates()


##func----


get_inflection_points <- 
  function(model) {
    model %>% 
      gratia::derivatives(
        's(WEEK)',
        partial_match = T
      ) %>% 
      mutate(
        'curves' = 
          .derivative %>%
          sign() %>% 
          diff() %>% 
          c(.,NA)
      ) %>% 
      select(
        WEEK,
        LeafHabit,
        PLOT_G.spp,
        curves
      ) %>% 
      filter(curves != 0)
  }


get_emmeans_table <- 
  function(
    model, 
    factor = c(
      'LeafHabit', 
      'PLOT_G.spp'
    ), 
    contrast_method = "pairwise", 
    adjust_method = "tukey", 
    set_infer = TRUE
  ) {
    model %>% 
      emmeans(
        specs = factor
      ) %>% 
      contrast(
        method = contrast_method, 
        adjust = adjust_method
      ) %>% 
      summary(
        infer = c(
          set_infer, 
          set_infer
        )
      ) %>%
      as.data.frame() %>%
      rename_with(
        ~ gsub(" ", "_", .x)
      ) %>%   # clean column names
      as_tibble() %>%
      mutate(
        p.value = 
          p.value %>% 
          round(3)
      )
  }


##post----


# stat_qPCR_curves <- 
#   stat_qPCR %>% 
#   get_inflection_points() 

stat_qPCR_comparisons <-
  stat_qPCR %>%
  get_emmeans_table()


##brm----


# library(brms)
# bform_PCR <- 
#   bf(
#     copies_final ~ 
#       
#       #overall fx
#       LeafHabit + 
#       PLOT +
#       s(
#         WEEK,
#         k = K
#       ) +
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         k = K
#       ) +
#       
#       s(
#         WEEK,
#         by = LeafHabit,
#         k = K,
#         m = 1
#       ) +
#       s(
#         WEEK,
#         by = PLOT,
#         k = K,
#         m = 1
#       ) +
#       
#       #RhatOKw/fixed priors
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         by = LeafHabit,
#         k = K,
#         m = 1
#       ) +
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         by = PLOT,
#         k = K,
#         m = 1
#       ) +
#       
#       #random
#       (
#         1 |
#           # p |
#           SUBPLOT_unique
#       )
#     #only1plot-each
#     # (
#     #   1 |
#     #     # s | #partial-pooling
#     #     PLOT_G.spp
#     # )
#   )
# 
# 
# tic()
# brm_PCR <- 
#   brm(
#     formula = bform_PCR, 
#     data = 
#       stat_qPCR_data, 
#     family = negbinomial(),
#     
#     #fixedFX=speed!
#     prior = set_prior(
#       "normal(0, 1)", 
#       class = "b"
#     ),
#     
#     # iter = 1000,
#     # warmup = 1000,
#     # control = list(
#     #   adapt_delta = 0.8, #>0.8
#     #   max_treedepth = 10 #>10
#     # ),
#     backend = "cmdstanr",
#     #optional,recommended
#     
#     #per-chain
#     #need 'cmdstanr' backend
#     # threads = threading(3), #experimental!
#     
#     #6x=45min-16h
#     #2x=2h
#     chains = 3, #total
#     # threads*chains < cores
#     cores = 3 #total
#   ); toc()
# 
# 
# brm_PCR %>% 
#   rhat() %>%
#   bayesplot::mcmc_rhat()
# brm_PCR %>% 
#   pp_check()
# 
# conditional_smooths(
#   brm_PCR#, 
#   # effects = 
#   #   "WEEK"
#   # "Temp1_7d_avg:Moisture1_7d_avg"#, 
# )
# 
# library(tidybayes)
# stat_qPCR_data %>% 
#   tidybayes::add_epred_draws(brm_PCR) %>% #glimpse()
#   ggplot(aes(WEEK, 
#              copies_final)) +
#   geom_point(color = 'grey') +
#   stat_summary(
#     fun.data = ggpubr::median_mad
#   ) +
#   tidybayes::stat_lineribbon(
#     aes(y = .epred),
#     alpha = 0.25
#   ) +
#   scale_y_log10() 
# 
# brm_PCR %>%
#   # draws_sum_across_chains %>% 
#   parameters::model_parameters(
#     ci = 0.95, 
#     exponentiate = T, #fam
#     test = "pd" #prob.dir.
#   ) %>% 
#   
#   filter(
#     str_detect(
#       Parameter, 
#       '1$' #chain_1only
#     )
#   ) 



# future::plan(multisession)



# old ----


# stat_qPCR_curves <- 
#   
#   qPCRfullQC_EM %>% #glimpse()
#   mutate('WEEK' = week(YYYYMMDD)) %>% 
#   
#   nest('data_qPCR' = !PLOT) %>% 
#   
#   mutate(
#     'qGAM' = data_qPCR %>% 
#       purrr::modify(
#         ~ gamm4(
#           data = .x,
#           formula = 
#             round(
#               10^`Log copies final` *
#                 0.17
#             ) / 10 ~ #scale
#             s(WEEK, 
#               # bs = 'cc',
#               k = 7), #+
#           # LeafHabit, #+
#           # t2(Moisture1_7d_avg,
#           #   Temp1_7d_avg,
#           #   k = 7),
#           family = negbin(1),
#           random = ~ (
#             1 | SUBPLOT
#           )
#         )
#       ),
#     'qGAM_knots' = 
#       qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$gam %>% 
#           k.check() %>% 
#           .[,'p-value']
#       ),
#     'qGAM_est' = 
#       qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$gam %>% 
#           gratia::smooth_estimates() %>% 
#           select(
#             .estimate,
#             .se
#           ) %>% 
#           rename(
#             .se_est = .se
#           )
#       ),
#     'qGAM_derivCurves' = 
#       qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$gam %>% 
#           gratia::derivatives() %>% 
#           mutate(
#             'curves' = 
#               .derivative %>%
#               sign() %>% 
#               diff() %>% 
#               c(.,NA) %>% 
#               as.vector()
#           )
#       ),
#     'qGAM_p' = qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$gam %>%
#           summary() %>% 
#           .$s.table %>% 
#           .[4]
#       ),
#     'qGAM_edf' = qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$gam %>%
#           summary() %>% 
#           .$s.table %>% 
#           .[1]
#       ),
#     'qGAM_AIC' = qGAM %>% 
#       purrr::modify(
#         ~ .x %>% 
#           .$mer %>%
#           AIC()
#       )
#   ) %>%
#   unnest(
#     c(
#       qGAM_knots,
#       qGAM_est,
#       qGAM_p, 
#       qGAM_edf, 
#       qGAM_derivCurves,
#       qGAM_AIC
#     )
#   ) %>% 
#   
#   filter(curves != 0)
