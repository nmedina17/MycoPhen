# setup ----


library(here); 
here::i_am('stats/statTax.R')

# source(
#   here('data/import.R'))

# source(
#   here('analysis/commEM.R')
# )
source(
  here('stats/statET.R')
)
# ASVsITSrarFull_EMFfiltExploreSum

library(lubridate)
library(gamm4)
library(gratia)

library(furrr)
# future_map()
library(tictoc)


# tax ----


ASVsITSrarFull_EMFfiltSum_focal <- 
  
  ASVsITSrarFull_EMFfiltSum %>%
  
  filter(
    Genus %in% 
      topEMFgenera |
      Genus %in% 
      choiceEMFgenera
  ) %>% 
  
  distinct() %>% #manyRepeats--faster!
  
  left_join(keyPlots) %>%
  mutate(
    SUBPLOT = as.factor(SUBPLOT),
    SUBPLOT_unique = as.factor(SUBPLOT_unique),
    'WEEK' = week(YYYYMMDD),
    Genus = as.factor(Genus),
    LeafHabit = 
      as.factor(LeafHabit),
    PLOT_G.spp = 
      as.factor(PLOT_G.spp),
  ) %>%
  ungroup()


### GAMM ----

# 
# #long = 20min...
# Sys.time()
# future::plan(multisession);
K <- 7

tic() #tw=15min #nb=10/8min
gam_tax <-

  # future({

  # ASVsITSrarFull_EMFfiltSum_topTen %>%
  ASVsITSrarFull_EMFfiltSum_focal %>%
  # ASVsITSrarFull_EMFfiltSum_mids %>% 

    left_join(keyPlots) %>%

    mutate(
      SUBPLOT =
        as.factor(SUBPLOT),
      SUBPLOT_unique =
        as.factor(SUBPLOT_unique),
      'WEEK' = week(YYYYMMDD),
      LeafHabit = as.factor(LeafHabit),
      Genus = as.factor(Genus)
  ) %>%
  ungroup() %>%
  
  # leave0s4stats
  # filter(
  #   !all(
  #     abund_sum == 0
  #   )
  # ) %>% 

    bam(
      data = .,
      formula =
        abund_sum + 1 ~

        # s(WEEK,
        #   k = K) +
        s(WEEK,
          # m = 1,
          by = Genus,
          k = K) +
        # s(WEEK,
        #   m = 1,
        #   by = LeafHabit,
        #   k = K) +
        # s(WEEK,
        #   m = 1,
        #   by = PLOT_G.spp,
        #   k = K) +

        LeafHabit +
        # PLOT_G.spp +
        Genus +
        # Genus : LeafHabit +
        # Genus : PLOT_G.spp + #long

        t2(
          Temp1_7d_avg,
          Moisture1_7d_avg,
          k = K
        ) +
        # t2(
        #   Temp1_7d_avg,
        #   Moisture1_7d_avg,
        #   by = Genus,
        #   m = 1,
        #   k = K
        # ) +
        # t2(
        #   Temp1_7d_avg,
        #   Moisture1_7d_avg,
        #   by = LeafHabit,
        #   m = 1,
        #   k = K
        # ) +
        # t2(
        #   Temp1_7d_avg,
        #   Moisture1_7d_avg,
        #   by = PLOT_G.spp,
        #   m = 1,
        #   k = K
        # ) +

        s(
          SUBPLOT,#_unique,
          bs = 're'
        )
      ,
          # random = ~ (
          #   1 | SUBPLOT_unique
          # ),
          # select = T,
          method = 'fREML',
          # control = glmerControl(mc.cores = 4),
          # family = 'poisson'
          # family = negbin(1)
          # family = Tweedie(1.95) #no
          family = nb()
        #tw=1.9?
      #nb=0.203
    ); toc()

# });
#
# # plan(sequential)
#
# gam_tax %>%
#   # future::resolved()
gam_tax %>%
#   # future::value() %>%
#   .$gam %>%
  gratia::appraise()
#   # gam.check() #!OK -- opt for separate GAMMs?
#   car::Anova()

gam_tax %>%
  summary()
#tw2signif...
#nb=?


# saveRDS(
#   gam_tax,
#   here('stats/gam_tax.rds')
# )


gam_tax <- readRDS(
  here('stats/gam_tax.rds')
)


# CODE BELOW IS LEGACY


# tic() #5h
# bam_tax <- 
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   left_join(keyPlots) %>%
#   arrange(
#     Genus,
#     PLOT_G.spp,
#     SUBPLOT_unique
#   ) %>% 
#   mutate(
#     SUBPLOT = as.factor(SUBPLOT),
#     LeafHabit = as.factor(LeafHabit),
#     # SUBPLOT_unique = as.factor(SUBPLOT_unique),
#     'WEEK' = week(YYYYMMDD),
#     Genus = as.factor(Genus),
#     'abund_sum_lag1' = 
#       lag(abund_sum,
#           n = 1)
#   ) %>%
#   ungroup() %>%
#   
#   bam(
#     formula = 
#       abund_sum ~ 
#       
#       s(WEEK,
#         k = K) +
#       s(WEEK,
#         m = 1,
#         by = Genus,
#         k = K) +
#       s(WEEK,
#         m = 1,
#         by = LeafHabit,
#         k = K) +
#       s(WEEK,
#         m = 1,
#         by = PLOT_G.spp,
#         k = K) +
#       
#       LeafHabit +
#       PLOT_G.spp +
#       Genus +
#       
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         k = K
#       ) +
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         by = Genus,
#         m = 1,
#         k = K
#       ) +
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         by = LeafHabit,
#         m = 1,
#         k = K
#       ) +
#       t2(
#         Temp1_7d_avg,
#         Moisture1_7d_avg,
#         by = PLOT_G.spp,
#         m = 1,
#         k = K
#       ) +
#       
#       s(
#         SUBPLOT_unique,
#         bs = 're'
#       )
#     ,
#     # select = T,
#     # rho = 0.95,
#     # correlation = corAR1(
#     #   form = ~ WEEK | 
#     #     SUBPLOT_unique
#     # ),
#     # method = 'REML',
#     # family = 'poisson'
#     # family = negbin(1)
#     family = tw()
#   ); toc()


#needed!
# saveRDS(
#   bam_tax,
#   here('stats/bam_tax.rds')
# )

# bam_tax <- readRDS(
#   here('stats/bam_tax.rds')
# )


# bam_tax %>%
#   # gratia::appraise()
#   # gratia::concrvity() #mostOK
# # residuals %>% 
# # pacf(lag.max = 9) #%>% #t-1 #lag=t-6
# # anova.gam()
# # gratia::draw()
#   summary()


# gam_tax_each <- gam_tax %>% 
#   nest('Gdata' = !Genus) %>% #.$Gdata %>% .[[1]]
#   mutate(
#     'gGAM' = Gdata %>%
#       furrr::future_imap(
#         ~ gamm4::gamm4(
#           data = .x,
#           family = 'poisson', #!binomial
#           # family = negbin(1),
#           random = 
#             ~ (
#               1 |
#                 SUBPLOT_unique
#             ),
#           formula =
#             # abund_sum_prop ~
#             abund_sum ~
#             s(WEEK,
#               k = 7) +
#             LeafHabit
#           # PLOT_G.spp
#         ) %>% 
#           .$gam
#       ),
#     'gGAM_knots' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           k.check() %>% 
#           .[,'p-value']
#       ),
#     'gGAM_R2' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           summary() %>% 
#           .$r.sq
#       ),
#     'gGAM_derivCurves' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           gratia::derivatives() %>% 
#           mutate(
#             'curves' = 
#               .derivative %>%
#               sign() %>% 
#               diff() %>% 
#               c(.,NA)
#           )
#       ),
#     'gGAM_p' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           summary() %>%
#           .$s.table %>% 
#           .[4]
#       ),
#     'gGAM_edf' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           summary() %>%
#           .$s.table %>% 
#           .[1]
#       )
#   ) %>% 
#   unnest(c(
#     gGAM_p,
#     gGAM_edf,
#     gGAM_knots,
#     gGAM_R2
#   ))
# 
# gam_tax
# 
# gam_tax %>%
#   unnest(gGAM_derivCurves) %>% 
#   filter(curves != 0) #; #Sys.time()
# 
# 
# gam_tax$gGAM[[1]] %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()
# gam_tax$gGAM[[2]] %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()
# 
# gam_tax$gGAM[[3]] %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()
# 
# gam_tax$gGAM[[4]] %>% 
#   # gam.check() # OK?
#   # broom::tidy()
#   summary()
# 
# gam_tax$gGAM[[5]] %>% 
#   # gam.check() # OK better
#   # broom::tidy()
#   summary()
# gam_tax$gGAM[[6]] %>% 
#   # gam.check() # OK better
#   # broom::tidy()
#   summary()
# gam_tax$gGAM[[7]] %>% 
#   # gam.check() # OK maybe
#   # broom::tidy()
#   summary()
# gam_tax$gGAM[[8]] %>% 
#   # gam.check() # OK better
#   # broom::tidy()
#   summary()
# gam_tax$gGAM[[9]] %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()
# 
# gam_tax$gGAM[[10]] %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()


ASVsITSrarFull_EMFfiltExploreSum


run_bam_structure <- 
  function(
    data,
    response,
    # by_factor,
    # by_factor2,
    family_used
  ) {
    library(mgcv)
    
    response <-
      substitute(response)
    formula_string <-
      paste(
        deparse(response),
        "~
        
        s(WEEK,
          k = 7
        ) +
        s(WEEK,
          by = LeafHabit,
          m = 1,
          k = 7
        ) +
        s(WEEK,
          by = PLOT_G.spp,
          m = 1,
          k = 7
        ) +
        
        s(SUBPLOT_unique,
          bs = 're') + 
          
        te(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          k = 7
        ) +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          by = LeafHabit,
          m = 1,
          full = F,
          k = 7
        ) +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          by = PLOT_G.spp,
          m = 1,
          full = F,
          k = 7
        ) + 
        
        LeafHabit +
        PLOT_G.spp"#,
        # deparse(by_factor)
      )
    formula_used <- 
      formula_string %>% 
      as.formula()
    
    model <- data %>% 
      bam(
        formula = 
          formula_used,
        method = 'fREML',
        family = 
          family_used,
        nthreads = 4
        # discrete = T #!nested
      )
    
    
    return(model)
  }


### separate ----


#### Russ ----


# gamm4_tax_week_Russula_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[1]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(
#     SUBPLOT = as.factor(SUBPLOT),
#     SUBPLOT_unique = as.factor(SUBPLOT_unique),
#     'WEEK' = week(YYYYMMDD),
#     LeafHabit = as.factor(LeafHabit),
#     Genus = as.factor(Genus)
#     # 'abund_sum_lag1' = 
#     #   lag(abund_sum,
#     #       n = 1)
#   ) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT_G.spp,
#             k = 7) +
#           # s(abund_sum_lag1,
#           #   k = 7) +
#           LeafHabit
#           # PLOT_G.spp
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(1)
#   ) #%>%
# 
# 
# gamm4_tax_week_Russula_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # resid() %>% 
#   # pacf(lag.max = 9) #t-1=!needed
#   # gratia::appraise() #poisson
#   # k.check()
#   # anova.gam()
#   summary()


Russula <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[1]
  )


# tic() #10min
# bam_tax_Russula <- 
#   Russula %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()

# saveRDS(
#   bam_tax_Russula,
#   here(
#     'stats/bam_tax_Russula.rds'
#   )
# )
bam_tax_Russula <- 
  readRDS(
  here(
    'stats/bam_tax_Russula.rds'
  )
)


bam_tax_Russula %>%
  # gratia::appraise()
  # anova.gam()
  summary()
  

bam_tax_Russula_curves <- 
  bam_tax_Russula %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Russula'
  )
  
bam_tax_Russula_comparisons <-
  bam_tax_Russula %>% 
  get_emmeans_table()


#### Inocybe ----


# gamm4_tax_week_Inocybe_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[2]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT,
#             k = 7) +
#           # PLOT
#         LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(1)
#         # family = Tweedie(1.95) ?
#   ) #%>%
# 
# 
# gamm4_tax_week_Inocybe_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # gratia::appraise() #right-skewOK=poisson
#   # k.check()
#   # anova.gam()
#   summary()


Inocybe <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[2]
  )


# tic() #5min
# bam_tax_Inocybe <-
#   Inocybe %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()


# saveRDS(
#   bam_tax_Inocybe,
#   here(
#     'stats/bam_tax_Inocybe.rds'
#   )
# )
bam_tax_Inocybe <- 
  readRDS(
  here(
    'stats/bam_tax_Inocybe.rds'
  )
)


bam_tax_Inocybe %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Inocybe_curves <- 
  bam_tax_Inocybe %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Inocybe'
  )

bam_tax_Inocybe_comparisons <-
  bam_tax_Inocybe %>% 
  get_emmeans_table()


#### Tuber ----


# gamm4_tax_week_Tuber_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[3]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT,
#             k = 7) +
#           # PLOT
#         LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(1)
#         # family = Tweedie(1.95) #?
#   ) #%>%
# 
# 
# gamm4_tax_week_Tuber_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # gratia::appraise() #right-skewOK=poisson
#   # k.check()
#   # anova.gam()
#   summary()


Tuber <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[3]
  )


# tic() #5min
# bam_tax_Tuber <- 
#   Tuber %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()

# saveRDS(
#   bam_tax_Tuber,
#   here(
#     'stats/bam_tax_Tuber.rds'
#   )
# )
bam_tax_Tuber <- 
  readRDS(
  here(
    'stats/bam_tax_Tuber.rds'
  )
)


bam_tax_Tuber %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Tuber_curves <- 
  bam_tax_Tuber %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Tuber'
  )

bam_tax_Tuber_comparisons <-
  bam_tax_Tuber %>% 
  get_emmeans_table()


#### Tomen ----


# gamm4_tax_week_Tomentella_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[4]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT,
#             k = 7) +
#           # PLOT
#         LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(1)
#         # family = Tweedie(1.95) ?
#   ) #%>%
# 
# 
# gamm4_tax_week_Tomentella_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # gratia::appraise() #right-skewOK=poisson
#   # k.check()
#   # anova.gam()
#   summary()


Tomentella <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[4]
  )


# tic() #5min
# bam_tax_Tomentella <- 
#   Tomentella %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()
# 
# 
# saveRDS(
#   bam_tax_Tomentella,
#   here(
#     'stats/bam_tax_Tomentella.rds'
#   )
# )
bam_tax_Tomentella <- 
  readRDS(
    here(
      'stats/bam_tax_Tomentella.rds'
    )
)


bam_tax_Tomentella %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Tomentella_curves <- 
  bam_tax_Tomentella %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Tomentella'
  )

bam_tax_Tomentella_comparisons <-
  bam_tax_Tomentella %>% 
  get_emmeans_table()


#### Amphi ----


# gamm4_tax_week_Amphinema_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[5]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT,
#             k = 7) +
#           # PLOT
#         LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(0.1) ?
#         # family = Tweedie(1.95) #?
#   ) #%>%
# 
# 
# gamm4_tax_week_Amphinema_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # gratia::appraise() #OK
#   # k.check()
#   # anova.gam()
#   summary()


Amphinema <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[5]
  )


# tic() #90min
# bam_tax_Amphinema <- 
#   Amphinema %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()

# saveRDS(
#   bam_tax_Amphinema,
#   here(
#     'stats/bam_tax_Amphinema.rds'
#   )
# )

bam_tax_Amphinema <- 
  readRDS(
    here(
      'stats/bam_tax_Amphinema.rds'
    )
)


bam_tax_Amphinema %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Amphinema_curves <- 
  bam_tax_Amphinema %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Amphinema'
  )

bam_tax_Amphinema_comparisons <-
  bam_tax_Amphinema %>% 
  get_emmeans_table()


#### Wilco ----


# gamm4_tax_week_Wilcoxina_leaf <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[6]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             # by = PLOT,
#             k = 7) +
#           # PLOT
#         LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         family = 'poisson'
#         # family = negbin(0.1) 
#         # family = Tweedie(1.95) 
#   ) #%>%
# 
# 
# gamm4_tax_week_Wilcoxina_leaf %>% 
#   # .$mer %>% AIC()
#   .$gam %>%
#   # gratia::appraise() #OK
#   # k.check()
#   # anova.gam()
#   summary()


Wilcoxina <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[6]
  )


# tic() #5min
# bam_tax_Wilcoxina <- 
#   Wilcoxina %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()
# 
# saveRDS(
#   bam_tax_Wilcoxina,
#   here(
#     'stats/bam_tax_Wilcoxina.rds'
#   )
# )
bam_tax_Wilcoxina <- 
  readRDS(
    here(
      'stats/bam_tax_Wilcoxina.rds'
    )
)


bam_tax_Wilcoxina %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Wilcoxina_curves <- 
  bam_tax_Wilcoxina %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Wilcoxina'
  )

bam_tax_Wilcoxina_comparisons <-
  bam_tax_Wilcoxina %>% 
  get_emmeans_table()


#### Elaph ----


# gamm4_tax_Elaphomyces <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[7]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             by = LeafHabit,
#             k = 7) +
#           LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         # family = 'poisson'
#         # family = negbin(1) #maybe
#         family = Tweedie(1.9) #maybe
#   ) #%>%
# gamm4_tax_Elaphomyces %>% 
#   .$gam %>%
#   gratia::appraise() #nb/tw
# # anova.gam()
# 
# 
# gam_tax_week_Elaphomyces_leaf <-
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   filter(Genus == topEMFgenera[7]) %>% 
#   left_join(keyPlots) %>%
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gam(data = .,
#       formula =
#         abund_sum ~
#         s(WEEK,
#           by = LeafHabit,
#           # by = PLOT,
#           k = 7) +
#         s(SUBPLOT_unique,
#           bs = 're',
#           k = 7) +
#         # PLOT
#       LeafHabit
#       ,
#       method = 'REML',
#       # family = 'poisson'
#       # family = negbin(1) #less
#       family = tw() #this
#   ) #%>%
# 
# 
# gam_tax_week_Elaphomyces_leaf %>% 
#   # AIC()
#   # gratia::appraise() #tw
#   # k.check()
#   # anova.gam()
#   summary()
# 
# AIC(gamm4_tax_Elaphomyces$mer)
# # AIC(gam_tax_Elaphomyces) #better


Elaphomyces <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[7]
  )


# tic() #10min
# bam_tax_Elaphomyces <- 
#   Elaphomyces %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()
# 
# saveRDS(
#   bam_tax_Elaphomyces,
#   here(
#     'stats/bam_tax_Elaphomyces.rds'
#   )
# )
bam_tax_Elaphomyces <- 
  readRDS(
    here(
      'stats/bam_tax_Elaphomyces.rds'
    )
)


bam_tax_Elaphomyces %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Elaphomyces_curves <- 
  bam_tax_Elaphomyces %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Elaphomyces'
  )

bam_tax_Elaphomyces_comparisons <-
  bam_tax_Elaphomyces %>% 
  get_emmeans_table()


#### Seba ----


# gamm4_tax_Sebacina <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[8]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             k = 7) +
#           LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         # family = 'poisson' #no
#         # family = negbin(1) #no
#         family = Tweedie(1.9) #zeros...
#   ) #%>%
# gamm4_tax_Sebacina %>% 
#   .$gam %>%
#   gratia::appraise()
# 
# 
# gam_tax_week_Sebacina_leaf <-
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   filter(Genus == topEMFgenera[8]) %>% 
#   left_join(keyPlots) %>%
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gam(data = .,
#       formula =
#         abund_sum ~
#         s(WEEK,
#           by = LeafHabit,
#           # by = PLOT,
#           k = 7) +
#         s(SUBPLOT_unique,
#           bs = 're',
#           k = 7) +
#         # PLOT
#         LeafHabit
#       ,
#       method = 'REML',
#       # family = 'poisson' #no
#       # family = negbin(1) #no
#       # family = nb() #maybe
#       family = tw() #best
#       # family = 'ziplss'
#       # family = 'ziP' #no...
#   ) #%>%
# 
# 
# gam_tax_week_Sebacina_leaf %>% 
#   # AIC()
#   # gratia::appraise() #tw
#   # k.check()
#   # anova.gam()
#   summary()
# 
# AIC(gamm4_tax_Sebacina$mer) 
# # AIC(gam_tax_Sebacina) #better


Sebacina <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[8]
  )


# tic() #80min #rerun!converge
# bam_tax_Sebacina <- 
#   Sebacina %>%
#   run_bam_structure(
#     response = abund_sum,
#     family_used = nb()
#   ); toc()
# 
# saveRDS(
#   bam_tax_Sebacina,
#   here(
#     'stats/bam_tax_Sebacina.rds'
#   )
# )
bam_tax_Sebacina <- 
  readRDS(
    here(
      'stats/bam_tax_Sebacina.rds'
    )
  )


bam_tax_Sebacina %>%
  # gratia::appraise()
  # anova.gam()
  summary()


bam_tax_Sebacina_curves <- 
  bam_tax_Sebacina %>% 
  get_inflection_points() %>% 
  mutate(
    'Genus' = 'Sebacina'
  )

bam_tax_Sebacina_comparisons <-
  bam_tax_Sebacina %>% 
  get_emmeans_table()


#### Hygro ----


# gamm4_tax_Hygrophorus <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[9]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             k = 7) +
#           LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         # family = 'poisson' #no
#         # family = negbin(1) #no
#         family = Tweedie(1.9) #fix
#   ) #%>%
# gamm4_tax_Hygrophorus %>% 
#   .$gam %>%
#   gratia::appraise()
# 
# 
# gam_tax_week_Hygrophorus_leaf <-
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   filter(Genus == topEMFgenera[9]) %>% 
#   left_join(keyPlots) %>%
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gam(data = .,
#       formula =
#         abund_sum ~
#         s(WEEK,
#           by = LeafHabit,
#           # by = PLOT,
#           k = 7) +
#         s(SUBPLOT_unique,
#           bs = 're',
#           k = 7) +
#         # PLOT
#       LeafHabit
#       ,
#       # weights = 1 / weights(.),
#       method = 'REML',
#       # family = 'poisson' #no
#       # family = negbin(1) #no
#       family = nb() #best=PLOT
#       # family = tw() #best=LeafHabit
#       # family = 'ziplss'
#       # family = 'ziP' #no...
#   ) #%>%
# 
# 
# gam_tax_week_Hygrophorus_leaf %>% 
#   # AIC()
#   # gratia::appraise() #tw
#   # k.check()
#   # anova.gam()
#   summary()
# 
# AIC(gamm4_tax_Hygrophorus$mer) 
# # AIC(gam_tax_Hygrophorus) #better


Hygrophorus <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[9]
  )


# gam_tax_Hygrophorus <- 
#   Hygrophorus %>%
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw()
#   )
# 
# 
# gam_tax_Hygrophorus %>%
#   # gratia::appraise()
#   # anova.gam()
#   summary()
# 
# 
# gam_tax_Hygrophorus_curves <- 
#   gam_tax_Hygrophorus %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'Genus' = 'Hygrophorus'
#   )
# 
# gam_tax_Hygrophorus_comparisons <-
#   gam_tax_Hygrophorus %>% 
#   get_emmeans_table()


#### Cortina ----


# gamm4_tax_Cortinarius <-
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   
#   filter(Genus == topEMFgenera[10]) %>% 
#   
#   left_join(keyPlots) %>%
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gamm4(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             k = 7) +
#           LeafHabit
#         ,
#         random = ~ (
#           1 | SUBPLOT_unique
#         ),
#         # family = 'poisson' #no
#         # family = negbin(1) #no...
#         family = Tweedie(1.9) #best
#   ) #%>%
# gamm4_tax_Cortinarius %>% 
#   .$gam %>%
#   gratia::appraise()
# 
# 
# gam_tax_week_Cortinarius_leaf <-
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   filter(Genus == topEMFgenera[10]) %>% 
#   left_join(keyPlots) %>%
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          SUBPLOT_unique = as.factor(SUBPLOT_unique),
#          'WEEK' = week(YYYYMMDD),
#          LeafHabit = as.factor(LeafHabit),
#          Genus = as.factor(Genus)) %>%
#   ungroup() %>%
#   
#   gam(data = .,
#       formula =
#         abund_sum ~
#         s(WEEK,
#           by = LeafHabit,
#           # by = PLOT,
#           k = 7) +
#         s(SUBPLOT_unique,
#           bs = 're',
#           k = 7) +
#         # PLOT
#       LeafHabit
#       ,
#       # weights = 1 / weights(.),
#       method = 'REML',
#       # family = 'poisson' #no
#       # family = negbin(1) #no
#       family = nb() #best
#       # family = tw() #good
#       # family = 'ziplss'
#       # family = 'ziP' #no...
#   ) #%>%
# gam_tax_week_Cortinarius_leaf %>% 
#   # AIC()
#   # gratia::appraise() #nb
#   # k.check()
#   # anova.gam()
#   summary()
# 
# # ks.test(
# #   resid(gam_tax_Cortinarius),
# #   'tw'
# # )
# 
# AIC(gamm4_tax_Cortinarius$mer) 
# # AIC(gam_tax_Cortinarius) #better


Cortinarius <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(
    Genus == topEMFgenera[10]
  )


# gam_tax_Cortinarius <- 
#   Cortinarius %>%
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw()
#   )
# 
# 
# gam_tax_Cortinarius %>%
#   # gratia::appraise()
#   # anova.gam()
#   summary()
# 
# 
# gam_tax_Cortinarius_curves <- 
#   gam_tax_Cortinarius %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'Genus' = 'Cortinarius'
#   )
# 
# gam_tax_Cortinarius_comparisons <-
#   gam_tax_Cortinarius %>% 
#   get_emmeans_table()


##join----


# gam_tax_all_curves <- 
#   dplyr::full_join(
#     bam_tax_Russula_curves,
#     bam_tax_Inocybe_curves,
#     bam_tax_Tuber_curves,
#     bam_tax_Tomentella_curves,
#     bam_tax_Amphinema_curves,
#     bam_tax_Wilcoxina_curves,
#     bam_tax_Elaphomyces_curves,
#     bam_tax_Sebacina_curves,
#     bam_tax_Hygrophorus_curves,
#     bam_tax_Cortinarius_curves
#   ) %>% 
#   mutate(
#     'Genus' = as.factor(Genus)
#   )