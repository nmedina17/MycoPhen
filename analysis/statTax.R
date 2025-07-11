# setup ----


library(here); 
here::i_am('stats/statTax.R')

source(
  here('data/import.R'))

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


ASVsITSrarFull_EMFfiltSum_topTen <- 
  
  ASVsITSrarFull_EMFfiltSum %>%
  
  filter(Genus %in% topEMFgenera) %>% 
  
  distinct() #manyRepeats--faster!


### GAMM ----

# 
# #long = 20min...
# Sys.time()
# future::plan(multisession); 
K <- 7
# gam_tax <- 
#   
#   # future({
#   
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#     
#     left_join(keyPlots) %>%
#     
#     mutate(SUBPLOT = as.factor(SUBPLOT),
#            SUBPLOT_unique = as.factor(SUBPLOT_unique),
#            'WEEK' = week(YYYYMMDD),
#            LeafHabit = as.factor(LeafHabit),
#            Genus = as.factor(Genus)) %>%
#     ungroup() %>%
#     
#     gam(
#       data = .,
#       formula =
#         abund_sum ~
#         
#         s(WEEK,
#           k = K) +
#         s(WEEK,
#           m = 1,
#           by = Genus,
#           k = K) +
#         s(WEEK,
#           m = 1,
#           by = LeafHabit,
#           k = K) +
#         s(WEEK,
#           m = 1,
#           by = PLOT_G.spp,
#           k = K) +
#         
#         LeafHabit +
#         PLOT_G.spp +
#         Genus +
#         
#         t2(
#           Temp1_7d_avg,
#           Moisture1_7d_avg,
#           k = K
#         ) +
#         t2(
#           Temp1_7d_avg,
#           Moisture1_7d_avg,
#           by = Genus,
#           m = 1,
#           k = K
#         ) +
#         t2(
#           Temp1_7d_avg,
#           Moisture1_7d_avg,
#           by = LeafHabit,
#           m = 1,
#           k = K
#         ) +
#         t2(
#           Temp1_7d_avg,
#           Moisture1_7d_avg,
#           by = PLOT_G.spp,
#           m = 1,
#           k = K
#         ) +
#         
#         s(
#           SUBPLOT_unique,
#           bs = 're'
#         )
#       ,
#           # random = ~ (
#           #   1 | SUBPLOT_unique
#           # ),
#           select = T,
#           method = 'fREML',
#           # control = glmerControl(mc.cores = 4),
#           # family = 'poisson'
#           # family = negbin(1)
#           # family = Tweedie(1.95) #no
#           family = tw() #1.9?
#     ) #%>%
# 
# # }); 
# 
# # plan(sequential)
# 
# gam_tax %>% 
#   # future::resolved()
#   gam_tax %>% 
#   # future::value() %>%
#   .$gam %>%
#   gratia::appraise()
#   # gam.check() #!OK -- opt for separate GAMMs?
#   car::Anova()


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
saveRDS(
  bam_tax,
  here('stats/bam_tax.rds')
)


bam_tax %>% 
  gratia::appraise()
  # gratia::concrvity() #mostOK
# residuals %>% 
# pacf(lag.max = 9) #%>% #t-1 #lag=t-6
# anova.gam()
# gratia::draw()


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


### sep ----


#### Russ ----


gamm4_tax_week_Russula_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[1]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(
    SUBPLOT = as.factor(SUBPLOT),
    SUBPLOT_unique = as.factor(SUBPLOT_unique),
    'WEEK' = week(YYYYMMDD),
    LeafHabit = as.factor(LeafHabit),
    Genus = as.factor(Genus)
    # 'abund_sum_lag1' = 
    #   lag(abund_sum,
    #       n = 1)
  ) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT_G.spp,
            k = 7) +
          # s(abund_sum_lag1,
          #   k = 7) +
          LeafHabit
          # PLOT_G.spp
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
  ) #%>%


gamm4_tax_week_Russula_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # resid() %>% 
  # pacf(lag.max = 9) #t-1=!needed
  # gratia::appraise() #poisson
  # k.check()
  # anova.gam()
  summary()

# gam_tax_Russula <-
#   ASVsITSrarFull_EMFfiltSum_topTen %>%
#   filter(Genus == topEMFgenera[1]) %>% 
#   left_join(keyPlots) %>%
#   mutate(
#     SUBPLOT = as.factor(SUBPLOT),
#     SUBPLOT_unique = as.factor(SUBPLOT_unique),
#     'WEEK' = week(YYYYMMDD),
#     Genus = as.factor(Genus)
#   ) %>%
#   ungroup() %>%
#   
#   gam(data = .,
#         formula =
#           abund_sum ~
#           s(WEEK,
#             k = 7) +
#           LeafHabit +
#         # s(abund_sum_lag1)
#         s(SUBPLOT_unique,
#           bs = 're'),
#         # family = 'poisson'
#         # family = negbin(1)
#         # family = tw()
#         family = ziplss
#   ) #%>%
# gam_tax_Russula %>% 
#   gratia::appraise()
#   # anova.gam()
#   summary()


#### Inocybe ----


gamm4_tax_week_Inocybe_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[2]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT,
            k = 7) +
          # PLOT
        LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) ?
  ) #%>%


gamm4_tax_week_Inocybe_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Tuber ----


gamm4_tax_week_Tuber_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[3]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT,
            k = 7) +
          # PLOT
        LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) #?
  ) #%>%


gamm4_tax_week_Tuber_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Tomen ----


gamm4_tax_week_Tomentella_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[4]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT,
            k = 7) +
          # PLOT
        LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) ?
  ) #%>%


gamm4_tax_week_Tomentella_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Amphi ----


gamm4_tax_week_Amphinema_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[5]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT,
            k = 7) +
          # PLOT
        LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(0.1) ?
        # family = Tweedie(1.95) #?
  ) #%>%


gamm4_tax_week_Amphinema_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #OK
  # k.check()
  # anova.gam()
  summary()


#### Wilco ----


gamm4_tax_week_Wilcoxina_leaf <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[6]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            # by = PLOT,
            k = 7) +
          # PLOT
        LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(0.1) 
        # family = Tweedie(1.95) 
  ) #%>%


gamm4_tax_week_Wilcoxina_leaf %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #OK
  # k.check()
  # anova.gam()
  summary()


#### Elaph ----


gamm4_tax_Elaphomyces <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[7]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            by = LeafHabit,
            k = 7) +
          LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        # family = 'poisson'
        # family = negbin(1) #maybe
        family = Tweedie(1.9) #maybe
  ) #%>%
gamm4_tax_Elaphomyces %>% 
  .$gam %>%
  gratia::appraise() #nb/tw
# anova.gam()


gam_tax_week_Elaphomyces_leaf <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(Genus == topEMFgenera[7]) %>% 
  left_join(keyPlots) %>%
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gam(data = .,
      formula =
        abund_sum ~
        s(WEEK,
          by = LeafHabit,
          # by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        # PLOT
      LeafHabit
      ,
      method = 'REML',
      # family = 'poisson'
      # family = negbin(1) #less
      family = tw() #this
  ) #%>%


gam_tax_week_Elaphomyces_leaf %>% 
  # AIC()
  # gratia::appraise() #tw
  # k.check()
  # anova.gam()
  summary()

AIC(gamm4_tax_Elaphomyces$mer)
AIC(gam_tax_Elaphomyces) #better


#### Seba ----


gamm4_tax_Sebacina <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[8]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            k = 7) +
          LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        # family = 'poisson' #no
        # family = negbin(1) #no
        family = Tweedie(1.9) #zeros...
  ) #%>%
gamm4_tax_Sebacina %>% 
  .$gam %>%
  gratia::appraise()


gam_tax_week_Sebacina_leaf <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(Genus == topEMFgenera[8]) %>% 
  left_join(keyPlots) %>%
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gam(data = .,
      formula =
        abund_sum ~
        s(WEEK,
          by = LeafHabit,
          # by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        # PLOT
        LeafHabit
      ,
      method = 'REML',
      # family = 'poisson' #no
      # family = negbin(1) #no
      # family = nb() #maybe
      family = tw() #best
      # family = 'ziplss'
      # family = 'ziP' #no...
  ) #%>%


gam_tax_week_Sebacina_leaf %>% 
  # AIC()
  # gratia::appraise() #tw
  # k.check()
  # anova.gam()
  summary()

AIC(gamm4_tax_Sebacina$mer) 
AIC(gam_tax_Sebacina) #better


#### Hygro ----


gamm4_tax_Hygrophorus <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[9]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            k = 7) +
          LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        # family = 'poisson' #no
        # family = negbin(1) #no
        family = Tweedie(1.9) #fix
  ) #%>%
gamm4_tax_Hygrophorus %>% 
  .$gam %>%
  gratia::appraise()


gam_tax_week_Hygrophorus_leaf <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(Genus == topEMFgenera[9]) %>% 
  left_join(keyPlots) %>%
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gam(data = .,
      formula =
        abund_sum ~
        s(WEEK,
          by = LeafHabit,
          # by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        # PLOT
      LeafHabit
      ,
      # weights = 1 / weights(.),
      method = 'REML',
      # family = 'poisson' #no
      # family = negbin(1) #no
      family = nb() #best=PLOT
      # family = tw() #best=LeafHabit
      # family = 'ziplss'
      # family = 'ziP' #no...
  ) #%>%


gam_tax_week_Hygrophorus_leaf %>% 
  # AIC()
  # gratia::appraise() #tw
  # k.check()
  # anova.gam()
  summary()

AIC(gamm4_tax_Hygrophorus$mer) 
AIC(gam_tax_Hygrophorus) #better


#### Cortina ----


gamm4_tax_Cortinarius <-
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
  filter(Genus == topEMFgenera[10]) %>% 
  
  left_join(keyPlots) %>%
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gamm4(data = .,
        formula =
          abund_sum ~
          s(WEEK,
            k = 7) +
          LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        # family = 'poisson' #no
        # family = negbin(1) #no...
        family = Tweedie(1.9) #best
  ) #%>%
gamm4_tax_Cortinarius %>% 
  .$gam %>%
  gratia::appraise()


gam_tax_week_Cortinarius_leaf <-
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  filter(Genus == topEMFgenera[10]) %>% 
  left_join(keyPlots) %>%
  mutate(SUBPLOT = as.factor(SUBPLOT),
         SUBPLOT_unique = as.factor(SUBPLOT_unique),
         'WEEK' = week(YYYYMMDD),
         LeafHabit = as.factor(LeafHabit),
         Genus = as.factor(Genus)) %>%
  ungroup() %>%
  
  gam(data = .,
      formula =
        abund_sum ~
        s(WEEK,
          by = LeafHabit,
          # by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        # PLOT
      LeafHabit
      ,
      # weights = 1 / weights(.),
      method = 'REML',
      # family = 'poisson' #no
      # family = negbin(1) #no
      family = nb() #best
      # family = tw() #good
      # family = 'ziplss'
      # family = 'ziP' #no...
  ) #%>%
gam_tax_week_Cortinarius_leaf %>% 
  # AIC()
  # gratia::appraise() #nb
  # k.check()
  # anova.gam()
  summary()

# ks.test(
#   resid(gam_tax_Cortinarius),
#   'tw'
# )

AIC(gamm4_tax_Cortinarius$mer) 
AIC(gam_tax_Cortinarius) #better
