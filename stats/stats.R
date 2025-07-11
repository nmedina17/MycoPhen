# setup ----


library(here); 
here::i_am('stats/stats.R')

source(
  here('data/import.R'))

source(
  here('analysis/commEM.R')
)
# ASVsITSrarFull_EMFfiltExploreSum

library(lubridate)
library(gamm4)
library(gratia)

library(furrr)
# future_map()


# tax ----


ASVsITSrarFull_EMFfiltSum_topTen <- 
  
  ASVsITSrarFull_EMFfiltSum %>%
  
  filter(Genus %in% topEMFgenera) %>% 
  
  distinct() #manyRepeats--faster!


### GAMM ----

# 
# #long = 20min...
# Sys.time()
future::plan(multisession); 
gam_tax <- future({
  
  ASVsITSrarFull_EMFfiltSum_topTen %>%
  
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
             # m = 1,
            by = Genus,
            k = 7) +
          LeafHabit +
          Genus
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        select = T,
        method = 'fREML',
        # control = glmerControl(mc.cores = 4),
        # family = 'poisson'
        # family = negbin(1)
        family = Tweedie(1.95) #no
    ) #%>%
}); 
plan(sequential)
gam_tax %>% 
  # future::resolved()
gam_tax %>% 
  future::value() %>%
  .$gam %>%
  gratia::appraise()
  # gam.check() #!OK -- opt for separate GAMMs?
  anova.gam()
  
bam_tax <- 
  ASVsITSrarFull_EMFfiltSum_topTen %>%
    left_join(keyPlots) %>%
    arrange(
      Genus,
      PLOT_G.spp,
      SUBPLOT_unique
    ) %>% 
    mutate(
      SUBPLOT = as.factor(SUBPLOT),
      LeafHabit = as.factor(LeafHabit),
      # SUBPLOT_unique = as.factor(SUBPLOT_unique),
      'WEEK' = week(YYYYMMDD),
      Genus = as.factor(Genus),
      'abund_sum_lag1' = 
        lag(abund_sum,
            n = 1)
    ) %>%
    ungroup() %>%
  
  bam(
  formula = abund_sum ~ 
    s(WEEK, 
      by = Genus, 
      m = 1,
      k = 7) + 
    LeafHabit + 
    s(SUBPLOT_unique,
      bs = 're') +
    s(abund_sum_lag1,
      by = Genus,
      k = 7) +
    Genus, 
    # select = T,
    # rho = 0.95,
    # correlation = corAR1(
    #   form = ~ WEEK | 
    #     SUBPLOT_unique
    # ),
    # method = 'REML',
    # family = 'poisson'
    # family = negbin(1)
    family = tw()
) 
bam_tax %>% 
  # gratia::appraise()
  gratia::concrvity() #mostOK
  # residuals %>% 
  # pacf(lag.max = 9) #%>% #t-1 #lag=t-6
  # anova.gam()
  # gratia::draw()


gam_tax_each <-gam_tax %>% 
  nest('Gdata' = !Genus) %>% #.$Gdata %>% .[[1]]
  mutate(
    'gGAM' = Gdata %>%
      furrr::future_imap(
        ~ gamm4::gamm4(
          data = .x,
          family = 'poisson', #!binomial
          # family = negbin(1),
          random = 
            ~ (
              1 |
                SUBPLOT_unique
            ),
          formula =
            # abund_sum_prop ~
            abund_sum ~
            s(WEEK,
              k = 7) +
          LeafHabit
      # PLOT_G.spp
        ) %>% 
          .$gam
    ),
    'gGAM_knots' = gGAM %>% 
      modify(
        ~ .x %>% 
          # .$gam %>% 
          k.check() %>% 
          .[,'p-value']
      ),
    'gGAM_R2' = gGAM %>% 
      modify(
        ~ .x %>% 
          summary() %>% 
          .$r.sq
      ),
    'gGAM_derivCurves' = gGAM %>% 
      modify(
        ~ .x %>% 
          gratia::derivatives() %>% 
          mutate(
            'curves' = 
              .derivative %>%
              sign() %>% 
              diff() %>% 
              c(.,NA)
          )
      ),
  'gGAM_p' = gGAM %>% 
    modify(
      ~ .x %>% 
        # .$gam %>% 
        summary() %>%
        .$s.table %>% 
        .[4]
    ),
  'gGAM_edf' = gGAM %>% 
    modify(
      ~ .x %>% 
        # .$gam %>% 
        summary() %>%
        .$s.table %>% 
        .[1]
    )
  ) %>% 
  unnest(c(
    gGAM_p,
    gGAM_edf,
    gGAM_knots,
    gGAM_R2
  ))

gam_tax

gam_tax %>%
  unnest(gGAM_derivCurves) %>% 
  filter(curves != 0) #; #Sys.time()


gam_tax$gGAM[[1]] %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()
gam_tax$gGAM[[2]] %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()

gam_tax$gGAM[[3]] %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()

gam_tax$gGAM[[4]] %>% 
  # gam.check() # OK?
  # broom::tidy()
  summary()

gam_tax$gGAM[[5]] %>% 
  # gam.check() # OK better
  # broom::tidy()
  summary()
gam_tax$gGAM[[6]] %>% 
  # gam.check() # OK better
  # broom::tidy()
  summary()
gam_tax$gGAM[[7]] %>% 
  # gam.check() # OK maybe
  # broom::tidy()
  summary()
gam_tax$gGAM[[8]] %>% 
  # gam.check() # OK better
  # broom::tidy()
  summary()
gam_tax$gGAM[[9]] %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()

gam_tax$gGAM[[10]] %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()


ASVsITSrarFull_EMFfiltExploreSum


### sep ----


#### Russ ----


gamm4_tax_Russula <-
  
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
            # by = LeafHabit,
            by = PLOT_G.spp,
            k = 7) +
          # s(abund_sum_lag1,
          #   k = 7) +
          # LeafHabit
          PLOT_G.spp
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
  ) #%>%
gamm4_tax_Russula %>% 
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
  
  
gamm4_tax_Inocybe <-
  
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
          PLOT
          # LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) ?
  ) #%>%
gamm4_tax_Inocybe %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Tuber ----


gamm4_tax_Tuber <-
  
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
          PLOT
          # LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) #?
  ) #%>%
gamm4_tax_Tuber %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Tomen ----


gamm4_tax_Tomentella <-
  
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
          PLOT
          # LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(1)
        # family = Tweedie(1.95) ?
  ) #%>%
gamm4_tax_Tomentella %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #right-skewOK=poisson
  # k.check()
  # anova.gam()
  summary()


#### Amphi ----


gamm4_tax_Amphinema <-
  
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
          PLOT
          # LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(0.1) ?
        # family = Tweedie(1.95) #?
  ) #%>%
gamm4_tax_Amphinema %>% 
  # .$mer %>% AIC()
  .$gam %>%
  # gratia::appraise() #OK
  # k.check()
  # anova.gam()
  summary()


#### Wilco ----


gamm4_tax_Wilcoxina <-
  
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
          PLOT
          # LeafHabit
        ,
        random = ~ (
          1 | SUBPLOT_unique
        ),
        family = 'poisson'
        # family = negbin(0.1) 
        # family = Tweedie(1.95) 
  ) #%>%
gamm4_tax_Wilcoxina %>% 
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
  

gam_tax_Elaphomyces <-
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
            # by = LeafHabit,
            by = PLOT,
            k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        PLOT
          # LeafHabit
        ,
        method = 'REML',
        # family = 'poisson'
        # family = negbin(1) #less
        family = tw() #this
  ) #%>%
gam_tax_Elaphomyces %>% 
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


gam_tax_Sebacina <-
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
gam_tax_Sebacina %>% 
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


gam_tax_Hygrophorus <-
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
          # by = LeafHabit,
          by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        PLOT
        # LeafHabit
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
gam_tax_Hygrophorus %>% 
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

gam_tax_Cortinarius <-
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
          # by = LeafHabit,
          by = PLOT,
          k = 7) +
        s(SUBPLOT_unique,
          bs = 're',
          k = 7) +
        PLOT
        # LeafHabit
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
gam_tax_Cortinarius %>% 
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



# ET ----

gam_ET0 <- 
  ASVsITSrarFull_EMFfiltExploreSum %>%

  na.omit() %>%

  ungroup() %>% 
  mutate(
    PLOT_G.spp = as.factor(PLOT_G.spp),
    LeafHabit = as.ordered(LeafHabit),
    Ectomycorrhiza_exploration_type_template = 
      as.ordered(Ectomycorrhiza_exploration_type_template)
  ) %>%

  gamm4(
    formula =
      abund_sum ~
      s(WEEK,
        # m = 1,
        # by = LeafHabit:Ectomycorrhiza_exploration_type_template,
        # by = Ectomycorrhiza_exploration_type_template,
        k = 7) +
      # t2(LeafHabit,
      #    Ectomycorrhiza_exploration_type_template,
      #    k = 3) +
      Ectomycorrhiza_exploration_type_template +
      # LeafHabit,
      PLOT_G.spp,
    family = 'poisson',
    # family = negbin(1), #no
    select = T,
    random = ~ (
      1 |
        # PLOT_G.spp /
        SUBPLOT_unique
    )
  ) #%>%
gam_ET0 %>% 
  # .$mer %>%
  # AIC()
  .$gam %>%
  gratia::appraise() #OK--poisson
  # gam.check() # !OK but OK?
  # anova.gam()
  # summary()

library(emmeans)
emmeans(
  gam_ET0$mer, 
  pairwise ~ Ectomycorrhiza_exploration_type_template#, 
  # re.form = NULL
) #%>% 
  # emmeans::pairwise.emmc()
  

library(lmerTest)
ASVsITSrarFull_EMFfiltExploreSum %>%
  
  na.omit() %>%
  
  mutate(
    PLOT_G.spp = as.factor(PLOT_G.spp)
  ) %>% 
  
  glmer(
    formula =
      abund_sum ~
      WEEK +
      Ectomycorrhiza_exploration_type_template +
      # LeafHabit +
      PLOT_G.spp +
      (1 | SUBPLOT_unique),
    family = negbin(0.3)
  ) #%>% 
  # residuals() %>%
  # qqnorm()
  # DHARMa::testResiduals()
  # summary()
  # car::Anova()
  # sjPlot::tab_model()
  # AIC()
  # performance::r2()


# ADDED LeafHabit to IMPROVE MODEL FIT

gam_ET <-
  ASVsITSrarFull_EMFfiltExploreSum %>%

  na.omit() %>% 
  
  mutate(SUBPLOT = as.factor(SUBPLOT),
         'WEEK' = week(YYYYMMDD)) %>%
  ungroup() %>%

  nest('ETdata' = !Ectomycorrhiza_exploration_type_template) %>% #.$Gdata %>% .[[1]]
  
  filter(Ectomycorrhiza_exploration_type_template != 'unknown') %>% 
  
  mutate(
    'gGAM' = ETdata %>%
      furrr::future_imap(
        ~ gamm4::gamm4(
          data = .x,
          # family = 'poisson', #better
          family = negbin(0.1),
          # family = 'Gamma', #neg
          random = ~ (
            1 | 
              # PLOT_G.spp /
              SUBPLOT_unique
          ),
          formula =
            # abund_sum_prop ~
            abund_sum ~
            LeafHabit +
            # PLOT_G.spp +
            s(WEEK,
              by = LeafHabit,
              # by = PLOT_G.spp,
              k = 7)
        ) %>% 
          .$gam
    ),
    'gGAM_k_p' = gGAM %>% 
      modify(
        ~ .x %>% 
          # .$gam %>% 
          k.check() %>% 
          .[,'p-value']
      ),
    'gGAM_R2' = gGAM %>% 
      modify(
        ~ .x %>% 
          summary() %>% 
          .$r.sq
      ),
    'gGAM_derivCurves' = gGAM %>% 
      modify(
        ~ .x %>% 
          gratia::derivatives() %>% 
          mutate(
            'curves' = 
              .derivative %>%
              sign() %>% 
              diff() %>% 
              c(.,NA)
          )
      ),
  'gGAM_p' = gGAM %>% 
    modify(
      ~ .x %>% 
        # .$gam %>% 
        summary() %>%
        .$s.table %>% 
        .[4] # !work w/ 2 fixed terms
    ),
  'gGAM_edf' = gGAM %>% 
    modify(
      ~ .x %>% 
        # .$gam %>% 
        summary() %>%
        .$s.table %>% 
        .[1]
    )
  ) %>% 
  unnest(c(
    gGAM_k_p,
    gGAM_R2,
    gGAM_p, 
    gGAM_edf
  )) %>% 
  arrange(Ectomycorrhiza_exploration_type_template)

gam_ET %>%
  unnest(gGAM_derivCurves) %>% 
  filter(curves != 0)


gam_ET$gGAM[[1]]$gam %>% 
  # gam.check() # OK
  # broom::tidy()
  summary()
gam_ET$gGAM[[2]]$gam %>% 
  # gam.check() # OK
  summary()
gam_ET$gGAM[[3]]$gam %>% 
  # gam.check() # OK
  summary()
gam_ET$gGAM[[4]]$gam %>% 
  # gam.check() # OK
  summary()
gam_ET$gGAM[[5]]$gam %>% 
  # gam.check() # OK
  summary()

gam_ET$gGAM[[6]]$gam %>% 
  gam.check() # NOT OK
  # summary()

gam_ET$gGAM[[7]]$gam %>% 
  # gam.check() # OK
  summary()

gam_ET$gGAM[[8]]$gam %>% 
  gam.check() # NOT OK
  # summary()


### sep ----


par(mfrow = c(2,2))
library(gamm4)

#### long ----

statET_long <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Long-dist.'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        # by = LeafHabit,
        by = PLOT_G.spp,
        k = 7) +
      # LeafHabit,
      PLOT_G.spp,
    # family = 'poisson',
    family = negbin(1),
    # family = Tweedie(p=1.95),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) #%>%
statET_long %>% 
  # .$mer %>% AIC()
  .$gam %>% 
  # gratia::draw()
  gratia::appraise()
  # gam.check()
  # plot()
  anova.gam()
  # summary()
statET_long_curves <- 
  statET_long %>% 
  .$gam %>% 
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  )

#### contact ----

statET_contact <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Contact'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        by = LeafHabit,
        # by = PLOT_G.spp,
        k = 7) +
      LeafHabit,
    # PLOT_G.spp,
    family = 'poisson',
    # family = negbin(0.00001),
    # family = Tweedie(p=1.1),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) #%>%
  # .$mer %>% AIC()
  # .$gam %>% 
  # gratia::draw()
  # gratia::appraise() #poisson
  # gam.check()
  # plot()
  # anova.gam()
  # summary()
statET_contact_curves <- 
  statET_contact %>% 
  .$gam %>%
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  ) %>% 
  filter(curves != 0)

#### medF ----

statET_medFringe <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Medium-dist. \n fringe'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        # by = LeafHabit,
        by = PLOT_G.spp,
        k = 7) +
      # LeafHabit,
    PLOT_G.spp,
    family = 'poisson',
    # family = negbin(1),
    # family = Tweedie(p=1.15),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) #%>%
statET_medFringe %>% 
  # .$mer %>% AIC()
  .$gam %>% 
  # gratia::draw()
  # gratia::appraise() #negbin/poisson
  # gam.check()
  # plot()
  # anova.gam()
  summary()
statET_medFringe_curves <- 
  statET_medFringe %>% 
  .$gam %>% 
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  )

#### medS ----

statET_medSmooth <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Medium-dist. \n smooth'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        # by = LeafHabit,
        # by = PLOT_G.spp,
        k = 7), #+
      # LeafHabit,
    # PLOT_G.spp,
    # family = 'poisson',
    family = negbin(1),
    # family = Tweedie(p=1.95),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) %>%
  # .$mer %>% AIC()
  .$gam #%>% 
  # gratia::draw()
  # gratia::appraise()
  # gam.check()
  # plot()
  # anova.gam()
# summary()
statET_medSmooth_curves <- 
  statET_medSmooth %>% 
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  )

#### shortD ----

statET_shortDelicate <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Short-dist. \n delicate'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        # by = LeafHabit,
        # by = PLOT_G.spp,
        k = 7), #+
      # LeafHabit,
    # PLOT_G.spp,
    family = 'poisson',
    # family = negbin(0.00001),
    # family = Tweedie(p=1.95),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) %>%
  # .$mer %>% AIC()
  .$gam #%>% 
  # gratia::draw()
  # gratia::appraise()
  # gam.check()
  # plot()
  # anova.gam()
# summary()
statET_shortDelicate_curves <- 
  statET_shortDelicate %>% 
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  )

#### shortC ----

statET_shortCoarse <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  filter(
    Ectomycorrhiza_exploration_type_template == 
      'Short-dist. \n coarse'
  ) %>% 
  gamm4(
    formula = 
      abund_sum ~
      s(WEEK,
        # by = LeafHabit,
        # by = PLOT_G.spp,
        k = 7), #+
      # LeafHabit,
    # PLOT_G.spp,
    # family = 'poisson',
    family = negbin(0.1),
    # family = Tweedie(p=1.95),
    # family = zeroinfl(poisson(), link = 'log'),
    random = ~ (
      1 | SUBPLOT_unique
    )
  ) %>%
  # .$mer %>% AIC()
  .$gam #%>% 
  # gratia::draw()
  # gratia::appraise()
  # gam.check()
  # plot()
  # anova.gam()
# summary()
statET_shortCoarse_curves <- 
  statET_shortCoarse %>% 
  gratia::derivatives() %>% 
  mutate(
    'curves' = 
      .derivative %>%
      sign() %>% 
      diff() %>% 
      c(.,NA)
  ) %>% 
  select(
    WEEK,
    # LeafHabit,
    curves
  )





# rich ----


gam_rich <- gamm4(
  data = 
    ASVsITSrarFull_EMFfilt_richDiv %>%
    
    na.omit() %>% 
    
    left_join(keyPlots) %>%
    
    mutate(
      SUBPLOT = as.factor(SUBPLOT)),
  family = 'poisson', #better
  # family = negbin(1), #worse
  random = ~ (
    1 | 
      # PLOT_G.spp / 
      SUBPLOT_unique
  ),
  formula = rich ~ 
    # PLOT_G.spp +
    # Group + # p<2e-16
    LeafHabit + # p<2e-16
    s(week(YYYYMMDD), # p = 0.221
      k = 7))
gam_rich$gam %>%
  # summary()
  # gam.check() #OK
  mgcv::anova.gam()


gam_div <- gamm4(
  data = 
    ASVsITSrarFull_EMFfilt_richDiv %>%
    
    na.omit() %>% 
    
    mutate(
      SUBPLOT = as.factor(SUBPLOT)
    ) %>% 
    
    left_join(keyPlots) %>% 
    
    mutate(
      LeafHabit = as.factor(LeafHabit)
    ),
  family = 
    'Gamma',
  random = ~ (
    1 | 
      # PLOT_G.spp / 
      SUBPLOT_unique
  ),
  formula = div ~ 
    # PLOT_G.spp +
    PLOT +
    # Group +
    # LeafHabit +
    s(week(YYYYMMDD), 
      # by = LeafHabit,
      by = PLOT,
      k = 7)
)
gam_div$gam %>%
  # mgcv::gam.check() #OK
  # summary()
  mgcv::anova.gam()


### post ----


# mgcv::gam(data = ASVsITSrarFull_EMFfilt_rich,
#           formula = n ~ )
#   emmeans::emmeans(~ PLOT_G.spp) #needmgcv::gam()


# networks ----


networkStat <- networkTbl %>% 
  
  left_join(keyPlots) %>% 
  
  filter(
    !(network_metric %in% 
        c('Clustering_coefficient',
          'Network_diameter'))) %>% 
  nest(data = !network_metric) %>% 
  mutate(
    'stat' = data %>% 
      modify(
        ~ gamm4(
          data = .x,
          formula = value ~ 
            s(as.numeric(SESSION), 
              k = 7) +
            PLOT,
          # family = 'poisson'
        ) %>% 
          .$gam
      ),
    'stat_viz' = stat %>% 
      modify(
        ~ .x %>% 
          anova.gam()
      )) 


# qPCR ----


cor.test(
  ~ Moisture1_7d_avg +
    Temp1_7d_avg,
  qPCRfullQC_EM
) #high
cor.test(
  ~ Moisture1_7d_avg +
    week(YYYYMMDD),
  qPCRfullQC_EM
) # sig but low

## new ----

library(gamm4)

stat_qPCR <-
  gamm4(
  data = qPCRfullQC_EM %>% 
    mutate(
      LeafHabit = as.factor(LeafHabit),
      WEEK = week(YYYYMMDD),
      PLOT = as.factor(PLOT)
    ),
  #goals
  #inference
  #modelGI
  #Pedersen2019peerJ
  formula = 
    round(
      10^`Log copies final` * 0.17
    ) /10 ~
    # s(WEEK,
    #   k = 7) ,#+
    s(WEEK,
      by = PLOT,
      # bs = 'cc',
      m = 1, #modelGI
      k = 7) +
    # s(WEEK,
    #   by = LeafHabit,
    #   m = 1, #modelGI
    #   k = 7) +
    
    # LeafHabit, #+ #p=0.5
    
    # t2(Moisture1_7d_avg,
    #    Temp1_7d_avg,
    #    m = 2, #modelGI
    #    full = T,
    #    k = 7), #+
    # t2(Moisture1_7d_avg,
    #    Temp1_7d_avg,
    #    by = LeafHabit,
    #    m = 1, #modelGI
    #    k = 7), #+
    # s(Moisture1_7d_avg,
    #   k = 7),
    
    PLOT, #p=0.5
    
  # family = 'poisson',
  # family = 'Gamma', #VtV<0def
  family = negbin(1),
  # method = 'REML',
  random = ~ (
    1 | 
      # PLOT /
      # SUBPLOT
      SUBPLOT_unique
  )
) #%>% 
stat_qPCR %>% 
  .$mer %>%
  # AIC()
  # emmeans(~ PLOT)
  .$gam %>%
  gratia::appraise()
  # gratia::concrvity(full = F) # OK only w/o t2
  # gam.check() #OK w/ t2!
  # summary()
  # anova.gam()
  # broom::tidy() %>%
  # knitr::kable()
  
  ### curves----
  # plot.derivSimulCI()
  gratia::derivatives(
    # partial_match = T
  ) %>%
  # smooth_estimates() %>%
  # ggplot(aes(WEEK, .estimate)) +
  # geom_point() + geom_line()
  # distinct(.smooth)
  mutate(
    'curves' = 
      .derivative %>% # no t2
      # .estimate %>% 
      # diff() %>%
      sign() %>%
      diff() %>% 
      c(.,NA)
  ) %>% 
  filter(curves != 0) %>% 
  select(!c(2:3))


stat_qPCR_curves <- 
  qPCRfullQC_EM %>% #glimpse()
  mutate('WEEK' = week(YYYYMMDD)) %>% 
  
  nest('data_qPCR' = !PLOT) %>% 
  
  mutate(
    'qGAM' = data_qPCR %>% 
      purrr::modify(
        ~ gamm4(
          data = .x,
          formula = 
            round(
              10^`Log copies final` *
                0.17
            ) / 10 ~ #scale
            s(WEEK, 
              # bs = 'cc',
              k = 7), #+
            # LeafHabit, #+
            # t2(Moisture1_7d_avg,
            #   Temp1_7d_avg,
            #   k = 7),
          family = negbin(1),
          random = ~ (
            1 | SUBPLOT
          )
        )
      ),
    'qGAM_knots' = 
      qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$gam %>% 
          k.check() %>% 
          .[,'p-value']
      ),
    'qGAM_est' = 
      qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$gam %>% 
          gratia::smooth_estimates() %>% 
          select(
            .estimate,
            .se
          ) %>% 
          rename(
            .se_est = .se
          )
      ),
    'qGAM_derivCurves' = 
      qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$gam %>% 
          gratia::derivatives() %>% 
          mutate(
            'curves' = 
              .derivative %>%
              sign() %>% 
              diff() %>% 
              c(.,NA) %>% 
              as.vector()
          )
      ),
    'qGAM_p' = qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$gam %>%
          summary() %>% 
          .$s.table %>% 
          .[4]
      ),
    'qGAM_edf' = qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$gam %>%
          summary() %>% 
          .$s.table %>% 
          .[1]
      ),
    'qGAM_AIC' = qGAM %>% 
      purrr::modify(
        ~ .x %>% 
          .$mer %>%
          AIC()
      )
  ) %>%
  unnest(
    c(
      qGAM_knots,
      qGAM_est,
      qGAM_p, 
      qGAM_edf, 
      qGAM_derivCurves,
      qGAM_AIC
      )
  ) %>% 
  
  filter(curves != 0)


## stats ----


statData <- qPCRfullQC %>%
  # filter(`Myco Type` == 'EM') %>%
  ##QC----
# filter(`Log copies final` > 8.5) %>%

mutate('PLOT_SUBPLOT' = paste(PLOT, SUBPLOT, sep = '_')
       # 'SESSION_PLOT_SUBPLOT' = paste(SESSION, PLOT_SUBPLOT, sep = '_')
) %>%
  
  #QUICK!
  distinct(PLATE_ID, .keep_all = T) %>%
  
  #4rstatix::
  rename('LeafHabit' = `Leaf Habit`,
         'MycoType' = `Myco Type`,
         'DNAcopies_log10' = `Log copies final`) %>%
  mutate('PLOT_ID' = paste0(PLOT, SUBPLOT),
         'DNAcopies' = 10^DNAcopies_log10) #%>%

# na.omit() %>%


##tests----

# # group_by(SESSION) %>%
# # group_by(Group) %>% #sig
# # group_by(LeafHabit) %>% #sig
# # group_by(PLOT) %>% #sig
# rstatix::anova_test(dv = DNAcopies_log10,
#                     within = c(SESSION),
#                     wid = PLOT_ID) %>%

# aov(formula = DNAcopies_log10 ~ MycoType * LeafHabit +
#       Error(SESSION)) %>% summary()
# rstatix::get_anova_table()

# lm(formula = DNAcopies ~ SESSION + LeafHabit + MycoType) %>%
# summary()
# residuals() %>% shapiro.test() #notNorm

##!correct----
# glm(formula = DNAcopies ~ SESSION,
#     family = 'poisson') %>% summary()



#GAM----


statDataFull <- statData %>%
  group_by(PLOT) %>%
  mutate('DNAcopies0' = round(DNAcopies),
         'DNAcopies01' = DNAcopies0 / max(DNAcopies0),
         LeafHabit = factor(LeafHabit, ordered = F),
         MycoType = factor(MycoType, ordered = F),
         # SUBPLOT = factor(SUBPLOT, ordered = F),
         Group = factor(Group, ordered = F),
         PLOT = factor(PLOT, ordered = F),
         WEEK = lubridate::week(YYYYMMDD),
         MONTH = lubridate::month(YYYYMMDD)) %>%
  ungroup() %>%
  
  # source('weather_prelim.R')
  left_join(weather_processedQC_rolled_sampleDates %>%
              mutate(WEEK = lubridate::week(Datetime)) )


library(mgcv)


stat <- mgcv::gam(data = statDataFull,
                  formula = DNAcopies ~ #toDATE?
                    ###global----
                  # s(MONTH,
                  #   m = 2 #m=TPRS
                  #   k = 5) +
                  # s(WEEK,
                  #   bs = 'cc', #bs=cyclicCubic
                  #   # m = 2 #m=!'cc'
                  #   k = 7) +
                  
                  # s(MONTH, PLOT, bs = 'fs', m = 2,
                  #   k = 7) + #bs=factorSmooth
                  
                  ###modelGI----
                  PLOT +
                    te(WEEK, Moisture1_7d_avg,
                       bs = c('cc', 'cc'),
                       m = 2,
                       k = c(7, 7)) +
                    te(WEEK, Moisture1_7d_avg,
                       bs = c('cc', 'cc'),
                       m = 1, by = PLOT,
                       k = c(7, 7)) +
                    
                    ####more----
                  MycoType +
                    te(WEEK, Moisture1_7d_avg,
                       bs = c('cc', 'cc'),
                       m = 1, by = MycoType,
                       k = c(7, 7)) +
                    LeafHabit +
                    te(WEEK, Moisture1_7d_avg,
                       bs = c('cc', 'cc'),
                       m = 1, by = LeafHabit,
                       k = c(7, 7)),
                  
                  ###groups----
                  # s(WEEK, by = PLOT, m = 1,
                  #   # bs = 'fs', #no
                  #   k = 7) +
                  # s(WEEK, bs = 're') +
                  #
                  # s(WEEK, bs = 'cc',
                  #   k = 7,
                  #   by = MycoType, m = 1) + #by=m=1
                  # MycoType +
                  #
                  # s(WEEK, bs = 'cc',
                  #   k = 7,
                  #   by = LeafHabit, m = 1) + #by=m=1
                  # LeafHabit +
                  #
                  # ###rand----
                  # s(SUBPLOT, k = 6, bs = 're'), #bs=randomEffect
                  
                  family = 'poisson', #try'Gamma'
                  method = 'REML')

##noamoss----
stat0 <- mgcv::gam(data = statDataFull,
                   formula = DNAcopies ~
                     # s(Moisture1_avg, Temp1_avg) +
                     
                     #ti=separateWiggles
                     # te(Moisture1_avg) +
                     # te(Temp1_avg) +
                     # ti(Moisture1_avg, Temp1_avg), #+
                     te(Moisture1_avg, bs = 'cc') +
                     te(Temp1_avg, bs = 'cc') +
                     ti(Moisture1_avg, Temp1_avg, bs = c('cc', 'cc')) + #, #+
                     te(Moisture1_avg, bs = 'gp') +
                     te(Temp1_avg, bs = 'gp') +
                     ti(Moisture1_avg, Temp1_avg, bs = c('gp', 'gp')) + #,
                     
                     s(SUBPLOT, bs = 're') +
                     #!works
                     # # s(PLOT, SUBPLOT, bs = 'fs') + #same
                     # te(PLOT) +
                     # ti(PLOT, SUBPLOT, bs = c('tp', 're')) +
                     
                     PLOT +
                     
                     MycoType +
                     
                     LeafHabit +
                     
                     s(WEEK, bs = 'gp', k = 7) +
                     s(MONTH, bs = 'cc', k = 7),
                   
                   #gaussProcess=tAutocorr
                   family = 'poisson',
                   method = 'REML') #%>% mgcv::summary.gam()
#plot(stat)
par(mfrow = c(2,2)); mgcv::gam.check(stat); AIC(stat); gratia::appraise(stat)
gratia::draw(stat); summary(stat); #mgcv::anova.gam(stat)


#simpleGAM----


cor.test(formula = ~ Temp1_7d_avg + Moisture1_7d_avg, data = statDataFull) #sig
K = 7
##MODEL----
hypothesis <- round(DNAcopies) ~ #_log10worse
  ###TREND----
MycoType +
  LeafHabit +
  # MycoType:LeafHabit +
  Group +
  PLOT +
  
  ####TIME----
# s(WEEK, k = K, bs = 'cc', m = 1) + #'cc'==m==ignored
#needed?NO==colinear...
s(WEEK, k = K, bs = 'cc', m = 1) + #m = 1, #by = PLOT == k==1
  # s(WEEK, by = MycoType, k = K, bs = 'cc') + #m = 1,
  # s(WEEK, by = LeafHabit, k = K, bs = 'cc') + #m = 1,
  # s(WEEK, by = Group, k = K, bs = 'cc') + #m = 1,
  #worse
  # te(WEEK, MycoType, k = K, bs = 'fs') +
  # te(WEEK, LeafHabit, k = K, bs = 'fs') +
  
  ###SEASON----
# s(Moisture1_14d_avg, k = K) + s(Temp1_14d_avg, k = K) + #notConcurv!
# te(Moisture1_14d_avg, Temp1_14d_avg, k = K, m = 2,
#    by = PLOT) + #colinear...
#lessCollinearity,butSTILL.
t2(Moisture1_14d_avg, Temp1_14d_avg, k = K, m = 1) + #KEY
  # te(Moisture1_14d_avg, Temp1_14d_avg, k = K, m = 1,
  #    by = MycoType) +
  # te(Moisture1_14d_avg, Temp1_14d_avg, k = K, m = 1,
  #    by = LeafHabit) +
  # te(Moisture1_14d_avg, Temp1_14d_avg, k = K, m = 1,
  #    by = Group) +
  # te(Moisture1_7d_avg, Temp1_7d_avg, k = K) +
  # te(Moisture1_avg, Temp1_avg, k = K)
  #AVG==2littleData...
  # s(Temp1_14d_avg, k = K) + s(Moisture1_14d_avg, k = K) +
  # ti(Temp1_14d_avg, Moisture1_14d_avg, k = K)

###NOISE----
# PLOT + #=SPECIES ##2many==separateModel?
# s(PLOT, SUBPLOT, k = K, bs = 're') + #==(1|PLOT/SUBPLOT)?
# s(PLOT, WEEK, k = K, bs = 're') +
# s(PLOT, k = K, bs = 're') + #==(1|PLOT)

# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# s(SUBPLOT, WEEK, k = K, bs = 're') + #'re'==!te #concurv? #repMeas...
# s(SUBPLOT, k = K, bs = 're') #==(1|SUBPLOT)

# te(SUBPLOT, WEEK, k = K, bs = 're', m = 1) #by = PLOT == k==1
t2(SUBPLOT, PLOT, k = K, bs = 're', m = 1) #otherPapers
start <- Sys.time()
simpleGAM <- gam(data = statDataFull,
                 formula = hypothesis,
                 ####AR---- #gamm()
                 # correlation = corAR1(form = ~ 1 | PLOT), #!converge
                 # correlation = corAR1(), #!converge
                 family = 'poisson', #nb,Gamma!work
                 method = 'REML', #3min #fREML==<1min
                 knots = list(WEEK = c(0, 52))#,
                 # discrete = T#,
                 # nthreads = parallelly::availableCores()
); end <- Sys.time()
##diagnose----
par(mfrow = c(2,2)); gam.check(simpleGAM); AIC(simpleGAM)
# mgcViz::check1D(simpleGAM)
# gratia::checkresiduals(simpleGAM)
concurvity(simpleGAM, full = T) #cutoff==0.8
concurvity(simpleGAM, full = F) #checkbyParms
##summary----
summary(simpleGAM); anova.gam(simpleGAM)
##plot----
plot(simpleGAM, pages = 1, all.terms = T,
     rug = T, residuals = T,
     shade = T, seWithMean = T,
     shift = coef(simpleGAM)[1],
     scheme = 2)
par(mfrow = c(1, 1)); vis.gam(simpleGAM,
                              view = c('Temp1_14d_avg', 'Moisture1_14d_avg'),
                              # view = c('Temp1_7d_avg', 'Moisture1_7d_avg'),
                              # view = c('Temp1_avg', 'Moisture1_avg'),
                              plot.type = 'persp',
                              # color = 'grey', nlevels = 20, #!work
                              too.far = 0,
                              se = 0,
                              theta = 220, r = 1)
##autocorr----
acf(resid(simpleGAM), lag.max = 9) #none
pacf(resid(simpleGAM), lag.max = 9)
#gamm()?


#GETPARMS----


# simpleGAM$linear.predictors

library(gratia)

simpleGAM %>%
  # draw()
  # gratia::fixed_effects()
  # gratia::n_smooths()
  # gratia::term_names()
  # gratia::smooth_coef_indices()
  # gratia::typical_values() #control
  # gratia::smooths()
  gratia::overview() #useful!
# gratia::parametric_effects() #useful!
# gratia::variance_comp() #useful==~CV!

statDataFull %>% group_by(PLOT) %>%
  ggplot(aes(Date, DNAcopies, color = PLOT)) + geom_point(color = 'grey') +
  facet_grid(MycoType ~ LeafHabit, scales = 'free_y') +
  geom_smooth(method = 'gam', formula = hypothesis) +
  stat_summary() + scale_y_log10()


# mgcv::predict.gam(stat) %>% as.data.frame();
ggplot(statData, aes(SESSION, mgcv::predict.gam(stat))) + geom_point()
# library(ggiraphExtra); ggiraphExtra::ggPredict(stat)

summary(stat); AIC(stat)
mgcv::anova.gam(stat)


#LOESS----
loess(data = statDataFull,
      formula = DNAcopies ~ WEEK) %>% summary()


cor.test(statDataFull$SESSION, statDataFull$DNAcopies_log10)
plot(statDataFull$SESSION, statDataFull$DNAcopies)
cor.test(statDataFull$Moisture1_7d_avg, statDataFull$DNAcopies_log10)
cor.test(statDataFull$Temp1_7d_avg, statDataFull$DNAcopies_log10)
plot(statDataFull$Temp1_7d_avg, statDataFull$DNAcopies_log10)
statDataFull %>% filter(MycoType == 'EM') %>% 
  ggplot(aes(SESSION, DNAcopies)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth()

#GAMM4----
K = 7
mod_g4 <- gamm4::gamm4(data = statDataFull %>% #,
                         filter(MycoType == 'EM'),
                       formula = log10(DNAcopies) ~
                         s(SESSION, k = K,
                           # bs = 'cc',
                           # by = PLOT,
                           m = 2) +
                         # s(Moisture1_7d_avg, k = K,
                         # # bs = 'cc',
                         # # by = PLOT,
                         # m = 2) +
                         # s(WEEK, by = PLOT, k = K, bs = 'cc', m = 2), #+
                         # s(WEEK, by = MycoType, bs = 'cc', k = K, m = 1) +
                         # s(WEEK, by = LeafHabit, bs = 'cc', k = K, m = 1) +
                         # s(WEEK, by = Group, bs = 'cc', k = K, m = 1) +
                         # t2(Moisture1_14d_avg, Temp1_14d_avg,
                         #    k = K, m = 1) +
                         # s(Moisture1_14d_avg, k = K, bs = 'cc', m = 1) +
                       # s(Temp1_14d_avg, k = K, bs = 'cc', m = 1) +
                       # MycoType + Group + LeafHabit,
                       # MycoType +
                       # LeafHabit, #+
                       # Group, #+ PLOT,#==2littleData
                       PLOT, #needByMycoType
                       random = ~ (1 | SUBPLOT),
                       # random = ~ (1 | SUBPLOT), #4PLOT
                       family = 'Gamma'#,
                       # knots = list(WEEK = c(0, 52))
)
mod_g4$gam %>% gratia::concrvity() %>% #pairwise=T
  flextable::flextable()
# mod_g4$gam %>% mgcv::anova.gam()
mod_g4$gam %>% gratia::overview() %>%
  flextable::flextable() #nicer
# gt::gt()
mod_g4$mer %>% summary() #AIC
# mod_g4$gam %>% summary() %>%
#   .$p.table #%>% #.$pTerms.table
#   # .$cov.unscaled
#   # as_tibble() %>% flextable::flextable()
mod_g4$gam %>% gratia::draw()
# coef()


#PLAN----
# Models = 1) Week, 2) t2(T, M), 3) PLOT, 4) by=PLOT?


#RE-WET----

statDataFull %>%
  filter(SESSION %in% c(3:6)) %>%
  mutate('REWET' = if_else(SESSION %in% c(3, 4), 'PRE', 'POST')) %>%
  
  # filter(SESSION %in% c(4, 5)) %>% #.$DNAcopies %>% hist()
  
  tidyr::nest(data = !MycoType) %>%
  
  mutate(data = data %>% purrr::modify(
    ~ .x %>%
      #noconverge
      # lme4::glmer(formula = DNAcopies0 ~
      #         REWET + (1 | PLOT / SUBPLOT),
      #       family = 'poisson') %>%
      #OK
      MASS::glm.nb(formula = DNAcopies0 ~ REWET + PLOT) %>%
      summary() #%>%
    # as_tibble()
  )) #%>%
# unnest(everything())


statDataFull %>%
  filter(SESSION %in% c(3:6)) %>%
  mutate('REWET' = if_else(
    SESSION %in% c(3, 4), 
    'PRE', 
    'POST')) %>%
  filter(MycoType == 'AM') %>%
  
  rstatix::anova_test(DNAcopies0 ~ REWET + PLOT) #%>% resid() #ges=35%1mills=3mil

##mag----

qPCRhist <- as.numeric(dist(statDataFull$DNAcopies0)) %>%
  hist(plot = F) #breaks = seq(1, 1e12, by = 1e6)
# tibble(mids = qPCRhist$mids, counts = qPCRhist$counts) %>%

statDataFull %>% 
  filter(MycoType == 'AM') %>%
  
  group_by(PLOT, SESSION) %>% summarize('DNAcopies0_med' = median(DNAcopies0)) %>%
  
  filter(SESSION %in% c(3:6)) %>%
  
  #diffsFWDinTimeOnly
  nest(tdata = !PLOT) %>%
  mutate(tdata = tdata %>% modify(
    ~ .x %>% 
      arrange(SESSION) %>%
      mutate(
        'DNAcopies0_med_lag' = lag(DNAcopies0_med),
        'DNAcopies0_med_jump' = DNAcopies0_med - DNAcopies0_med_lag)
  )) %>%
  unnest(tdata) %>%
  pull(DNAcopies0_med_jump) %>%
  dist() %>% as.numeric() %>% as_tibble_col() %>%
  
  ggplot(aes(value)) + 
  geom_histogram(alpha = 0.5) +
  geom_vline(aes(xintercept = 1e6)) +
  scale_x_log10() + 
  scale_y_continuous(n.breaks = 10) +
  theme_bw()

statDataFull %>% 
  filter(MycoType == 'AM') %>%
  
  ggplot(aes(SESSION, DNAcopies0)) +
  geom_point(color = 'grey') + 
  stat_summary(fun.data = ggpubr::median_mad) +
  scale_y_log10() + 
  theme_bw() + 
  scale_x_continuous(n.breaks = 10) +
  geom_line(aes(y = 1.5e6)) + 
  geom_line(aes(y = 2.5e6))


statData %>%
  ggplot(aes(SESSION, DNAcopies01,
             color = PLOT)) +
  geom_point(color = 'grey') +
  stat_summary() +
  stat_smooth(formula = y ~ s(x, k = 8), 
              method = 'gam') +
  facet_wrap(~ PLOT) +
  scale_y_log10() +
  labs(y = '18S DNA copy number (relative to max)',
       x = 'Time (samplings)')


#simpleGLM----
glm(data = statDataFull %>% 
      filter(MycoType == 'EM'),
    formula = round(DNAcopies) ~ 
      Moisture1_avg,
    family = 'poisson') %>%
  summary()
statDataFull %>% 
  filter(MycoType == 'EM') %>%
  ggplot(aes(Moisture1_avg, 
             DNAcopies, 
             color = PLOT)) +
  geom_point() +
  scale_y_log10()


#OLD----
statData %>%
  
  nest(!PLOT) %>%
  mutate(
    'stat' = data %>% modify(
      ~ .x #%>%
      # glm(formula = DNAcopies ~ SESSION,
      #     family = 'poisson')
    ),
    'statTbl' = stat %>% modify(
      ~ .x %>%
        summary() %>%
        .$coefficients %>%
        as.data.frame() %>% rownames_to_column() %>%
        filter(rowname == 'SESSION') %>%
        pull(`Pr(>|z|)`)
    )
  ) %>% unnest(stat)


#decomp----
test %>% mutate(DNAcopies = `Log copies final`) %>%
  filter(SUBPLOT == 1 & PLOT == 'CAOV') %>%
  na.exclude() %>%
  pull(DNAcopies) %>%
  ts(frequency = 9) %>%
  decompose() %>%
  plot()


#share----
# test1 %>%
#   select(!c(PLATE_ID, `Sap Flow`)) %>%
#   rename('BIOMASS_DNAcopies_log' = `Log copies final`) %>%
#   write_csv('~/Downloads/qPCR.csv')

# statDataFull %>% write.csv('~/Downloads/qPCRdata.csv')


#peaks----