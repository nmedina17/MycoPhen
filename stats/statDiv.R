# setup ----


library(here); 
here::i_am('stats/statDiv.R')

# source(
#   here('data/import.R')
# )

# source(
#   here('analysis/commEM.R')
# )
source(
  here('stats/statPCR.R')
)
# ASVsITSrarFull_EMFfiltExploreSum

library(lubridate)
library(gamm4) #mgcv
library(gratia) #validate

library(furrr)
# future_map()
library(tictoc)



#data----


stat_div_data <-
  ASVsITSrarFull_EMFfilt_richDiv %>%
  
  na.omit() %>% 
  
  left_join(keyPlots) %>%
  
  group_by(SUBPLOT_unique) %>% 
  
  mutate(
    SUBPLOT = as.factor(SUBPLOT),
    LeafHabit = as.factor(LeafHabit),
    PLOT_G.spp = as.factor(PLOT_G.spp),
    
    'div_CVp_med' = 
      mad(div) /
      median(div) * 100,
    'rich_CVp_med' = 
      mad(rich) /
      median(rich) * 100
  ) %>% 
  ungroup()


# rich ----


gamm4_rich <- gamm4(
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
    PLOT_G.spp +
    # Group + # p<2e-16
    LeafHabit + # p<2e-16
    s(
      week(YYYYMMDD), # p = 0.221
      k = 7
    ) 
  )


# gamm4_rich$gam %>%
  # summary()
  # gam.check() #OK
  # car::Anova()


##gam----

K = 7

tic()
gam_rich <- 
  gam(
    
    data = 
      stat_div_data,
    
    family = 'poisson', #better
    # family = negbin(1), #worse
    # family = nb(), #worse
    
    formula = rich ~ 
      
      PLOT_G.spp +
      LeafHabit + 
      
      s(
        WEEK, 
        k = K
      ) +
      # s(
      #   WEEK,
      #   by = PLOT_G.spp,
      #   m = 1,
      #   k = K
      # ) +
      s(
        WEEK,
        by = LeafHabit,
        m = 1,
        k = K
      ) +
      
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        k = K
      ) +
      # t2(
      #   Temp1_7d_avg,
      #   Moisture1_7d_avg,
      #   by = PLOT_G.spp,
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
      
      s(
        SUBPLOT,#_unique,
        bs = 're'
      )
  ); toc()


gam_rich %>%
  # gratia::appraise() #OKpoisson
  # gratia::draw()
  # car::Anova()
  summary()


##post----


# gam_rich_curves <-
#   gam_rich %>%
#   get_inflection_points()

gam_rich_comparisons <-
  gam_rich %>%
  get_emmeans_table()


#CVp----


glmm_rich_CVp <-
  
  stat_div_data %>% 
  # na.omit() %>% 
  
  
  glmer.nb(
    formula = 
      rich_CVp_med ~
      
      #overall fx
      LeafHabit +
      PLOT_G.spp +
      
      #random
      (
        1 |
          SUBPLOT_unique
      )#,
    
    # family = 'poisson' #better
    # family = 'Gamma' #worse
    # family = glmmTMB::nbinom2() #worse
  )


# stat_div_data %>% 
#   ggplot(aes(
#     sample = div
#   )) +
#   stat_qq()

glmm_rich_CVp %>% 
  # DHARMa::simulateResiduals() %>%
  # DHARMa::plotQQunif() #glmer.nb()!
  
  performance::check_model()


glmm_rich_CVp %>% 
  # summary()
  car::Anova()
  # anova()


##post----

library(emmeans)
# library(multcomp) #select()

# gam_div_CVp_post <-
#   gam_div_CVp %>% 
#   emmeans(
#     specs = 'PLOT_G.spp',
#     method = 'pairwise'
#   ) %>% 
#   
#   pairs()



# div ----


gamm4_div <- gamm4(
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
    # PLOT +
    # Group +
    LeafHabit +
    s(week(YYYYMMDD), 
      # by = LeafHabit,
      by = PLOT,
      k = 7)
)
# gamm4_div$gam %>%
  # mgcv::gam.check() #OK
  # car::Anova()
  # summary()


##gam----

K = 7

tic()
gam_div <- 
  gam(
    
    data = 
      stat_div_data,
    
    # family = 'poisson', #bad
    # family = negbin(1), #worse
    # family = nb(), #bad
    family = tw(), #wayyybetter
    
    formula = div ~ 
      
      PLOT_G.spp +
      LeafHabit + 
      
      s(
        WEEK, 
        k = K
      ) +
      # s(
      #   WEEK,
      #   by = PLOT_G.spp,
      #   m = 1,
      #   k = K
      # ) +
      s(
        WEEK,
        by = LeafHabit,
        m = 1,
        k = K
      ) +
      
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        k = K
      ) +
      # t2(
      #   Temp1_7d_avg,
      #   Moisture1_7d_avg,
      #   by = PLOT_G.spp,
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
      
      s(
        SUBPLOT,#_unique,
        bs = 're'
      )
  ); toc()


gam_div %>%
  # gratia::appraise() #tw=1.01 #nb=2164074
  # gratia::draw()
  # car::Anova()
  summary()


## post ----


# gam_div_curves <-
#   gam_div %>%
#   get_inflection_points()

gam_div_comparisons <-
  gam_div %>%
  get_emmeans_table()


#CVp----


glmm_div_CVp <-
  
  stat_div_data %>% 
  
  
  glmer(
    formula = 
      div_CVp_med ~
      
      #overall fx
      LeafHabit +
      PLOT_G.spp +
      
      #random
      (
        1 |
          SUBPLOT_unique
      ),
    
    # family = 'poisson' #no
    family = 'Gamma'
  )


# stat_div_data %>% 
#   ggplot(aes(
#     sample = div
#   )) +
#   stat_qq()

glmm_div_CVp %>% 
  # DHARMa::simulateResiduals() %>%
  # DHARMa::plotQQunif() #glmer.nb()!
  
  performance::check_model()
  
  # summary()
  # car::Anova()
# anova()


##post----

library(emmeans)
# library(multcomp) #select()

# gam_div_CVp_post <-
#   gam_div_CVp %>% 
#   emmeans(
#     specs = 'PLOT_G.spp',
#     method = 'pairwise'
#   ) %>% 
#   
#   pairs()