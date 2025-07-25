# this file initiates community ecology analyses 


# setup----


library(here); 
here::i_am('analysis/commEM.R')
library(dplyr); 
library(tidyr); 
library(stringr); 
library(ggplot2)

# dependency file, mainly generates ASV table object 'ASVsITSrarFull_EM_QC'
source(
  here('data/import.R')
)

library(purrr) #::modify()
library(vegan); #select()


## quick mini view of hygro ----

ASVsITSrarFull_EM_QC %>% 
  filter(Genus == 'Hygrophorus') %>%
  group_by(PLOT, 
           SESSION, 
           Species) %>% 
  summarise(n = n())


## sum ----


ASVsITSrarFull_EMFfiltSum <- 
  
  ASVsITSrarFull_EM_QC %>%
  
  group_by(PLOT, PLOT_G.spp, 
           YYYYMMDD, 
           SUBPLOT, 
           LeafHabit,
           Genus) %>%
  # rstatix::get_summary_stats(abund) %>%
  summarise(
    n = n(),
    'abund_sum' = sum(abund)) %>%
  arrange(desc(abund_sum)) %>% #%>% .$Family %>% unique()
  ungroup() %>% group_by(
    PLOT, PLOT_G.spp, 
    YYYYMMDD) %>%
  mutate(
    'abund_sum_prop' = abund_sum / 
      sum(abund_sum),
    WEEK = week(YYYYMMDD)
  ) %>%
  arrange(PLOT_G.spp, 
          YYYYMMDD, 
          WEEK,
          SUBPLOT, 
          Genus) %>% #check
  
  ### ETjoin ----
  left_join(
    ASVsITSrarFull_EM_QC %>% 
      select(
        SUBPLOT_unique,
        Genus,
        Ectomycorrhiza_exploration_type_template,
        Moisture1_7d_avg,
        Temp1_7d_avg
      ) 
  ) %>% 
  
  mutate(
    ET = Ectomycorrhiza_exploration_type_template 
  )


## export ----


qPCRfullQC_EM <- 
  vroom::vroom(
    here('data/qPCRfullQC_EM.csv'))


# legacy -- too slow to make full combined table of qPCR and ASVs

#mem=30gb!
# qPCRabund_EM <- 
#   ASVsITSrarFull_EMFfiltSum %>% 
#   left_join(qPCRfullQC_EM)

# write.csv(qPCRabund_EM, here('stats/qPCRabund_EM.csv'))


# unique ASVs

ASVsITSrarFull$asv_id %>% 
  # unique() %>% 
  length()
ASVsITSrarFull_EM_QC$asv_id %>% 
  unique() %>% 
  length()


## top 10 ----


topEMFdetail <- 
  
  ASVsITSrarFull_EM_QC %>%
  
  nest(data = !c(YYYYMMDD, PLOT)) %>% #.[[1, 'data']]
  arrange(PLOT, YYYYMMDD) %>%
  mutate(
    data = data %>% 
      modify(
        ~ .x %>% 
          group_by(Genus) %>%
          summarise(
            'abund_sum' = sum(abund)) %>%
          arrange(desc(abund_sum)) %>% 
          head(5)
  )) %>% #.[[1, 'data']]
  unnest(everything())


topEMF <- 
  ASVsITSrarFull_EM_QC %>%
  group_by(Genus) %>%
  summarise(
    'abund_sum' = sum(abund)) %>%
  ungroup() %>% 
  mutate(
    'abund_prop' = abund_sum / 
      sum(abund_sum)) %>%
  arrange(desc(abund_sum)) %>%
  mutate(
    'abund_prop_cumul' = 
      cumsum(abund_prop))


topEMFgenera <- topEMF %>% 
  head(10) %>% 
  pull(Genus)


## choice ----

choiceEMFgenera <- 
  topEMF %>% 
  filter(
    Genus %in% 
      c(
        'Hymenogaster',
        # 'Tylospora',
        'Piloderma',
        'Lactarius',
        # 'Membranomyces',
        'Cenococcum',
        # 'Humaria',
        'Rhizopogon',
        'Suillus',
        'Helvella',
        'Scleroderma',
        
        'Amanita',
        'Boletus'
      )
  ) %>%
  pull(Genus) 


## comm ----

# legacy, not used

# ASVsITSrar_comm <- ASVsITSrar %>%
#   select(asv_id, starts_with('X')) %>% #glimpse()
#   # as.data.frame() %>% #data.table::transpose()
#   t() #%>% View();
# colnames(ASVsITSrar_comm) <- ASVsITSrar_comm['asv_id', ]
# dim(ASVsITSrar_comm)
# ASVsITSrar_comm <- ASVsITSrar_comm[2:259, ] #%>% View()
# ASVsITSrar_commTbl <- ASVsITSrar_comm %>% as_tibble() %>%
#   mutate(across(everything(), as.numeric))


# MAIN COMMUNITY TABLE 

ASVsITSrarFull_EMFfilt_commTbl <- 
  
  ASVsITSrarFull_EM_QC %>%
  
  mutate(
    WEEK = week(YYYYMMDD)
  ) %>%
  
  select(
    asv_id, 
    seqID : PLATE_ID, 
    SUBPLOT_unique,
    SUBPLOT,
    Group,
    WEEK,
    PLOT_G.spp, 
    Temp1_7d_avg,
    Moisture1_7d_avg,
    YYYYMMDD
  ) %>% #!G.spp!!
  pivot_wider(
    names_from = asv_id,
    values_from = abund,
    values_fill = 0
  ) %>%
  arrange(ID) #check
# select(!c(seqID, PLOT:PLATE_ID))
# stats::prcomp(ASVsITSrarFull_EMFfilt_commTbl[,7:1334]) #alt


### data-export ----

ASVsITSrarFull_EMFfilt_commTbl %>% 
  rename(
    'SAMPLE_ID' = ID
  ) %>%
  select(
    -c(
      seqID,
      PLATE_ID, 
      SUBPLOT_unique
    )
  ) %>%
vroom::vroom_write(
  file = here('analysis/ASVsITS2rarFull_EMF_commTbl.csv'),
  delim = ',',
  col_names = TRUE,
  na = 'NA'
)


# rows only

ASVsITSrarFull_EMFfilt_comm <- 
  ASVsITSrarFull_EMFfilt_commTbl %>%
  select(starts_with('ASV')) #%>% #glimpse()


commTbl_QUBI <- ASVsITSrarFull_EMFfilt_commTbl %>% 
  filter(PLOT == 'QUBI')
commTbl_QUAL <- ASVsITSrarFull_EMFfilt_commTbl %>% 
  filter(PLOT == 'QUAL')
commTbl_CAOV <- ASVsITSrarFull_EMFfilt_commTbl %>% 
  filter(PLOT == 'CAOV')
commTbl_PIST <- ASVsITSrarFull_EMFfilt_commTbl %>% 
  filter(PLOT == 'PIST')
commTbl_PIAB <- ASVsITSrarFull_EMFfilt_commTbl %>% 
  filter(PLOT == 'PIAB')


### divers ----


# not used, combined table version below this


# ASVsITSrarFull_EMFfilt_rich <- 
#   
#   ASVsITSrarFull_EM_QC %>% #distinct_()
#   
#   group_by(PLOT_G.spp, 
#            YYYYMMDD, 
#            SUBPLOT) %>%
#   summarize(n = n()) #alternate option, same as vegan::specnumber()


# used manual count

ASVsITSrarFull_EMFfilt_richDiv <- 
  
  ASVsITSrarFull_EMFfilt_commTbl %>%
  
  nest(
    'comm' = 
      starts_with('ASV')
  ) %>%
  mutate(
    'rich' = comm %>% 
      modify(
        ~ .x %>% 
          vegan::specnumber()
      ),
    'div' = comm %>% 
      modify(
        ~ .x %>% 
          vegan::diversity()
      )
  ) %>%
  unnest(c(rich, div)) #%>%
  # pivot_longer(c(rich, div), names_to = 'commVar') %>%


# exploration type
## type ----

ASVsITSrarFull_EMFfiltExploreSum <-

  ASVsITSrarFull_EM_QC %>%
  
  mutate(
    WEEK = week(YYYYMMDD),
    ET = Ectomycorrhiza_exploration_type_template
  ) %>%

  group_by(
    LeafHabit,
    Group,
    PLOT_G.spp,
    YYYYMMDD,
    WEEK,
    Moisture1_7d_avg,
    Temp1_7d_avg,
    SUBPLOT,
    SUBPLOT_unique,
    ET
  ) %>%
  # rstatix::get_summary_stats(abund) %>%
  summarize(
    n = n(),
    'abund_sum' = sum(abund)) %>%
  arrange(desc(abund_sum)) %>% #%>% .$Family %>% unique()
  mutate(
    'abund_sum_prop' = abund_sum / 
      sum(abund_sum),
    ET = 
      factor(
        ET,
        labels = c(
          'Contact',
          'Short-dist. \n delicate',
          'Short-dist. \n coarse',
          'Medium-dist. \n fringe',
          'Medium-dist. \n smooth',
          'Long-dist.',
          'Mat',
          'Unknown'
        ),
        levels = c(
          'contact',
          'short-distance_delicate',
          'short-distance_coarse',
          'medium-distance_fringe',
          'medium-distance_smooth',
          'long-distance',
          'mat',
          'unknown'
        )
      )
  ) %>%
  arrange(PLOT_G.spp,
          YYYYMMDD,
          SUBPLOT) 


#corrplot----

# correlation plot for supplementary figure

ET_corr <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  ungroup() %>%
  filter(
    !(
      ET %in% 
        c('Unknown', 'Mat')
      )
  ) %>%
  select(
    WEEK,
    PLOT_G.spp,
    SUBPLOT,
    ET,
    abund_sum
  ) %>% 
  na.omit() %>% 
  pivot_wider(
    names_from = ET,
    values_from = abund_sum,
    values_fill = 0
  ) %>% 
  select(
    where(
      is.numeric
    ),
    -c(
      WEEK,
      SUBPLOT
    )
  ) 
  
ET_corr_p <- 
  ET_corr %>%
  as.matrix() %>% 
  Hmisc::rcorr(
    type = 'spearman'
  ) 
  
ET_corr %>% 
  cor() %>% 
  corrplot::corrplot(
    method = 'color',
    type = 'lower',
    diag = F,
    # addCoef.col = T,
    # addCoefasPercent = T,
    tl.col = 'black',
    # tl.srt = 45,
    # tl.cex = 0.75,
    # cl.cex = 0.75,
    # cl.ratio = 0.2,
    # cl.align.text = 'l',
    addgrid.col = 'gray',
    # number.cex = 0.5
    p.mat = ET_corr_p$P,
    sig.level = c(0.001, 0.01, 0.05),
    insig = "label_sig",
    pch.cex = 1.5,
    pch.col = "black"
  )


ET_corr_groups <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  ungroup() %>%
  select(
    WEEK,
    PLOT_G.spp,
    SUBPLOT,
    ET,
    abund_sum
  ) %>% 
  na.omit() %>% 
  nest(
    data = -c(
      WEEK,
      PLOT_G.spp
      # SUBPLOT
    )
  ) %>% 
  mutate(
    'corr_data' = modify(
      data,
      ~ .x %>% 
        select(
          ET,
          abund_sum
        ) %>%
        pivot_wider(
          names_from = ET,
          values_from = abund_sum
          # values_fill = 0
        ) 
    )
  ) 

# not used

# ET_corr_groups %>%
#   # unnest(corrs)
#   mutate(
#     'corrs' = modify(
#       corr_data,
#       ~ .x %>% 
#         as.matrix() %>%
#         cor(
#           method = 'spearman'
#         ) %>% 
#         corrplot::corrplot(
#           method = 'color',
#           type = 'lower',
#           tl.col = 'black',
#           addgrid.col = 'gray',
#           # number.cex = 0.5
#           # p.mat = ET_corr_p$P,
#           # sig.level = c(0.001, 0.01, 0.05),
#           # insig = "label_sig",
#           pch.cex = 1.5,
#           pch.col = "black"
#         )
#     )
#   ) 



Genus_corr <- 
  ASVsITSrarFull_EMFfiltSum %>% 
  ungroup() %>%
  select(
    WEEK,
    PLOT_G.spp,
    SUBPLOT,
    Genus,
    abund_sum
  ) %>% 
  na.omit() %>% 
  pivot_wider(
    names_from = Genus,
    values_from = abund_sum
    # values_fill = 0
  ) %>% 
  select(
    where(
      is.numeric
    ),
    -c(
      WEEK,
      SUBPLOT
    )
  ) 

# not used or elaborated on

# Genus_corr_p <- 
#   Genus_corr %>%
#   as.matrix() %>% 
#   Hmisc::rcorr(
#     type = 'spearman'
#   ) 
# 
# Genus_corr %>% 
#   cor() %>% 
#   corrplot::corrplot(
#     method = 'color',
#     type = 'lower',
#     tl.col = 'black',
#     # tl.srt = 45,
#     # tl.cex = 0.75,
#     # cl.cex = 0.75,
#     # cl.ratio = 0.2,
#     # cl.align.text = 'l',
#     addgrid.col = 'gray',
#     # number.cex = 0.5
#     p.mat = Genus_corr_p$P,
#     sig.level = c(0.001, 0.01, 0.05),
#     insig = "label_sig",
#     pch.cex = 1.5,
#     pch.col = "black",
#     sig.symbols = c("***", "**", "*")
#   )