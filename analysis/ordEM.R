# this file runs community ordination analysis of soil ECM fungi


# setup ----

library(here); 
here::i_am('analysis/ordEM.R')


# dependency files, both need to be run once

# source(here('data/import.R'))
source(here('analysis/commEM.R'))


library(vegan)
library(tibble) 
#::rownames_to_column()


# stat ----


ordStat <- 
  vegan::adonis2(
  data =
    ASVsITSrarFull_EMFfilt_commTbl %>%
    
    # na.omit() %>% 
    
    select(!starts_with('ASV')) %>%

    left_join(keyPlots) %>%

    mutate(
      SESSION = as.factor(SESSION),
      WEEK = as.factor(week(YYYYMMDD)),
      LeafHabit = as.factor(LeafHabit)
    ) %>%

    mutate(
      across(
        where(is.numeric),
        ~ .x + 1
      )
    ),

  formula =
    ASVsITSrarFull_EMFfilt_comm %>%
    
    na.omit() %>% 
    
    mutate(
      across(
        where(is.numeric),
        ~ .x + 1
      )
    ) ~

    WEEK *
    LeafHabit,
  
  na.action = 'na.omit'
)

ordStat %>% 
  summary()


## variance ----


tax_disp_week <- 
  betadisper(
    d = 
      ASVsITSrarFull_EMFfilt_commTbl %>%
      
      na.omit() %>% 
      
      select(starts_with('ASV')) %>%
      
      vegdist(),
    
    group = c(
      ASVsITSrarFull_EMFfilt_commTbl$
        # SESSION %>%
        YYYYMMDD %>%
        week() %>%
        
        na.omit()
    )
  )

permutest( #.betadisper
  tax_disp_week,
  pairwise = F
) #!signif


tax_disp_leafHabit <- 
  betadisper(
    d = 
      ASVsITSrarFull_EMFfilt_commTbl %>%
      
      na.omit() %>% 
      
      select(starts_with('ASV')) %>%
      
      vegdist(),
    
    group = c(
      ASVsITSrarFull_EMFfilt_commTbl %>% 
        
        na.omit() %>% 
        
        left_join(keyPlots) %>% 
        .$LeafHabit
    )
  )

permutest( #.betadisper
  tax_disp_leafHabit,
  pairwise = F
) #signif


tax_disp_PLOT <- 
  betadisper(
    d = 
      ASVsITSrarFull_EMFfilt_commTbl %>%
      
      na.omit() %>% 
      
      select(starts_with('ASV')) %>%
      
      vegdist(),
    
    group = c(
      ASVsITSrarFull_EMFfilt_commTbl %>% 
        
        na.omit() %>% 
        
        left_join(keyPlots) %>% 
        .$PLOT_G.spp
    )
  )

permutest( #.betadisper
  tax_disp_PLOT,
  pairwise = F
) #signif



# PCA----
ordEMF <- pca(
  ASVsITSrarFull_EMFfilt_commTbl %>% 
    select(
      starts_with('ASV')
    ),
  scale = T
  );
ordEMF_sum <- summary(ordEMF)
ordEMF_xy <- ordEMF_sum$sites #%>% as_tibble()
ordEMF_prop <- ordEMF_sum$cont$importance %>% 
  .[2, ]
ordEMF_spp <- ordEMF_sum$species %>%
  as_tibble(rownames = NA) %>%
  tibble::rownames_to_column('asv_id') %>%
  left_join(
    unique(
      ASVsITSrarFull_EM_QC[,
                           c('asv_id',
                             'G.spp',
                             'Genus')]))

###NMDS
# metaMDS(ASVsITSrarFull_EMFfilt_commTbl %>%
#           select(!c(1:7))) #2slow?
keep_rows <- 
  ASVsITSrarFull_EMFfilt_commTbl %>%
  select(
    starts_with('ASV')
  ) %>% 
  rowSums(.) > 0
nmdsEMF <-
  ASVsITSrarFull_EMFfilt_commTbl %>%
  select(
    starts_with('ASV')
  ) %>%
  .[keep_rows, ] %>%
  vegdist() %>%
  metaMDS()


nmdsEMF %>% 
  # plot()
  .$points %>%
  cbind(
    ASVsITSrarFull_EMFfilt_commTbl %>%
      select(!starts_with('ASV')) %>% 
      .[keep_rows, ] 
  ) %>%
  
  ggplot(aes(
    MDS1,
    MDS2,
    color = 
      as.factor(SESSION)
  )) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(
    ~ PLOT_G.spp
  ) +
  scale_color_viridis_d(
    option = 'turbo'
  )
  

###PCoA
# stats::cmdscale() #Euclidean=no
# Sys.time()
# PCoA_EMF <- ASVsITSrarFull_EMFfilt_comm %>% vegdist() %>%
#   cmdscale(eig = T, add = T); Sys.time() #2slow!
# # PCoA_ITSsum <- ...
# plot(PCoA_EMF$points)


getOrd <- function(COMM_TBL) {
  
 
  # stat ----
  
  ordStat <- vegan::adonis2(
    data = COMM_TBL %>%
      select(!starts_with('ASV')),
    formula = COMM_TBL %>% 
      select(starts_with('ASV')) ~
      as.factor(SESSION) #* 
      # PLOT
    ) #long
  ordStat %>% summary()
  
  # PCA----
  ordEMF <- pca(
    COMM_TBL %>% 
      select(
        starts_with('ASV')
      ));
  ordEMF_sum <- summary(ordEMF)
  ordEMF_xy <- ordEMF_sum$sites #%>% as_tibble()
  ordEMF_prop <- ordEMF_sum$cont$importance %>% 
    .[2, ]
  # ordEMF_spp <- ordEMF_sum$species %>% 
  #   as_tibble(rownames = NA) %>%
  #   rownames_to_column('asv_id') %>%
  #   left_join(
  #     unique(
  #       ASVsITSrarFull_EM_QC[, 
  #                            c('asv_id', 
  #                              'G.spp', 
  #                              'Genus')]))
  
  
  return(list(ordStat, ordEMF_xy))
  
  
}


#trial 


# Model!='PLOT'

# QUBI ----
ord_QUBI <- commTbl_QUBI %>% 
  getOrd()

# QUAL ----
ord_QUAL <- commTbl_QUAL %>% 
  getOrd()

# CAOV ----
ord_CAOV <- commTbl_CAOV %>% 
  getOrd()

# PIST ----
ord_PIST <- commTbl_PIST %>% 
  getOrd()

# PIAB ----
ord_PIAB <- commTbl_PIAB %>% 
  getOrd()
