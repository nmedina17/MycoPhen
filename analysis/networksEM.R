# setup ----


library(here); 
here::i_am('analysis/networksEM.R')

source(here('analysis/commEM.R'))


# https://medium.com/analytics-vidhya/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork-f6e1ceb1c523


### corr ----


focal_plotE <- 
  ASVsITSrarFull_EMFfilt_commTbl %>%
  filter(PLOT == 'QUBI') #%>% # & SESSION == 1
# select(!c(1:6)) %>% mutate('rsum' = rowSums(.)) %>% #.$rsum
# filter(rsum > 0)
focal_plotE_comm <- focal_plotE %>% 
  select(starts_with('ASV'))
cooc_matE <- focal_plotE_comm %>% 
  t()
colnames(cooc_matE) <- focal_plotE$ID; 
cooc_matE #check
# Sys.time();
# cooc_mat %>% cooccur::cooccur(spp_names = T); Sys.time(); #2slow!


### meco ----


library(microeco); 
library(tidygraph); 
library(ggraph)


#### init ----


EMFtax_tbl <- 
  ASVsITSrarFull_EM_QC %>% 
  select(asv_id:Species) %>%
  distinct() %>% 
  mutate(
    'G.spp' = paste(
      Genus, 
      Species, 
      sep = '.')
  ) %>% 
  .[-1329, ] #emptyASV
# which(is.na(EMFtax_tbl$asv_id)) #1329



meco_asv_tblE <- microtable$new(
  as.data.frame(cooc_matE),
  tax_table = EMFtax_tbl,
  sample_table = keyITS %>%
    left_join(DNAprocess) %>%
    column_to_rownames('ID')); Sys.time()


#### init_net ----


net1E <- trans_network$new(
  meco_asv_tblE,
  cor_method = 'spearman',
  filter_thres = 1e-16); Sys.time() #5min

net1E$cal_network() #MAIN

# V(net1$res_network)$deg <- degree(net1$res_network)
net1E_tbl <- net1E$res_network %>% 
  tidygraph::as_tbl_graph()


#### graph ----


net1E_tbl %>%
  left_join(EMFtax_tbl,
            by = c('name' = 'asv_id')) %>%
  mutate(
    'deg' = degree(
      ., 
      normalized = F)) %>%
  
  # ggraph::autograph()
  ggraph(layout = 'fr') +
  geom_edge_link(color = 'gray', 
                 alpha = 0.25) +
  geom_node_point(color = 'brown', 
                  alpha = 0.25, aes(size = deg)) +
  geom_node_text(aes(label = G.spp, 
                     size = deg), 
                 repel = T) +
  theme_graph() #+ legend()


#### stat ----


net1E$cal_network_attr(); 
net1E$res_network_attr

net1E$cal_module() #subgroups!

net1E$get_node_table();
net1E$res_node_table %>% 
  as_tibble() %>% #glimpse()
  mutate('dd' = degree / sum(degree)) %>% #glimpse()
  
  ggplot(aes(degree)) +
  stat_bin(geom = 'point', bins = 4) + #stat_density() #bins!=30
  theme_classic()
# net1E$cal_powerlaw()


#### vs. ----


library(meconetcomp); 
library(magrittr) #%<>%
library(igraph)


# first create a list
nets_QUBI <- list()



##### init ----


cooc_mat <- 
  ASVsITSrarFull_EMFfilt_comm %>% 
  t() 
  
colnames(cooc_mat) <- 
  ASVsITSrarFull_EMFfilt_commTbl$ID; 
cooc_mat #check

meco_asv_tbl <- 
  microtable$new(
    as.data.frame(cooc_mat),
    tax_table = 
      EMFtax_tbl %>%
      tibble::column_to_rownames(
        'asv_id'
      ),
  sample_table = 
    keyITS %>%
    left_join(DNAprocess) %>%
    tibble::column_to_rownames('ID') %>% 
    .[-1329, ] #match
  ); Sys.time()


##### runs ----


source(
  here('analysis/run_meco_netcomp.R'))
# run_meco_netcomp()


PLOTS <- 
  ASVsITSrarFull_EMFfilt_commTbl %>% 
  .$PLOT %>% 
  unique() %>% 
  .[1:5] #EMonly

SESSIONS <- 
  ASVsITSrarFull_EMFfilt_commTbl %>% 
  .$SESSION %>% 
  unique() %>% 
  .[!is.na(.)]


nets <- list()

for (plot in 1:length(PLOTS)) {
  
  for (t in SESSIONS) {
    
    net_tmp <- 
      meco_asv_tbl %>% 
      run_meco_netcomp(
        SET_PLOT = PLOTS[plot],
        SET_SESSION = t
      )
    
    nets[[
      (plot - 1) * 
        length(SESSIONS) + 
        t
      ]] <- net_tmp
  }
  
}

names(nets) <- paste0(
  rep(PLOTS, 
      each = SESSIONS %>% 
        length()
      ),
  rep(SESSIONS,
      PLOTS %>% 
        length()
      )
)
nets


##### calcs ----


nets %<>% 
  cal_module(
    undirected_method = 
      "cluster_fast_greedy") #!global

nets_cals <-
  cal_network_attr(nets) 
# returns data.frame object

# nets %<>%
#   get_node_table(node_roles = TRUE) %>%
#   get_edge_table

# calculate global properties of all sub-networks
# tmp <- subnet_property(nets) #bug?


#####gam----

nets_gams <- nets_cals %>% 
  as_tibble(
    rownames = NA
  ) %>% 
  tibble::rownames_to_column(
    'Net_metric'
  ) %>%
  pivot_longer(
    !Net_metric,
    names_to = 'PLOT_SESSION'
  ) %>%
  mutate(
    'PLOT' = PLOT_SESSION %>% 
      str_remove('\\d+') %>% 
      as.factor(),
    'SESSION' = PLOT_SESSION %>% 
      str_extract('\\d+') %>% 
      as.numeric(),
    .after = 'PLOT_SESSION'
  ) %>%
  nest(
    data = !Net_metric
  ) %>%
  mutate(
    gams = data %>% 
      modify(
        ~ gam(
            formula = value ~ 
              s(SESSION, 
                by = PLOT,
                k = 7),
            data = .x,
            family = nb(),
            method = 'REML'
          )
      )
  ) 

nets_gams$gams[[1]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[2]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[3]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[4]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[5]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[6]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[7]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[8]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[9]] %>% 
  # gratia::appraise()
  summary()
nets_gams$gams[[10]] %>% 
  # gratia::appraise()
  summary()