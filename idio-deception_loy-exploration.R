################################################################################

# An Idiographic Approach to Deception Cues - Loy, Corley, & Rohde (2018)

################################################################################

library(tidyverse)
library(readxl)
library(lme4)
library(cowplot)
library(performance)
library(metafor)
library(qgraph)
library(IsingFit)
library(psych)
library(igraph)
library(psychonetrics)
library(grid)
library(gridExtra)
library(png)

# Load data --------------------------------------------------------------------

# Data for this study can be retrieved from https://osf.io/auj5b/

if (!dir.exists("data/")) {
  
  dir.create("data/")
  
}

if (!file.exists("data/data_allcues.RDS")) {
  
  osf_retrieve_file("5b1475b364f25a000f7661ba") %>% 
    osf_download(path = "data/", 
                 conflicts = "overwrite")
  
}

loy <- readRDS("data/data_allcues.RDS")

# Wrangle ----------------------------------------------------------------------

loy$utterance <- factor(loy$utterance, levels = c("truth", "lie"))

# Analysis ---------------------------------------------------------------------

# Latency

latency_uncon_lmm <- lmer(latency_speech
                          ~ 1
                          + (1|subject_nr)
                          + (1|targ),
                          data = loy)

latency_icc <- icc(latency_uncon_lmm, by_group = TRUE)

latency_decep_lmm <- lmer(latency_speech
                          ~ 1
                          + utterance
                          + (1 + utterance|subject_nr)
                          + (1|targ),
                          data = loy)

latency_coef <- coef(latency_decep_lmm)$subject_nr

# Duration

duration_uncon_lmm <- lmer(uttdur
                           ~ 1
                           + (1|subject_nr)
                           + (1|targ),
                           data = loy)

duration_icc <- icc(duration_uncon_lmm, by_group = TRUE)

duration_decep_lmm <- lmer(uttdur
                           ~ 1
                           + utterance
                           + (1 + utterance|subject_nr)
                           + (1|targ),
                           data = loy)

duration_coef <- coef(duration_decep_lmm)$subject_nr

# Silent Pauses

silent_uncon_lmm <- glmer(sp
                          ~ 1
                          + (1|subject_nr)
                          + (1|targ),
                          data = loy,
                          family = binomial)

silent_icc <- icc(silent_uncon_lmm, by_group = TRUE)

silent_decep_lmm <- glmer(sp
                          ~ 1
                          + utterance
                          + (1 + utterance|subject_nr)
                          + (1|targ),
                          data = loy,
                          family = binomial)

silent_coef <- coef(silent_decep_lmm)$subject_nr

# Filled Pauses

filled_uncon_lmm <- glmer(fp
                          ~ 1
                          + (1|subject_nr)
                          + (1|targ),
                          data = loy,
                          family = binomial)

filled_icc <- icc(filled_uncon_lmm, by_group = TRUE)

filled_decep_lmm <- glmer(fp
                          ~ 1
                          + utterance
                          + (1 + utterance|subject_nr)
                          + (1|targ),
                          data = loy,
                          family = binomial)

filled_coef <- coef(filled_decep_lmm)$subject_nr

# Pause Length

pause_uncon_lmm <- lmer(spdur
                        ~ 1
                        + (1|subject_nr)
                        + (1|targ),
                        data = loy)

pause_icc <- icc(pause_uncon_lmm, by_group = TRUE)

pause_decep_lmm <- lmer(spdur
                        ~ 1
                        + utterance
                        + (1 + utterance|subject_nr)
                        + (1|targ),
                        data = loy)

pause_coef <- coef(pause_decep_lmm)$subject_nr

# Repair

repair_uncon_lmm <- glmer(repairs
                         ~ 1
                         + (1|subject_nr)
                         + (1|targ),
                         data = loy,
                         family = binomial)

repair_icc <- icc(repair_uncon_lmm, by_group = TRUE)

repair_decep_lmm <- glmer(repairs
                         ~ 1
                         + utterance
                         + (1 + utterance|subject_nr)
                         + (1|targ),
                         data = loy,
                         family = binomial,
                         control = glmerControl(
                           optimizer = "Nelder_Mead"
                         ))

repair_coef <- coef(repair_decep_lmm)$subject_nr

# Hand Movements

hands_uncon_lmm <- glmer(hand_n
                         ~ 1
                         + (1|subject_nr),
                         data = loy,
                         family = binomial)

hands_icc <- icc(hands_uncon_lmm, by_group = TRUE)

hands_decep_lmm <- glmer(hand_n
                         ~ 1
                         + utterance
                         + (1 + utterance|subject_nr),
                         data = loy,
                         family = binomial,
                         control = glmerControl(
                           optimizer = "Nelder_Mead"
                         ))

hands_coef <- coef(hands_decep_lmm)$subject_nr

# Networks ---------------------------------------------------------------------

# Iteratively generate network graphs for each sender

loy_select <- loy %>% 
  select(
    subject_nr,
    veracity = utterance,
    fp, sp, sub, 
    prolongations, repairs, repetitions,
    head_ns, hand_n, body, shoulder, mouth, eyebrows, smile, gaze
  ) %>% 
  mutate(
    veracity = case_when(
      veracity == "truth" ~ 0,
      veracity == "lie"   ~ 1
    )
  ) %>%
  mutate(
    across(everything(), as.character)
  ) %>% 
  type_convert()

subs <- length(unique(loy$subject_nr))

network_list <- list()
poly_list    <- list()

for (i in 1:subs) {
  
  # Select appropriate sender
  
  loy_sub <- loy_select %>% 
    filter(subject_nr == unique(loy$subject_nr)[[i]]) %>% 
    select(-subject_nr)
  
  # Find and remove items with no variance
  
  retain <- apply(loy_sub, 2,
                  function(x) {
                    
                    if (length(unique(x)) == 1) {
                      
                      return(0)
                      
                    } else {
                      
                      return(1)
                      
                    }
                    
                  })
  
  loy_trunc <- loy_sub[, retain == 1]
  
  if (nrow(loy_trunc) == 0) {
    
    network_list[i] <- NA
    
  } else {
    
    poly_list[i] <- polychoric(loy_trunc)[1]
    
    net_base <- ggm(loy_trunc,
                    omega = "full")

    net_opt <- net_base %>%
      prune(
        alpha  = .10,
        adjust = "none") %>%
      modelsearch(
        prunealpha = .10,
        addalpha   = .10
      ) %>%
      runmodel()

    network_list[[i]] <- getmatrix(net_opt, "omega")

    rownames(network_list[[i]]) <- colnames(loy_trunc)
    colnames(network_list[[i]]) <- colnames(loy_trunc)
  
  }
  
}

# Visualizations for each participant

plot_list <- list()

for (i in 1:subs) {
  
    plot_list[i] <- 
      qgraph(network_list[[i]],
             layout    = "spring",
             color     = c("darkred", rep("white", ncol(network_list[[i]]) - 1)),
             filename  = paste("figures/loy_network-plot_", 
                               str_pad(i, width = 2, pad = "0"), 
                               sep = ""),
             filetype  = "png",
             height    = 5,
             width     = 5
             )
  
}

poly_plot_list <- list()

for (i in 1:subs) {
  
  poly_plot_list[i] <- 
    qgraph(poly_list[[i]],
           layout    = "spring",
           color     = c("darkred", rep("white", ncol(network_list[[i]]) - 1)),
           filename  = paste("figures/loy_polychor-plot_", 
                             str_pad(i, width = 2, pad = "0"), 
                             sep = ""),
           filetype  = "png",
           height    = 5,
           width     = 5
    )
  
}

# Group level networks

## Simple polychoric correlation matrix as a network

loy_full_poly <- polychoric(select(loy_select, -subject_nr))

loy_full_poly_plot <- 
  qgraph(loy_full_poly[[1]],
         layout    = "spring",
         color     = c("darkred", rep("white", ncol(loy_full_poly[[1]]) - 1)),
         filename  = "figures/loy_polychor-plot_full",
         filetype  = "png",
         height    = 5,
         width     = 5
  )

## Gaussaian graphical model

net_full_base <- ggm(select(loy_select, -subject_nr),
                omega = "full")

net_full_opt <- net_full_base %>%
  prune(
    alpha  = .10,
    adjust = "none") %>%
  modelsearch(
    prunealpha = .10,
    addalpha   = .10
  ) %>%
  runmodel()

network_full           <- getmatrix(net_full_opt, "omega") 
rownames(network_full) <- colnames(loy_full_poly[[1]])
colnames(network_full) <- colnames(loy_full_poly[[1]])

loy_full_ggm_plot <- 
  qgraph(network_full,
         layout    = "spring",
         color     = c("darkred", rep("white", ncol(network_full) - 1)),
         title     = "Cross-Sectional Network",
         filename  = "figures/loy_ggm-plot_full",
         filetype  = "png",
         height    = 5,
         width     = 5
  )

## Variability network

var_net <- loy_full_poly[[1]]
var_net[var_net != 1] <- NA

var_com <- expand_grid(x = colnames(var_net), y = colnames(var_net))

for (i in 1:nrow(var_com)) {
  
  var_vec <- rep(NA, subs)
  
  for (k in 1:subs) {
    
    if (var_com[i, ]$x %in% colnames(network_list[[k]]) & var_com[i, ]$y %in% colnames(network_list[[k]])) {
      
      var_vec[k] <- network_list[[k]][var_com[i, ]$x, var_com[i, ]$y]
      
    } else {
     
      var_vec[k] <- NA
      
    }
    
  }
  
  var_vec[is.na(var_vec)] <- 0
  
  var_net[var_com[i, ]$x, var_com[i, ]$y] <- sd(var_vec)
  
}

loy_variance_ggm_plot <- 
  qgraph(var_net,
         layout     = loy_full_ggm_plot$layout,
         color      = c("darkred", rep("white", ncol(network_full) - 1)),
         edge.color = "darkgrey",
         title      = "Across-Individual Variability",
         filename   = "figures/loy_ggm-variance-plot_full",
         filetype   = "png",
         height     = 5,
         width      = 5
  )

## Average Within-Subjects network

avg_net <- loy_full_poly[[1]]
avg_net[avg_net != 1] <- NA

for (i in 1:nrow(var_com)) {
  
  avg_vec <- rep(NA, subs)
  
  for (k in 1:subs) {
    
    if (var_com[i, ]$x %in% colnames(network_list[[k]]) & var_com[i, ]$y %in% colnames(network_list[[k]])) {
      
      avg_vec[k] <- network_list[[k]][var_com[i, ]$x, var_com[i, ]$y]
      
    } else {
      
      avg_vec[k] <- NA
      
    }
    
  }
  
  avg_vec[is.na(avg_vec)] <- 0
  
  avg_net[var_com[i, ]$x, var_com[i, ]$y] <- mean(avg_vec)
  
}

loy_average_ggm_plot <- 
  qgraph(avg_net,
         layout     = loy_full_ggm_plot$layout,
         color      = c("darkred", rep("white", ncol(network_full) - 1)),
         title      = "Across-Individual Average Network",
         filename   = "figures/loy_ggm-average-plot_full",
         filetype   = "png",
         height     = 5,
         width      = 5
  )

# Create grid plots

network_names <- paste("figures/loy_network-plot_", 
                        str_pad(1:subs, width = 2, pad = "0"),
                        ".png",
                        sep = "")

for (i in 1:length(network_names)) {
  
  assign(paste("network_plot_", i, sep = ""),
         readPNG(network_names[i]))
  
}

png("./figures/loy_network-grid.png", 
    height = 12, width = 12, units = "in", res = 1500)
grid.arrange(grobs = 
               map(paste("network_plot_", 1:subs, sep = ""), 
                   function(x) { rasterGrob(get(x))}),
             nrow = 4)
dev.off()

polychor_names <- paste("figures/loy_polychor-plot_", 
                        str_pad(1:subs, width = 2, pad = "0"),
                        ".png",
                        sep = "")

for (i in 1:length(polychor_names)) {
  
  assign(paste("polychor_plot_", i, sep = ""),
         readPNG(polychor_names[i]))
  
}

png("./figures/loy_polychor-grid.png", 
    height = 12, width = 12, units = "in", res = 1500)
grid.arrange(grobs = 
               map(paste("polychor_plot_", 1:subs, sep = ""), 
                 function(x) { rasterGrob(get(x))}),
             nrow = 4)
dev.off()

png("./figures/loy_ggm-full-figure.png", 
    height = 12, width = 12, units = "in", res = 1500)
grid.arrange(grobs = 
               map(c("figures/loy_ggm-plot_full.png",
                     "figures/loy_ggm-average-plot_full.png",
                     "figures/loy_ggm-variance-plot_full.png",
                     "figures/loy_network-grid.png"), 
                   function(x) { rasterGrob(readPNG(x))}),
             nrow = 2)
dev.off()

png("./figures/loy_ggm-summary-figure.png", 
    height = 12, width = 12, units = "in", res = 1500)
grid.arrange(grobs = 
               map(c("figures/loy_ggm-plot_full.png",
                     "figures/loy_ggm-average-plot_full.png",
                     "figures/loy_ggm-variance-plot_full.png"), 
                   function(x) { rasterGrob(readPNG(x))}),
             nrow = 2, ncol = 2)
dev.off()
