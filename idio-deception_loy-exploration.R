################################################################################

# An Idiographic Approach to Deception Cues - Loy, Corley, & Rohde (2018)

################################################################################

library(tidyverse)
library(readxl)
library(lme4)
library(cowplot)
library(performance)
library(metafor)

# Load data --------------------------------------------------------------------

# Data are available here

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
