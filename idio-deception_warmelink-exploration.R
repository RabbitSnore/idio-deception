################################################################################

# An Idiographic Approach to Deception Cues - Warmelink & O'Connell (2023)

################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(metafor)

# Load data --------------------------------------------------------------------

# Data for this study can be retrieved from https://osf.io/9fne8/

if (!dir.exists("data/")) {
  
  dir.create("data/")
  
}

if (!file.exists("data/Execution Study Long Format.csv")) {
  
  osf_retrieve_file("62ab1f83a065d90776d173c6") %>% 
    osf_download(path = "data/", 
                 conflicts = "overwrite")
  
}

execution_data <- read_csv("data/Execution Study Long Format.csv")

# Wrangle ----------------------------------------------------------------------

colnames(execution_data) <- str_replace_all(colnames(execution_data), " ", "_")
colnames(execution_data) <- tolower(colnames(execution_data))

# Description of data ----------------------------------------------------------

question_detail_table <- execution_data %>% 
  group_by(question) %>% 
  summarise(
    mean   = mean(total_details),
    sd     = sd(total_details),
    median = median(total_details)
  )

question_detail_veracity_table <- execution_data %>% 
  group_by(question, veracity) %>% 
  summarise(
    mean   = mean(total_details),
    sd     = sd(total_details),
    median = median(total_details),
    n      = n()
  )

mean_details_wide <- question_detail_veracity_table %>% 
  pivot_wider(
    id_cols     = "question",
    values_from = c("mean", "sd", "n"),
    names_from  = "veracity",
  )

mean_details_wide <- mean_details_wide %>% 
  escalc(
    measure = "SMD",
    m1i  = mean_0,
    m2i  = mean_1,
    sd1i = sd_0,
    sd2i = sd_1,
    n1i  = n_0,
    n2i  = n_1,
    data = .) %>% 
  mutate(
    ci_lb = yi - sqrt(vi)*qnorm(.975),
    ci_ub = yi + sqrt(vi)*qnorm(.975)
  )

# Analysis ---------------------------------------------------------------------

detail_uncon_lmm <- lmer(total_details 
                         ~ 1
                         + (1|question) 
                         + (1|participant), 
                         data = execution_data)

detail_icc <- icc(detail_uncon_lmm, by_group = TRUE)

detail_base_lmm  <- lmer(total_details 
                         ~ 1
                         + veracity
                         + (1 + veracity|question) 
                         + (1|participant), 
                         data = execution_data)

details_coef <- coef(detail_base_lmm)$question

detail_rma <- rma(yi = yi, vi = vi, data = mean_details_wide)
