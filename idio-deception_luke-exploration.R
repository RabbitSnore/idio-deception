################################################################################

# An Idiographic Approach to Deception Cues - Luke (2015)

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(cowplot)

# Load data --------------------------------------------------------------------

study_2 <- read_csv("data/luke-2015_study-2.csv")

# Wrangle ----------------------------------------------------------------------

study_2_long <- study_2 %>% 
  pivot_longer(
    cols = c("details1a", "details1b", "details1c", 
             "details2a", "details2b", "details2c", 
             "details3a", "details3b", "details3c"),
    names_to = "phase",
    values_to = "details"
  ) %>% 
  mutate(
    Veracity = case_when(
      phase %in% c("details3a", "details3b", "details3c") ~ Veracity,
      phase %in% c("details2a", "details2b", "details2c") ~ 0,
      phase %in% c("details1a", "details1b", "details1c") ~ 0
    )
  )

# Modeling ---------------------------------------------------------------------

# Estimate mixed effects models

details_2_lmm_uncon <- lmer(details 
                            ~ 1 
                            + (1|ID)
                            + (1|phase), 
                            data = study_2_long)

details_2_icc <- icc(details_2_lmm_uncon, by_group = TRUE)

details_2_lmm_ver   <- lmer(details 
                            ~ 1
                            + Veracity
                            + (1 + Veracity|ID)
                            + (1 + Veracity|phase), 
                            data = study_2_long)

# Extract coefficients

details_2_coef_id    <- coef(details_2_lmm_ver)$ID
details_2_coef_phase <- coef(details_2_lmm_ver)$phase

# Visualization ----------------------------------------------------------------

luke_ri_id_2_plot <- 
  ggplot(details_2_coef_id,
         aes(
           x = `(Intercept)`
         )) +
  geom_histogram(
    binwidth = 1,
    fill = "darkgrey",
    color = "black"
  ) +
  labs(
    x = "Random Intercept for Details (Participants)",
    y = "Count"
  ) +
  theme_classic()

luke_rs_id_2_plot <- 
  ggplot(details_2_coef_id,
         aes(
           x = Veracity
         )) +
  geom_histogram(
    binwidth = .25,
    fill = "darkgrey",
    color = "black"
  ) +
  labs(
    x = "Random Slope for Deception (Participants)",
    y = "Count"
  ) +
  theme_classic()

luke_ri_phase_2_plot <- 
  ggplot(details_2_coef_phase,
         aes(
           x = `(Intercept)`
         )) +
  geom_histogram(
    binwidth = 0.75,
    fill = "darkgrey",
    color = "black"
  ) +
  labs(
    x = "Random Intercept for Details (Interview Phases)",
    y = "Count"
  ) +
  theme_classic()

luke_rs_phase_2_plot <- 
  ggplot(details_2_coef_phase,
         aes(
           x = Veracity
         )) +
  geom_histogram( 
    binwidth = .75,
    fill = "darkgrey",
    color = "black"
  ) +
  labs(
    x = "Random Slope for Deception (Interview Phases)",
    y = "Count"
  ) +
  theme_classic()

# Export images ----------------------------------------------------------------

hist_grid <- plot_grid(luke_ri_id_2_plot, luke_rs_id_2_plot, 
                       luke_ri_phase_2_plot, luke_rs_phase_2_plot,
                       nrow = 2)

save_plot("figures/luke_details-slopes_hist.png", hist_grid,
          base_height = 7, base_width = 8)

