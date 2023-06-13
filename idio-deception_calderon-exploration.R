################################################################################

# An Idiographic Approach to Deception Cues - Calderon et al data set

################################################################################

library(tidyverse)
library(readxl)
library(lme4)
library(cowplot)
library(performance)
library(metafor)

# Load data --------------------------------------------------------------------

combined <- read_csv("data/responses.csv")

# Wrangle ----------------------------------------------------------------------

question_data <- combined %>% 
  group_by(question, veracity) %>% 
  summarise(
    mean_wordcount   = mean(length, na.rm = TRUE),
    sd_wordcount     = sd(length, na.rm = TRUE),
    median_wordcount = median(length, na.rm = TRUE),
    n                = n()
  ) %>% 
  pivot_wider(
    id_cols     = "question",
    names_from  = "veracity",
    values_from = c(ends_with("wordcount"), "n")
  ) %>% 
  extract(
    col    = "question",
    into   = "study",
    regex  = "(.*)_.*",
    remove = FALSE
  ) %>% 
  filter(complete.cases(question))

question_meta <- escalc(measure = "SMD", data = question_data,
                        m1i = mean_wordcount_1, m2i = mean_wordcount_2,
                        sd1i = sd_wordcount_1, sd2i = sd_wordcount_2,
                        n1i = n_1, n2i = n_2)

vrij_2011 <- combined %>% 
  filter(study == "Vrij2011b")

# Modeling ---------------------------------------------------------------------

# Word Count

wordcount_lmm_base <- lmer(
  length
  ~ 1
  + (1|subject)
  + (1|question),
  data = combined
)

wordcount_icc <- icc(wordcount_lmm_base, by_group = TRUE)

question_rma     <- rma.mv(yi = yi, 
                       V = vi, 
                       random = ~ 1|study, 
                       data = question_meta)

question_rma_mod <- rma.mv(yi = yi, 
                           V = vi, 
                           mods = ~ scale(mean_wordcount_1, scale = FALSE),
                           random = ~ 1|study, 
                           data = question_meta)

question_rma_100 <- rma.mv(yi = yi, 
                           V = vi, 
                           mods = ~ scale(mean_wordcount_1, 
                                          scale = FALSE,
                                          center = 100),
                           random = ~ 1|study, 
                           data = question_meta)

question_rma_15  <- rma.mv(yi = yi, 
                           V = vi, 
                           mods = ~ scale(mean_wordcount_1, 
                                          scale = FALSE,
                                          center = 15),
                           random = ~ 1|study, 
                           data = question_meta)

## Within-person analysis

wordcount_lmm_vrij <- lmer(
  length
  ~ 1
  + (1|subject)
  + (1|question),
  data = vrij_2011
)

wordcount_vrij_icc <- icc(wordcount_lmm_vrij, by_group = TRUE)

wordcount_lmm_vrij_veracity <- lmer(
  length
  ~ 1
  + veracity
  + (1 + veracity|subject)
  + (1 + veracity|question),
  data = vrij_2011
)

lmm_vrij_coef <- coef(wordcount_lmm_vrij_veracity)

vrij_2011_wordcount_ind <- vrij_2011 %>% 
  pivot_wider(
    id_cols = c("subject", "question"),
    names_from = "veracity",
    names_prefix = "message_",
    values_from = "length"
  ) %>%
  mutate(
    diff_wordcount = (message_1 - message_2)/2,
    base_wordcount = message_1
  )

# Visualization ----------------------------------------------------------------

word_cue_questions <- 
ggplot(question_meta,
       aes(
         y = yi,
         x = mean_wordcount_1
       )) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x"
  ) +
  geom_point() +
  geom_vline(
    xintercept = mean(question_data$mean_wordcount_1),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  scale_y_continuous(
    breaks = seq(-0.80, 0.80, 0.10)
  ) +
  labs(
    x     = "Average Truthful Word Count",
    y     = "Average Effect of Deception (g)"
  ) +
  theme_classic()

ggplot(lmm_vrij_coef$subject,
       aes(
         x = veracity
       )) +
  geom_histogram(
    binwidth = 1.5,
    color    = "black",
    fill     = "grey"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  theme_classic()

vrij_2011_wordcount_plot <- 
ggplot(vrij_2011_wordcount_ind,
       aes(
         x = base_wordcount,
         y = diff_wordcount
       )) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x"
  ) +
  geom_point() +
  geom_vline(
    xintercept = mean(vrij_2011$length),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  scale_y_continuous(
  ) +
  labs(
    x = "Average Truthful Word Count",
    y = "Average Effect of Deception (words)"
  ) +
  theme_classic()

vrij_2011_wordcount_sub_plot <- 
ggplot(vrij_2011_wordcount_ind,
       aes(
         x = base_wordcount,
         y = diff_wordcount
       )) +
  geom_smooth(
    aes(group = as.factor(subject)),
    method = "lm",
    formula = "y ~ x",
    se = FALSE,
    linewidth = .05,
    color = "lightblue"
  ) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x"
  ) +
  geom_point() +
  geom_vline(
    xintercept = mean(vrij_2011$length),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  scale_y_continuous(
  ) +
  labs(
    x = "Average Truthful Word Count",
    y = "Average Effect of Deception (words)"
  ) +
  guides(
    color = "none"
  ) +
  theme_classic()
