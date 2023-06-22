################################################################################

# An Idiographic Approach to Deception Cues - MU3D (Lloyd et al, 2017)

################################################################################

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(cowplot)
library(performance)

# Load data --------------------------------------------------------------------

# Accessing these data requires agreement to terms and conditions
# The data can be accessed here: https://sc.lib.miamioh.edu/handle/2374.MIA/6067

mu3d <- read_xlsx("data/MU3D Codebook.xlsx", sheet = 2)

# Wrangle ----------------------------------------------------------------------

mu3d <- mu3d %>% 
  extract(
    col   = VideoID,
    into  = c("id", "type"),
    regex = "(.*)_(.*)",
    remove = FALSE
  )

colnames(mu3d) <- tolower(colnames(mu3d))

mu3d$id_valence <- paste(mu3d$id, mu3d$valence, sep = "_")

subject_mean_wordcount <- mu3d %>% 
  group_by(id) %>% 
  summarise(
    mean_wordcount = mean(wordcount)
  )

mu3d$id <- factor(
  mu3d$id, 
  levels = arrange(subject_mean_wordcount, desc(mean_wordcount))$id)

subject_mean_wordcount$id <- factor(
  subject_mean_wordcount$id, 
  levels = arrange(subject_mean_wordcount, desc(mean_wordcount))$id)

mu3d$veracity <- factor(
  mu3d$veracity,
  levels = c(1, 0))

wordcount_table <- mu3d %>% 
  group_by(veracity) %>% 
  summarise(
    mean_wordcount = mean(wordcount),
    sd             = sd(wordcount),
    se             = sd/sqrt(n()),
    ci_lb          = mean_wordcount - se*qnorm(.975),
    ci_ub          = mean_wordcount + se*qnorm(.975)
  )

mu3d_wordcount_ind <- mu3d %>% 
  pivot_wider(
    id_cols = "id",
    names_from = c("veracity", "valence"),
    names_prefix = "message_",
    values_from = "wordcount"
  ) %>%
  mutate(
    diff_wordcount = ((message_1_1 - message_0_1) + (message_1_0 - message_0_0))/2,
    base_wordcount = (message_1_1 + message_1_0)/2
  ) %>% 
  left_join(
    select(mu3d, id, sex, race),
    by = "id"
  )

# Statistical modeling ---------------------------------------------------------

# Word count

wordcount_lmm_base <- lmer(
  wordcount 
  ~ 1
  + (1|id),
  data = mu3d
)

wordcount_icc <- icc(wordcount_lmm_base, by_group = TRUE)

wordcount_lmm_slopes <- lmer(
  wordcount 
  ~ 1
  + veracity
  + (1 + veracity|id),
  data = mu3d
)

wordcount_lmm_slope_data    <- as.data.frame(coef(wordcount_lmm_slopes)$id)
wordcount_lmm_slope_data$id <- rownames(wordcount_lmm_slope_data)

mu3d <- mu3d %>% 
  left_join(wordcount_lmm_slope_data, by = "id") %>% 
  mutate(
    intercept_wordcount = `(Intercept)`,
    veracity_wordcount  = veracity0
  ) %>% 
  select(-`(Intercept)`, -veracity0)

# Visualization ----------------------------------------------------------------

## Baseline word count by deception effect

word_cue_base <- 
ggplot(mu3d_wordcount_ind,
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
    xintercept = mean(mu3d$wordcount),
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

word_cue_race <- 
ggplot(mu3d_wordcount_ind,
       aes(
         x = base_wordcount,
         y = diff_wordcount,
         color = as.factor(race)
       )) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x"
  ) +
  geom_point() +
  geom_vline(
    xintercept = mean(mu3d$wordcount),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  scale_y_continuous(
    breaks = seq(-30, 80, 10)
  ) +
  scale_color_manual(
    values = c("#FFBC42", "#D81159"),
    labels = c("Black", "White")
  ) +
  labs(
    x     = "Average Truthful Word Count",
    y     = "Average Effect of Deception (words)",
    color = "Race"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

word_cue_gender <- 
ggplot(mu3d_wordcount_ind,
       aes(
         x = base_wordcount,
         y = diff_wordcount,
         color = as.factor(sex)
       )) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x"
  ) +
  geom_point() +
  geom_vline(
    xintercept = mean(mu3d$wordcount),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  scale_y_continuous(
    breaks = seq(-30, 80, 10)
  ) +
  scale_color_manual(
    values = c("#406E8E", "#FF5154"),
    labels = c("Female", "Male")
  ) +
  labs(
    x     = "Average Truthful Word Count",
    y     = "Average Effect of Deception (words)",
    color = "Gender"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

word_cue_grid <- 
plot_grid(word_cue_base, word_cue_race, word_cue_gender,
          nrow = 1, align = "h", axis = "b")

save_plot("figures/mu3d_word-count_grid.png", word_cue_grid,
          base_height = 4, base_width = 10)
