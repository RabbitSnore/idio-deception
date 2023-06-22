################################################################################

# An Idiographic Approach to Deception Cues - Calderon et al (2022)

################################################################################

library(tidyverse)
library(readxl)
library(lme4)
library(cowplot)
library(performance)
library(metafor)
library(osfr)

# Load data --------------------------------------------------------------------

if (!dir.exists("data/")) {
  
  dir.create("data/")
  
}

if (!file.exists("data/responses.csv")) {
  
  osf_retrieve_file("5c7fa2c758e63b0019d6e042") %>% 
    osf_download(path = "data/", 
                 conflicts = "overwrite")
  
}

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
                        n1i = n_1, n2i = n_2) %>% 
  mutate(
    yi = yi * -1
  )

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

calderon_stationarity_plot <- 
ggplot(question_meta,
       aes(
         x     = question,
         y     = yi,
         color = study,
         group = study
       )) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2
  ) +
  geom_line(
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("#221D23",
               "#4F3824",
               "#D1603D",
               "#DDB967",
               "#D0E37F",
               "#0B7A75")
  ) +
  scale_y_continuous(
    breaks = seq(-.80, .80, .10)
  ) +
  scale_x_discrete(
    labels = NULL
  ) +
  guides(
    color = "none"
  ) +
  labs(
    x = "Interview Question",
    y = "Effect of Deception (g)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 8)
  )

save_plot("figures/calderon_stationarity-plot.png", calderon_stationarity_plot,
          base_width = 10, base_height = 3.5)
