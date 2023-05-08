################################################################################

# An Idiographic Approach to Deception Cues - Miami Data

################################################################################

library(tidyverse)
library(readxl)
library(lme4)

# Load data --------------------------------------------------------------------

mu3d <- read_xlsx("data/MU3D Codebook.xlsx", sheet = 2)

concreteness <- read_xlsx("data/ConcreteDictionary.xlsx")

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

# Concreteness ratings

mu3d_text <- mu3d$transcription

mu3d_text <- str_remove_all(mu3d_text, "[[:punct:]]")

mu3d_text_split <- str_split(mu3d_text, " ")

mu3d_text_coded <- 
  map(mu3d_text_split, function(x) {
    
    map(x, function(y) {
      
      concreteness[concreteness$Word == y, ]$Conc.M
      
    })
    
  })

mu3d_text_conc_means <- 
  map(mu3d_text_coded, function(x) {
    
    mean(unlist(x), na.rm = TRUE)
    
  }) %>% 
  unlist()

mu3d$concreteness <- mu3d_text_conc_means

subject_mean_concreteness <- mu3d %>% 
  group_by(id) %>% 
  summarise(
    mean_concreteness = mean(concreteness)
  )

mu3d$id <- factor(
  mu3d$id, 
  levels = arrange(subject_mean_concreteness, desc(mean_concreteness))$id)

subject_mean_concreteness$id <- factor(
  subject_mean_concreteness$id, 
  levels = arrange(subject_mean_concreteness, desc(mean_concreteness))$id)

concreteness_table <- mu3d %>% 
  group_by(veracity) %>% 
  summarise(
    mean_concreteness = mean(concreteness),
    sd                = sd(concreteness),
    se                = sd/sqrt(n()),
    ci_lb             = mean_concreteness - se*qnorm(.975),
    ci_ub             = mean_concreteness + se*qnorm(.975)
  )

# Statistical modeling ---------------------------------------------------------

# Word count

wordcount_lmm_base <- lmer(
  wordcount 
  ~ 1
  + veracity
  + (1|id),
  data = mu3d
)

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

# Concreteness

concreteness_lmm_base <- lmer(
  concreteness 
  ~ 1
  + veracity
  + (1|id),
  data = mu3d
)

concreteness_lmm_slopes <- lmer(
  concreteness 
  ~ 1
  + veracity
  + (1 + veracity|id),
  data = mu3d
)

# Visualization ----------------------------------------------------------------

ggplot(wordcount_table,
       aes(
         y = mean_wordcount,
         x = veracity,
         group = 1,
         ymax = ci_ub,
         ymin = ci_lb
       )) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    linewidth = 1,
    width = .25
  ) +
  theme_classic()

ggplot(mu3d,
       aes(
         y = wordcount,
         x = veracity,
         group = id_valence,
         color = as.factor(valence)
       )) +
  facet_wrap(~ id, nrow = 8) +
  geom_line(
    linewidth = 1,
    alpha     = .5
  ) +
  geom_hline(
    data = subject_mean_wordcount,
    aes(
      yintercept = mean_wordcount
    ),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(mu3d$wordcount),
    linetype   = "dashed"
  ) +
  guides(
    color = "none"
  ) +
  theme_classic()

ggplot(mu3d,
       aes(
         x = veracity0
       )) +
  geom_histogram(
    binwidth = 0.50
  ) +
  theme_classic()

ggplot(mu3d,
       aes(
         x = intercept_wordcount,
         y = veracity_wordcount
       )) +
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
    breaks = seq(-10, 10, 1)
  ) +
  labs(
    x = "Intercept (Word Count)",
    y = "Effect of Deception"
  ) +
  theme_classic()

