library(tidyverse)
library(ggpubr)
library(rstatix)


weightloss <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/THREE/three_way_repeated.csv')
weightloss

############################### Summary statistics
weightloss %>%
  group_by(diet, exercises, time) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  weightloss, x = "exercises", y = "score",
  color = "time", palette = "jco",
  facet.by = "diet", short.panel.labs = FALSE
)
bxp

############################### Check assumptions

################################# Outliers
weightloss %>%
  group_by(diet, exercises, time) %>%
  identify_outliers(score)

################################# Normality assumption
weightloss %>%
  group_by(diet, exercises, time) %>%
  shapiro_test(score)


ggqqplot(weightloss, "score", ggtheme = theme_bw(),
         color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  facet_grid(diet + exercises ~ time, labeller = "label_both")

################################# Computation
res.aov <- anova_test(
  data = weightloss, dv = score, wid = id,
  within = c(diet, exercises, time)
)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

############## simple two-way interaction
# Two-way ANOVA at each diet level
two.way <- weightloss %>%
  group_by(diet) %>%
  anova_test(dv = score, wid = id, within = c(exercises, time))
two.way

# Extract anova table
get_anova_table(two.way)

############## Compute simple main effects
# Effect of time at each diet X exercises cells
time.effect <- weightloss %>%
  group_by(diet, exercises) %>%
  anova_test(dv = score, wid = id, within = time)
time.effect

# Extract anova table
get_anova_table(time.effect) %>%
  filter(diet == "no")

############# Compute pairwise comparisons
# Pairwise comparisons
pwc <- weightloss %>%
  group_by(diet, exercises) %>%
  pairwise_t_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic) # Remove details
# Show comparison results for "diet:no,exercises:yes" groups
pwc %>% filter(diet == "no", exercises == "yes") %>%
  select(-p)     # remove p columns

s
############################################## Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "exercises")
pwc.filtered <- pwc %>% 
  filter(diet == "no", exercises == "yes")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

