library(tidyverse)
library(ggpubr)
library(rstatix)


selfesteem2 <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/TWO/two_way_repeated.csv')
selfesteem2

############################### Summary statistics
selfesteem2 %>%
  group_by(treatment, time) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  selfesteem2, x = "time", y = "score",
  color = "treatment", palette = "jco"
)
bxp

############################### Check assumptions

################################# Outliers
selfesteem2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)

################################# Normality assumption
selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

ggqqplot(selfesteem2, "score", ggtheme = theme_bw(), 
         color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  facet_grid(time ~ treatment, labeller = "label_both")

################################# Assumption of sphericity
#automatically checked during the computation of the ANOVA test using the R function anova_test() [rstatix package]

################################# Computation
res.aov <- anova_test(
  data = selfesteem2, dv = score, wid = id,
  within = c(treatment, time)
)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

############## Compute simple main effects
# Effect of treatment at each time point
one.way <- selfesteem2 %>%
  group_by(time) %>%  anova_test(dv = score, wid = id, within = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

############# Compute pairwise comparisons
# Pairwise comparisons between treatment groups
pwc <- selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


# Effect of time at each level of treatment
one.way2 <- selfesteem2 %>%
  group_by(treatment) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
# Pairwise comparisons between time points
pwc2 <- selfesteem2 %>%
  group_by(treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


############################# Procedure for Non-Significant two-way interaction
# Pairwise paired t-test comparisons:

# comparisons for treatment variable
selfesteem2 %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# comparisons for time variable
selfesteem2 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )


############################################## Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

