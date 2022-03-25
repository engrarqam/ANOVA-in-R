library(tidyverse)
library(ggpubr)
library(rstatix)


selfesteem <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/ONE/one_way_repeated.csv')
selfesteem

############################### Summary statistics
selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(selfesteem, x = "time", y = "score", 
          color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("t1", "t2", "t3"),
          ylab = "score", xlab = "time")
bxp

############################### Check assumptions

################################# Outliers
selfesteem %>% 
  group_by(time) %>%
  identify_outliers(score)

ggqqplot(selfesteem, "score", facet.by = "time", 
         color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         ylab = "score", xlab = "time")

################################# Normality assumption
selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)

ggqqplot(selfesteem, "score", facet.by = "time", 
         color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         ylab = "score", xlab = "group")

################################# Assumption of sphericity
#automatically checked during the computation of the ANOVA test using the R function anova_test() [rstatix package]

################################# Computation
res.aov <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
get_anova_table(res.aov)

################################# Post-hoc tests
# pairwise comparisons
pwc <- selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


################################# Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

