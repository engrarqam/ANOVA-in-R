library(tidyverse)
library(ggpubr)
library(rstatix)

jobsatisfaction <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/TWO/two_way.csv')
jobsatisfaction

############################### Summary statistics
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  jobsatisfaction, x = "gender", y = "score",
  color = "education_level", palette = "jco"
)
bxp

ggline(jobsatisfaction, x = "education_level", y = "score",
       color = "gender", palette = c("#00AFBB", "#E7B800"))
############################### Check assumptions

################################# Outliers
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  identify_outliers(score)

################################# Normality assumption
# Check normality assumption by analyzing the model residuals

# Build the linear modelS

model  <- lm(score ~ gender*education_level,
             data = jobsatisfaction)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

# Check normality assumption by groups.
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  shapiro_test(score)

ggqqplot(jobsatisfaction, "score", ggtheme = theme_bw(), 
         color = "education_level", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  facet_grid(gender ~ education_level)

################################# Homogneity of variance assumption
jobsatisfaction %>% levene_test(score ~ gender*education_level)

################################# Computation
res.aov <- jobsatisfaction %>% anova_test(score ~ gender * education_level)
res.aov

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

############## Compute simple main effects

# Group the data by gender and fit  anova
model <- lm(score ~ gender * education_level, data = jobsatisfaction)
jobsatisfaction %>%
  group_by(gender) %>%
  anova_test(score ~ education_level, error = model)
############# Compute pairwise comparisons
#Compare the score of the different education levels by gender levels:
# pairwise comparisons
library(emmeans)
pwc <- jobsatisfaction %>% 
  group_by(gender) %>%
  emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 
pwc

############################# Procedure for Non-Significant two-way interaction
#Inspect main effects
res.aov

#Pairwise t-test:
jobsatisfaction %>%
  pairwise_t_test(
    score ~ education_level, 
    p.adjust.method = "bonferroni"
  )

#Pairwise comparisons using Emmeans test:
model <- lm(score ~ gender * education_level, data = jobsatisfaction)
jobsatisfaction %>% 
  emmeans_test(
    score ~ education_level, p.adjust.method = "bonferroni",
    model = model
  )


############################################## Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "gender")
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

