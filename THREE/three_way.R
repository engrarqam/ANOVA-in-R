library(tidyverse)
library(ggpubr)
library(rstatix)

headache <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/THREE/three_way.csv')
headache

############################### Summary statistics
headache %>%
  group_by(gender, risk, treatment) %>%
  get_summary_stats(pain_score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  headache, x = "treatment", y = "pain_score", 
  color = "risk", palette = "jco", facet.by = "gender"
)
bxp

ggline(headache, x = "treatment", y = "pain_score",
       color = "risk", palette = "jco", facet.by = "gender")
############################### Check assumptions

################################# Outliers
headache %>%
  group_by(gender, risk, treatment) %>%
  identify_outliers(pain_score)

################################# Normality assumption
model  <- lm(pain_score ~ gender*risk*treatment, data = headache)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

headache %>%
  group_by(gender, risk, treatment) %>%
  shapiro_test(pain_score)

ggqqplot(headache, "pain_score", ggtheme = theme_bw(),
         color = "treatment", palette = "jco") +
  facet_grid(gender + risk ~ treatment, labeller = "label_both")


################################# Homogneity of variance assumption
headache %>% levene_test(pain_score ~ gender*risk*treatment)

################################# Computation
res.aov <- headache %>% anova_test(pain_score ~ gender*risk*treatment)
res.aov

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

# Group the data by gender and 
# fit simple two-way interaction 
model  <- lm(pain_score ~ gender*risk*treatment, data = headache)
headache %>%
  group_by(gender) %>%
  anova_test(pain_score ~ risk*treatment, error = model)


############## Compute simple main effects

# Group the data by gender and risk, and fit  anova
treatment.effect <- headache %>%
  group_by(gender, risk) %>%
  anova_test(pain_score ~ treatment, error = model)
treatment.effect

############# Compute simple comparisons
#Compare the different treatments by gender and risk variables:
# Pairwise comparisons
library(emmeans)
pwc <- headache %>%
  group_by(gender, risk) %>%
  emmeans_test(pain_score ~ treatment, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p) # Remove details
# Show comparison results for male at high risk
pwc %>% filter(gender == "male", risk == "high")

# Estimated marginal means (i.e. adjusted means) 
# with 95% confidence interval
get_emmeans(pwc) %>% filter(gender == "male", risk == "high")



############################################## Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "treatment")
pwc.filtered <- pwc %>% filter(gender == "male", risk == "high")
bxp +
  stat_pvalue_manual(
    pwc.filtered, color = "risk", linetype = "risk", hide.ns = TRUE,
    tip.length = 0, step.increase = 0.1, step.group.by = "gender"
  ) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
