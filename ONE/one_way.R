library(tidyverse)
library(ggpubr)
library(rstatix)


PlantGrowth <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANOVA/ONE/one_way.csv')
PlantGrowth

############################### Summary statistics
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")

############################### Visualization
ggboxplot(PlantGrowth, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "weight", xlab = "group")

############################### Check assumptions

################################# Outliers
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)

ggqqplot(PlantGrowth, "weight", facet.by = "group", 
         color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         ylab = "weight", xlab = "group")

################################# Normality assumption
PlantGrowth %>%
  group_by(group) %>%
  shapiro_test(weight)

# Check normality assumption by analyzing the model residuals

# Build the linear modelS
model  <- lm(weight ~ group, data = PlantGrowth)
model

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

# Check normality assumption by groups.
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(PlantGrowth)

ggqqplot(PlantGrowth, "weight", facet.by = "group", 
         color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         ylab = "weight", xlab = "group")

################################# Homogneity of variance assumption
plot(model, 1)
PlantGrowth %>% levene_test(weight ~ group)

################################# Computation
res.aov <- PlantGrowth %>% anova_test(weight ~ group)
res.aov

################################# Post-hoc tests
# Pairwise comparisons
pwc <- PlantGrowth %>% tukey_hsd(weight ~ group)
pwc

################################# Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "group")
ggboxplot(PlantGrowth, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


################################# Relaxing the homogeneity of variance assumption

# Welch One way ANOVA test
res.aov2 <- PlantGrowth %>% welch_anova_test(weight ~ group)

# Pairwise comparisons (Games-Howell)
pwc2 <- PlantGrowth %>% games_howell_test(weight ~ group)

# Visualization: box plots with p-values
pwc2 <- pwc2 %>% add_xy_position(x = "group", step.increase = 1)
ggboxplot(PlantGrowth, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"))+
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

########## You can also perform pairwise comparisons using pairwise t-test with no assumption of equal variances:
pwc3 <- PlantGrowth %>% 
  pairwise_t_test(
    weight ~ group, pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc3













