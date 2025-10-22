# Load packages
library(lme4)
library(lmtest)
library(dplyr)
library(kableExtra)

model_Ha <- lmer(First_Summer_ALF_Biomass_sqrt ~ selected_cat*LOC + (1 | Entry) + (1|LOC:Rep), data=data2, REML=FALSE) # alternate hypothesis : intercropping selection influences biomass

model_H0 <- lmer(First_Summer_ALF_Biomass_sqrt ~ 1 + (1 | Entry) + (1|LOC:Rep), data=data2, REML=FALSE) # null hypothesis : selection doesn't matter to biomass

anova(model_H0, model_Ha)
# Pr(>Chisq) = 0.08768
# Alternative model with selection does not significantly improve model fit
# Being selected for intercropping does not significantly affect First_Summer_ALF_Biomass (after accounting for Entry, Location, and Rep)

LRT <- lrtest(model_H0, model_Ha)
LRT

emmeans(model_Ha, pairwise ~ selected_cat)
plot(emmeans(model_Ha, ~ selected_cat))

# Format the table
LRT_table <- LRT %>%
  as.data.frame() %>%
  rename(
    "df" = "Df",
    "Log-Likelihood" = "LogLik",
    "Chi-square" = "Chisq",
    "p-value" = "Pr(>Chisq)"
  )

kbl(LRT_table, digits = 15, caption = "Likelihood Ratio Test comparing null and alternative models for selection status significance") %>%
  kable_classic(full_width = F, html_font = "Helvetica")



library(emmeans)

emm <- emmeans(model_Ha, ~ selected_cat)

emm_table <- as.data.frame(emm)

kbl(emm_table, digits = 3, caption = "Table 2. Estimated marginal means of first-summer alfalfa biomass by selection category.") %>%
  kable_classic(full_width = F, html_font = "Helvetica")


library(ggplot2)

emm_df <- as.data.frame(emm)

ggplot(emm_df, aes(x = selected_cat, y = emmean, fill = selected_cat)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Estimated Marginal Means of First-Summer Alfalfa Biomass",
    subtitle = "Selection for intercropping does not significantly affect biomass",
    x = "Selection Category",
    y = "Biomass sqrt(kg/ha)"
  ) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "orange")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

