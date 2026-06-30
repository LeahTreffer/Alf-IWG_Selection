
# - No differences of entries in the first summer (same across locations)
# - see some things we expect:
#   - slower regrowth of the falcata populations

# For NY: 
#   - alfalfa biomass in the first fall was different by entry and treatment but not their interaction
# - alfalfa biomass in the first fall was AVC9 (a selection made from maize in WI) was higher than IAFAL-C3 (a falcata type) which was lower than several others; however, AVC9 is not significantly different than the base population it was selected from
# - alfalfa biomass in the first fall was different by treatment (alfalfa in intercrop higher biomass than in monoculture)
# - but no differences/effect of alfalfa entry on the IWG or weeds

library(corrplot)
library(ggrepel)
library(ggplot2)
library(multcompView)
library(lmerTest)
library(kableExtra)
library(Kendall)
library(lme4)
library("PerformanceAnalytics")
library(dplyr)
library(tidyverse)
library(psych)
library(readxl) 
library(emmeans)
library(multcompView)
library(multcomp)
library(here)

data <- read.csv(here::here("NewYork","data","Alf-IWG alfalfa selection MASTER - NY_MG2.csv"))

data$YearCode <- as.factor(data$YearCode)
data$Harvest <- as.factor(data$Harvest)
data$ENT <- as.character(data$ENT)
data$Entry <- as.character(data$Entry)
data$Block <- as.factor(data$Block)
data$treatment <- as.factor(data$treatment)
data$Serpentine <- as.character(data$Serpentine)

data_inter <- subset(data, treatment == "intercrop")
  
model_SprVigor <- lmer(Alfalfa_SpringVigor ~ Entry*YearCode + (1|Block)+(1|Serpentine:Block), data=data_inter)
aov.table <- anova(model_SprVigor)

#estimate how much of the biomass variation is attributable to differences among entries
model_SmrBms <- lmer(`Alfalfa_Summer_Biomass.g.` ~ 1 + (1|Block)+(1|Entry), data=data_inter)

## whether entries differ ##
# Alfalfa_Summer_Biomass.g.
model_SmrBms <- lmer(`Alfalfa_Summer_Biomass.g.` ~ Entry + (1|Block), data=data_inter)
anova(model_SmrBms)
emm <- emmeans(model_SmrBms, ~ Entry, adjust = "tukey")
pairs_df <- as.data.frame(emm, adjust = "tukey") # Extract pairwise results table
sig_pairs <- pairs_df %>%
  filter(p.value < 0.07) %>%
  arrange(p.value)
# Print nicely formatted table
kbl(sig_pairs, digits = 5, caption = "Significant pairwise comparisons of Entry (Tukey-adjusted) <br> biomass ~ Entry*Location + (1|Location)") %>%
  kable_classic(full_width = F, html_font = "Helvetica")

# Alfalfa_FallBiomass.g.
model_FlBms <- lmer(`Alfalfa_FallBiomass.g.` ~ Entry + (1|Block), data=data_inter)
anova(model_FlBms)
emm <- emmeans(model_FlBms, list(pairwise ~ Entry), adjust = "tukey")
pairs_df <- as.data.frame(emm[["pairwise differences of Entry"]]) # Extract pairwise results table
sig_pairs <- pairs_df %>%
  filter(p.value < 0.07) %>%
  arrange(p.value)
# Print nicely formatted table
kbl(sig_pairs, digits = 5, caption = "Significant pairwise comparisons of Entry (Tukey-adjusted) <br> biomass ~ Entry*Location + (1|Location)") %>%
  kable_classic(full_width = F, html_font = "Helvetica")
cld_entry <- cld(
  emm,
  adjust = "tukey",
  Letters = letters
)
plot_df <- as.data.frame(cld_entry)
plot_df <- plot_df %>%
  arrange(desc(emmean)) %>%
  mutate(
    Entry = factor(Entry, levels = Entry)
  )
ggplot(plot_df, aes(x = Entry, y = emmean)) + 
  geom_col(fill = "#661100", width = 0.75, color = "black", alpha=0.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 0.6) +
  geom_text(aes(label = .group, y = upper.CL + 5), size = 5) +
  labs(
    x = NULL,
    y = "Fall biomass (g/m)",
    title = "2025 Fall biomass of alfalfa entries",
    subtitle = "Bars are estimated marginal means ±95% CI"
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

letters_df <- as.data.frame(cld_entry) %>%
  dplyr::select(Entry, .group)
label_df <- data_inter %>%
  group_by(Entry) %>%
  summarise(y = max(`Alfalfa_FallBiomass.g.`, na.rm = TRUE)) %>%
  left_join(letters_df, by = "Entry")%>%
  mutate(.group = trimws(.group)) %>%      
  arrange(.group, desc(y))                
entry_order <- label_df$Entry
data_inter <- data_inter %>%
  mutate(Entry = factor(Entry, levels = entry_order))
label_df <- label_df %>%
  mutate(Entry = factor(Entry, levels = entry_order))
ggplot(data_inter, aes(x = Entry, y = `Alfalfa_FallBiomass.g.`)) +
  geom_boxplot(fill = "#661100", color = "black", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "black") +
  labs(x = NULL,
       y = "Fall biomass (g m⁻¹)",
       title = "2025 Fall biomass of alfalfa entries") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(face = "bold"))+
  geom_text(data = label_df, aes(x = Entry, y = y + 5, label = .group), inherit.aes = FALSE, size = 5)



data_mono <- data %>%
  group_by(Entry) %>%
  filter(any(treatment == "monoculture")) %>%
  ungroup()
model_FlBms <- lmer(`Alfalfa_FallBiomass.g.` ~ Entry*treatment + (1|Block), data=data_mono)
anova(model_FlBms)
# biomass differs between treatments
emm_trt <- emmeans(model_FlBms, ~ treatment)
pairs(emm_trt)
summary(emm_trt)
plot_df <- as.data.frame(emm_trt)
# intercrop had a higher estimated mean than monoculture plots 
ggplot(plot_df, aes(x = treatment, y = emmean, fill = treatment)) +
  geom_col(width = 0.65, color = "black") +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(plot_df$upper.CL) + 5,
    label = "**",
    size = 8
  ) +
  scale_fill_manual(
    values = c(
      "intercrop" = "#661100",
      "monoculture" = "#88888888"
    )
  ) +
  labs(
    x = NULL,
    y = "Fall Alfalfa biomass (g/m)",
    title = "Fall Alfalfa biomass response to cropping treatment"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# Weed_SummerBiomass.g.
model_SmrWd <- lmer(`Weed_SummerBiomass.g.` ~ Entry + (1|Block), data=data_inter)
anova(model_SmrWd)

# Weed_FallBiomass.g.
model_FllWd <- lmer(`Weed_FallBiomass.g.` ~ Entry + (1|Block), data=data_inter)
anova(model_SmrWd)

# IWG_FallBiomass.g.
data_iwg <- data %>%
  group_by(Entry) %>%
  filter((!is.na(`IWG_FallBiomass.g.`))) %>%
  ungroup()
model_FIWGBms <- lm(`IWG_FallBiomass.g.` ~ Entry, data=data_iwg)
anova(model_FIWGBms)
summary(model_FIWGBms)

#Grain_SummerThreshed.g.
data_iwg <- data %>%
  group_by(Entry) %>%
  filter((!is.na(`Grain_SummerThreshed.g.`))) %>%
  ungroup()
model_FG <- lm(`Grain_SummerThreshed.g.` ~ Entry, data=data_iwg)
anova(model_FG)
summary(model_FG)




## Likelihood ratio test

x_bar1 <- data %>%
  filter(Entry == "AVC3") %>%
  summarise(mean_value = mean(Alfalfa_FallBiomass.g., na.rm = TRUE)) %>%
  pull(mean_value)
x_bar2 <- data %>%
  filter(Entry == "AVC9") %>%
  summarise(mean_value = mean(Alfalfa_FallBiomass.g., na.rm = TRUE)) %>%
  pull(mean_value)
sd <- data %>%
  filter(Entry %in% c("AVC3", "AVC9")) %>%
  summarise(sd_value = sd(Alfalfa_FallBiomass.g., na.rm = TRUE)) %>%
  pull(sd_value)
n1 <- data %>%
  filter(Entry == "AVC3", !is.na(Alfalfa_FallBiomass.g.)) %>%
  summarise(n = n()) %>%
  pull(n)
n2 <- data %>%
  filter(Entry == "AVC9", !is.na(Alfalfa_FallBiomass.g.)) %>%
  summarise(n = n()) %>%
  pull(n)
df <- n1 + n2 - 2
t = (x_bar1 - x_bar2)/ sqrt(sd^2 * (1/n1 + 1/n2))
# critical t-value for one-tailed t-test with a=0.05 and df=21 : 1.721
# critical t-value for two-tailed t-test with a=0.05 and df=21 : 2.080

# t-test function 
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("AVC1", "AVC6")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("AVC2", "AVC7")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("AVC3", "AVC9")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("AVC4", "AVC11")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("AVC5", "AVC14")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("IWAF-L-Base", "IWAF-L-WI1")),
       var.equal = TRUE)
t.test(Alfalfa_FallBiomass.g. ~ Entry,
       data = filter(data, Entry %in% c("IWAF-H-Base", "IWAF-H-WI1")),
       var.equal = TRUE)


# Rank changes
# average entry value 
entry_means <- data_inter %>%
  group_by(Entry) %>%
  summarise(mean_summer_biomass = mean(Alfalfa_Summer_Biomass.g., na.rm = TRUE),
  mean_fall_biomass = mean(Alfalfa_FallBiomass.g., na.rm = TRUE)) %>%
  ungroup()
# rank entries 
ranked <- entry_means %>%
  mutate(rank_summer = rank(-mean_summer_biomass, ties.method = "average"),
         rank_fall = rank(-mean_fall_biomass, ties.method = "average")) %>%
  ungroup()

# Overall stability (mean and sd)
rank_summary <- ranked %>%
  mutate(mean_rank = rowMeans(across(c(rank_summer,rank_fall)), na.rm = TRUE),
         sd_rank = apply(across(c(rank_summer,rank_fall)), 1, sd, na.rm = TRUE)) %>%
  arrange(mean_rank)
rank_summary

ggplot(rank_summary, aes(x = mean_rank, y = sd_rank, label = Entry)) +
  geom_point() +
  geom_text_repel() +
  labs(x = "Mean Rank (Overall Performance)",
       y = "SD of Rank (Stability)",
       title = "Genotype Stability Across Locations")

cor(ranked$rank_summer, ranked$rank_fall, method = "spearman", use = "pairwise.complete.obs")
Kendall(ranked$rank_summer, ranked$rank_fall)

ranked_long <- ranked %>%
  pivot_longer(cols = c(rank_summer, rank_fall), names_to = "Trait", values_to = "rank")%>%
  mutate(Trait = factor(Trait, levels = c("rank_summer", "rank_fall"),
                        labels = c("Summer", "Fall")))

ranked_long[42, "rank"] <- 9

ggplot(ranked_long, aes(x = Trait, y = rank, group = Entry, color = Entry)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +  # rank 1 at top
  theme_minimal() +
  labs(title = " ",
       y = "Rank (1 = highest biomass)")

ggplot(ranked_long, aes(x = Trait, y = rank, group = Entry, color = Entry)) +
geom_line() +
geom_point() +
geom_text(
data = filter(ranked_long, Trait == "Fall"),
aes(label = Entry),
hjust = -0.1
) +
scale_y_reverse() +
coord_cartesian(clip = "off") +
theme_minimal() +
theme(
legend.position = "none",
plot.margin = margin(5.5, 60, 5.5, 5.5)
) +
labs(
title = "",
x = "",
y = "Rank (1 = highest biomass)"
)

