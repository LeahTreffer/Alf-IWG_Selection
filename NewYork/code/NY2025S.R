# just want to play around with data and see if the new planting and new entries have anything exciting
# looking for differences by entry

#setwd("/Users/leahtreffer/Library/CloudStorage/GoogleDrive-lkt38@cornell.edu/.shortcut-targets-by-id/1HB830FpEuQ7JMUVP_w2Age0OjghkSCuN/Alfalfa-IWG AFRP (2021-2024)/Breeding Study")

library(ggplot2)
library(multcompView)
library(lmerTest)
library(lme4)
library("PerformanceAnalytics")
library(dplyr)
library(tidyverse)
library(psych)
library(readxl) 
library(emmeans)
library(multcompView)
library(multcomp)

#data <- read.csv("Data/partial data files/Fieldbook/2024-09-26-03-48-01_2024_NYMG2_Alf-IWG_table.csv")
data <- read.csv("NewYork/data/leah summer alf-IWG_12_08_2025_15_04_59.csv")

extras <- read.csv("NewYork/data/2024-2025 NY MG2.csv")

data2 <- left_join(data, extras[,c("ENT_COMB", "ENT", "Entry", "Serpentine")], by= 'Serpentine') # add in seed info to the data table


data2$ENT <- as.factor(data2$ENT)
data2$Entry <- as.factor(data2$Entry)
data2$Block <- as.factor(data2$Block)
data2$Serpentine <- as.character(data2$Serpentine)
data2$treatment <- as.factor(data2$treatment)

intercrop_data <- subset(data2, treatment == 'intercrop')
monoculture_data <- subset(data2, treatment == 'monoculture')
mono_entries <- unique(monoculture_data$Entry)
mono_entry_data <- subset(data2, Entry %in% mono_entries) # entries that are in inter and mono


#########################
# anova of alfalfa hieght
#mix.lmer <- lmer(Alfalfa_Height ~ Entry + (1 | Block), data=intercrop_data)
#summary(mix.lmer)
#anova(mix.lmer)

#emm_treat <- emmeans(mix.lmer, ~ treatment)
#pairs(emm_treat, adjust = "tukey")  # Tukey HSD
#lm_simple <- lm(Alfalfa_Height ~ Entry, data = intercrop_data)
#anova(lm_simple)

#emm_entry <- emmeans(lm_simple, ~ Entry)
emm_entry <- emmeans(mix.lmer, ~ Entry)
pairs(emm_entry, adjust = "tukey")
cld_entry <- cld(emm_entry, adjust = "tukey", Letters = letters)

cld_df <- as.data.frame(cld_entry)
cld_df$Entry2 <- reorder(cld_df$Entry, cld_df$emmean, FUN = median)
levels(cld_df$Entry2) <- levels(data$Entry2)

ggplot(intercrop_data, aes(x = Entry, y = Alfalfa_Height)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Alfalfa Height",
       x = "Entry",
       y = "Height") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = cld_df,
            aes(x = Entry2, y = emmean + 5, label = .group),  # adjust y position if needed
            inherit.aes = FALSE,
            size = 4,
            vjust = 0)
# save boxplot_S_height_intercrop_only


#########################
# anova of alfalfa maturity
mix.lmer <- lmer(Alfalfa.maturity ~ Entry + (1 | Block), data=intercrop_data)
#summary(mix.lmer)
#anova(mix.lmer)

emm_entry <- emmeans(mix.lmer, ~ Entry)
pairs(emm_entry, adjust = "tukey")
cld_entry <- cld(emm_entry, adjust = "tukey", Letters = letters)

cld_df <- as.data.frame(cld_entry)
cld_df$Entry2 <- reorder(cld_df$Entry, cld_df$emmean, FUN = median)
levels(cld_df$Entry2) <- levels(intercrop_data$Entry)

ggplot(intercrop_data, aes(x = Entry, y = Alfalfa.maturity)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Alfalfa Maturity",
       x = "Entry",
       y = "Height") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = cld_df,
            aes(x = Entry2, y = emmean + 5, label = .group),  # adjust y position if needed
            inherit.aes = FALSE,
            size = 4,
            vjust = 0)
# save boxplot_S_height_intercrop_only



################
mix.lmer <- lmer(lodging ~ Entry + treatment + (1 | Block), data=data2)
summary(mix.lmer)
anova(mix.lmer)
# lodging is not significant by Entry, although certain entries are significantly different (AVC1, Larry15, and marginally so: AVC3, INTALF20, IWAF-L-WI1, NY19-45)
# treatment is sifnificant (monocultures are less lodged than intercrop)

