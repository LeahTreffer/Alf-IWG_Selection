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
data <- read.csv("NewYork/data/2024-2025 NY MG2.csv")

# Add in extra data
rates <- read_excel("NewYork/data/AlfalfaIWG_breeding_PlotDesign_NY_2024-3.xlsx", 
                                                       sheet = "Alfalfa rates")
rates <- as.data.frame(rates[,c("Entry", "Seed/Plot g", "coated", "Chalcids")]) # get just the columns we want

rates <- rates %>%
  mutate(Chalcids = ifelse(Chalcids == "!", TRUE, FALSE)) %>% # change ! to 1 (seeds that did have visible Chalcid flies)
  mutate(Chalcids = ifelse(is.na(Chalcids), FALSE, Chalcids)) # change NA to 0

data <- left_join(data, rates) # add in seed info to the data table

data$ENT <- as.factor(data$ENT)
data$Entry <- as.factor(data$Entry)
data$Block <- as.factor(data$Block)
data$Serpentine <- as.character(data$Serpentine)
data$treatment <- as.factor(data$treatment)
data$`Seed/Plot g` <- as.numeric(data$`Seed/Plot g`)
data$coated <- as.factor(data$coated)
data$Chalcids <- as.factor(data$Chalcids)

data$Entry <- gsub("-", "_", data$Entry)
data$Entry <- gsub(" #", "", data$Entry)

#data$Alfalfa.Height <- rowMeans(data[,c("Alfalfa.Height.1", "Alfalfa.Height.2", "Alfalfa.Height.3", "Alfalfa.Height.4")], na.rm = TRUE) # 'na.rm = TRUE' to handle NA values
#data$IWG.Height <- rowMeans(data[,c("IWG.Height.1", "IWG.Height.2", "IWG.Height.3", "IWG.Height.4")], na.rm = TRUE) # 'na.rm = TRUE' to handle NA values

intercrop_data <- subset(data, treatment == 'intercrop')
monoculture_data <- subset(data, treatment == 'monoculture')

mono_entries <- unique(monoculture_data$Entry)
mono_entry_data <- subset(data, Entry %in% mono_entries)

hist(data$Fall.Alfalfa.Stand.Count, breaks = 20)
hist(data$Fall.Alfalfa.Height, breaks = 20)

hist(intercrop_data$Fall.IWG.Stand.Count, breaks = 20) # so this is IWG data
hist(intercrop_data$Fall.IWG.Height, breaks = 20) # so this is IWG data

hist(monoculture_data$Fall.IWG.Height, breaks = 20, main = "monoculture companion height") # and this is alfalfa data (the alfalfa companion)
hist(monoculture_data$Fall.IWG.Stand.Count, breaks = 20, main = "monoculture companion stand count") # and this is alfalfa data (the alfalfa companion)

hist(data$Alfalfa.winter.survival, breaks = 20)
hist(log(data$Alfalfa.winter.survival), breaks = 20)

hist(data$Spring.Alfalfa.stand.count)

hist(data$Spring.Alfalfa.height, breaks = 20)

hist(intercrop_data$Spring.IWG.height, breaks = 20)# so this is IWG data
hist(monoculture_data$Spring.IWG.height, breaks = 20, main = "monoculture companion height")# and this is alfalfa data (the alfalfa companion)

hist(data$Spring.Alfalfa.vigor, breaks = 10)

hist(data$Spring.Alfalfa.maturity, breaks = 5)

hist(data$Spring.disease.incidence)
hist(data$Spring.disease.severity)

# correlation between alfalfa stand count and IWG stand count
#chart.Correlation(data[,c("Alfalfa.Stand.Count", "IWG.Stand.Count")], histogram=TRUE, pch=19)

# histogram of alfalfa stand count in intercrop and alfalfa stand count in monoculture 
# visualize if data is normal
# visualize if x axis for the two are significantly different (there are much fewer mono plots so frequency (y axis) will be lower, but if they don't overlap along the x-axis this indicates stand count in mono is different than stand count of intercroped)
ggplot(data, aes(x = Fall.Alfalfa.Stand.Count, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize the colors if needed
  labs(title = "Alfalfa Stand Count Distribution by Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Stand Count",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Fall.IWG.Height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.25) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize the colors if needed
  labs(title = "Neighbor Height Distribution by Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Mean Height (cm)",
       y = "Frequency") +
  theme_minimal()

################################################
############ box plot of each entry

# stand count
# visualize the value distribution for each entry, and if entry was in mono and intercrop, has that split as well
ggplot(data, aes(x = factor(Entry), y = Fall.Alfalfa.Stand.Count, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Stand Count  Distribution by Entry and Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#reordered 
data$Entry2 <- reorder(data$Entry, data$Fall.Alfalfa.Stand.Count, FUN = median, na.rm = TRUE)
# Plot
ggplot(data, aes(x = Entry2, y = Fall.Alfalfa.Stand.Count, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Stand Count Distribution\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data, aes(x = Entry2, y = Spring.Alfalfa.stand.count, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Stand Count Distribution\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################################
# anova of alfalfa stand count
#mix.lmer <- lmer(Fall.Alfalfa.Stand.Count ~ Entry2 + (1 | Block), data=intercrop_data)
#summary(mix.lmer)
#anova(mix.lmer)

#emm_treat <- emmeans(mix.lmer, ~ treatment)
#pairs(emm_treat, adjust = "tukey")  # Tukey HSD
lm_simple <- lm(Fall.Alfalfa.Stand.Count ~ Entry, data = intercrop_data)
anova(lm_simple)

emm_entry <- emmeans(lm_simple, ~ Entry)
pairs(emm_entry, adjust = "tukey")
cld_entry <- cld(emm_entry, adjust = "tukey", Letters = letters)

cld_df <- as.data.frame(cld_entry)
cld_df$Entry2 <- reorder(cld_df$Entry, cld_df$emmean, FUN = median)
levels(cld_df$Entry2) <- levels(data$Entry2)

ggplot(intercrop_data, aes(x = Entry2, y = Fall.Alfalfa.Stand.Count)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Alfalfa Stand Count Distribution\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = cld_df,
            aes(x = Entry2, y = emmean + 20, label = .group),  # adjust y position if needed
            inherit.aes = FALSE,
            size = 4,
            vjust = 0)

################################################
############ box plot of each entry

# Height
data$Entry3 <- reorder(data$Entry, data$Fall.Alfalfa.Height, FUN = median, na.rm = TRUE)
# Plot
ggplot(data, aes(x = Entry3, y = Fall.Alfalfa.Height, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Fall Alfalfa Height Distribution",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# save boxplot_F_alfalfa_height

data$Entry4 <- reorder(data$Entry, data$Spring.Alfalfa.height, FUN = median, na.rm = TRUE)
# Plot
ggplot(data, aes(x = Entry4, y = Spring.Alfalfa.height, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Spring Alfalfa Height Distribution",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# save boxplot_S_alfalfa_height

####################################
# anova of alfalfa hieght
#mix.lmer <- lmer(Fall.Alfalfa.Stand.Count ~ Entry2 + (1 | Block), data=intercrop_data)
#summary(mix.lmer)
#anova(mix.lmer)

#emm_treat <- emmeans(mix.lmer, ~ treatment)
#pairs(emm_treat, adjust = "tukey")  # Tukey HSD
lm_simple <- lm(Fall.Alfalfa.Height ~ Entry, data = intercrop_data)
anova(lm_simple)

emm_entry <- emmeans(lm_simple, ~ Entry)
pairs(emm_entry, adjust = "tukey")
cld_entry <- cld(emm_entry, adjust = "tukey", Letters = letters)

cld_df <- as.data.frame(cld_entry)
cld_df$Entry2 <- reorder(cld_df$Entry, cld_df$emmean, FUN = median)
levels(cld_df$Entry2) <- levels(data$Entry2)

ggplot(intercrop_data, aes(x = Entry, y = Fall.Alfalfa.Height)) +
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
# save boxplot_F_height_intercrop_only

lm_simple <- lm(Spring.Alfalfa.height ~ Entry, data = intercrop_data)
anova(lm_simple)

emm_entry <- emmeans(lm_simple, ~ Entry)
pairs(emm_entry, adjust = "tukey")
cld_entry <- cld(emm_entry, adjust = "tukey", Letters = letters)

cld_df <- as.data.frame(cld_entry)
cld_df$Entry2 <- reorder(cld_df$Entry, cld_df$emmean, FUN = median)
levels(cld_df$Entry2) <- levels(data$Entry2)

ggplot(intercrop_data, aes(x = Entry, y = Spring.Alfalfa.height)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Alfalfa Height",
       x = "Entry",
       y = "Height") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = cld_df,
            aes(x = Entry2, y = emmean + 12, label = .group),  # adjust y position if needed
            inherit.aes = FALSE,
            size = 4,
            vjust = 0)
# save boxplot_S_height_intercrop_only

################################################
############ 


# kendall correlation (tau)
# ordianal association between quantities (comparison of rank)
# stand count
corr <- cor.test(intercrop_data$Fall.Alfalfa.Stand.Count, intercrop_data$Spring.Alfalfa.stand.count, method = 'kendall')
corr

# height 
corr <- cor.test(intercrop_data$Fall.Alfalfa.Height, intercrop_data$Spring.Alfalfa.height, method = 'kendall')
corr
