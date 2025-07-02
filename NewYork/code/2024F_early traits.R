# just want to play around with data and see if the new planting and new entries have anything exciting
# looking for differences by entry

#setwd("/Users/leahtreffer/Library/CloudStorage/GoogleDrive-lkt38@cornell.edu/.shortcut-targets-by-id/1HB830FpEuQ7JMUVP_w2Age0OjghkSCuN/Alfalfa-IWG AFRP (2021-2024)/Breeding Study")

library(ggplot2)
library(multcompView)
library(lme4)
library("PerformanceAnalytics")
library(dplyr)
library(psych)
library(readxl) 

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
chart.Correlation(data[,c("Alfalfa.Stand.Count", "IWG.Stand.Count")], histogram=TRUE, pch=19)

# histogram of alfalfa stand count in intercrop and alfalfa stand count in monoculture 
# visualize if data is normal
# visualize if x axis for the two are significantly different (there are much fewer mono plots so frequency (y axis) will be lower, but if they don't overlap along the x-axis this indicates stand count in mono is different than stand count of intercroped)
ggplot(data, aes(x = Alfalfa.Stand.Count, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize the colors if needed
  labs(title = "Alfalfa Stand Count Distribution by Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Stand Count",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = IWG.Height, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.25) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize the colors if needed
  labs(title = "Neighbor Height Distribution by Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Mean Height (cm)",
       y = "Frequency") +
  theme_minimal()

# box plot of each entry
# visualize the value distribution for each entry, and if entry was in mono and intercrop, has that split as well
p <- ggplot(data, aes(x = factor(Entry), y = Alfalfa.Stand.Count, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Stand Count  Distribution by Entry and Treatment\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#reordered 
data$Entry2 <- reorder(data$Entry, data$Alfalfa.Stand.Count, FUN = median, na.rm = TRUE)
# Plot
q <- ggplot(data, aes(x = Entry2, y = Alfalfa.Stand.Count, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Stand Count Distribution\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

q

# anova of alfalfa stand count
model_alfstdc <- aov(Alfalfa.Stand.Count ~ Entry + Block + treatment, data)
summary(model_alfstdc)

fit = lm(Alfalfa.Stand.Count ~ Entry + Block + treatment + Chalcids, data = data)
fit

mix.lmer <- lmer(Alfalfa.Stand.Count ~ treatment + Chalcids + Block + (1 | Entry) + (1 | Block), data=data)
summary(mix.lmer)


# Height 
data$Entry3 <- reorder(data$Entry, data$Alfalfa.Height, FUN = median, na.rm = TRUE)
# Plot
r <- ggplot(data, aes(x = Entry3, y = Alfalfa.Height, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("blue", "red")) +  # Customize colors
  labs(title = "Alfalfa Height Distribution\n(number of individual plants per 0.5m length of plot)",
       x = "Entry",
       y = "Stand Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

r
