# Load packages
library(lme4)
library(lmtest)
library(dplyr)
library(kableExtra)

model_Ha <- lmer(First_Summer_ALF_Biomass_sqrt ~ selected*LOC + (1 | Entry) + (1|LOC:Rep), data=data, REML=FALSE) # alternate hypothesis : intercropping selection influences biomass

model_H0 <- lmer(First_Summer_ALF_Biomass_sqrt ~ 1 + (1 | Entry) + (1|LOC:Rep), data=data, REML=FALSE) # null hypothesis : selection doesn't matter to biomass

anova(model_H0, model_Ha)
# Pr(>Chisq) = 0.08768
# Alternative model with selection does not significantly improve model fit
# Being selected for intercropping does not significantly affect First_Summer_ALF_Biomass (after accounting for Entry, Location, and Rep)

LRT <- lrtest(model_H0, model_Ha)
LRT

emmeans(model_Ha, pairwise ~ selected)
plot(emmeans(model_Ha, ~ selected))

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

emm <- emmeans(model_Ha, ~ selected)

emm_table <- as.data.frame(emm)

kbl(emm_table, digits = 3, caption = "Table 2. Estimated marginal means of first-summer alfalfa biomass by selection category.") %>%
  kable_classic(full_width = F, html_font = "Helvetica")


library(ggplot2)

emm_df <- as.data.frame(emm)

ggplot(emm_df, aes(x = selected, y = emmean, fill = selected)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, linewidth = 0.6) +
  labs(
    title = "Estimated Marginal Means of Year 1 Summer Alfalfa Biomass",
    x = "Selection Category",
    y = "Biomass sqrt(kg/ha)"
  ) +
  scale_fill_manual(values = c("darkgreen", "purple", "lightgreen")) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 22, color = "black"),  # larger tick labels
    axis.text.y = element_text(size = 18, color = "black"),  # optional
    axis.title.x = element_text(size = 18),  # keep x-axis label normal
    axis.title.y = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# "darkgreen", "purple", "lightgreen" 

# "#a6cee3", "#1f78b4", "orange"



# Box plot with sig letters for ordinal categorical linear model of Regrowth Vigor 

# model3 <- clmm(Alfalfa_Regrowth ~ Entry + LOC + (1|LOC:Rep), data = data)

emm <- emmeans(model3, list(pairwise ~ Entry), adjust = "tukey")

cld_df <- multcomp::cld(emm,
                        adjust = "sidak",   # sidak auto-applied for emmeans
                        Letters = letters,
                        alpha = 0.05) %>%
  as.data.frame() %>%
  mutate(.group = gsub(" ", "", .group))  # clean extra spaces

# Compute y position for letters above boxes
# (use max of Alfalfa_Regrowth per Entry)
y_positions <- data %>%
  group_by(Entry) %>%
  summarise(
    y_pos = if (all(is.na(Alfalfa_Regrowth))) NA_real_
    else max(as.numeric(Alfalfa_Regrowth), na.rm = TRUE) + 0.3,
    .groups = "drop"
  )


data <- data %>%
  mutate(selection_cat = if_else(
    is.na(selected_type) | selected_type == "",
    selected,
    selected_type
  ))

# Merge y positions with letters
cld_plot_df <- left_join(
  cld_df,
  data %>% distinct(Entry, selection_cat),
  by = "Entry"
)

cld_plot_df <- left_join(
  cld_plot_df,
  y_positions %>% distinct(Entry, y_pos),
  by = "Entry"
)


# Boxplot with significance letters
ggplot(data, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  geom_text(
    data = cld_plot_df,
    aes(x = Entry, y = y_pos, label = .group),
    angle = 45,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    x = "Selection Category",
    y = "Regrowth Vigor (0–5)",
    title = "Regrowth Vigor by Entry across Locations"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )







library(ggplot2)
library(dplyr)

# 1. Order entries by significance letter
cld_plot_df <- cld_plot_df %>%
  arrange(.group) %>%
  mutate(Entry = factor(Entry, levels = unique(Entry)))

data <- data %>%
  mutate(Entry = factor(Entry, levels = levels(cld_plot_df$Entry)))  # match factor order

# 2. Define custom colors for selection_cat
custom_colors <- c(
  "base" = "darkgreen",   # replace Type1/Type2/Type3 with actual values in your data
  "other" = "purple",
  "WI_IWG" = "lightgreen",
  "WI_Maize" = "lightgreen",
  "MN_IWG" = "lightgreen",
  "MN_Maize" = "lightgreen",
  "KS_IWG" = "lightgreen"
)

# 3. Plot
ggplot(data, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  geom_text(
    data = cld_plot_df,
    aes(x = Entry, y = y_pos, label = .group),
    angle = 45,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(
    x = "Selection Category",
    y = "Regrowth Vigor (0–5)",
    title = "Regrowth Vigor by Entry across Locations"
  ) +
  theme_bw(base_size = 14) +
  theme(
#    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

