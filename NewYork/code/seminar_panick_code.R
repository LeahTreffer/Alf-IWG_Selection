NY$Time <- as.factor(paste0(NY$YEAR,"_",NY$Harvest))


# Alfalfa Biomass
model_Bms <- lmer(Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block), data=data_inter)
anova(model_Bms)
# both Entry and Time are significant
emm <- emmeans(model_Bms, list(pairwise ~ Entry), adjust = "tukey")
pairs_df <- as.data.frame(emm[["pairwise differences of Entry"]]) # Extract pairwise results table
View(pairs_df)
sig_pairs <- pairs_df %>%
filter(p.value < 0.05) %>%
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

library(ggplot2)
library(dplyr)
library(emmeans)
library(multcomp)
library(multcompView)

# Calculate estimated marginal means for Entry
emm_entry <- emmeans(model_Bms, ~ Entry)
emm_time <- emmeans(model_Bms, ~ Time)

# Compact letter display
cld_entry <- cld(
  emm_entry,
  adjust = "tukey",
  Letters = letters
)

# Prepare plotting data
plot_entry <- as.data.frame(cld_entry) %>%
  mutate(
    .group = trimws(.group)
  ) %>%
  arrange(desc(emmean)) %>%
  mutate(
    Entry = factor(Entry, levels = Entry)
  )

# Plot
p_entry <- ggplot(plot_entry, aes(x = Entry, y = emmean)) +
  geom_col(
    fill = "#661100",
    width = 0.75,
    color = "black",
    alpha = 0.65
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_text(
    aes(y = upper.CL, label = .group),
    vjust = -0.6,
    size = 4.5
  ) +
  labs(
    x = NULL,
    y = expression("Alfalfa biomass (kg ha"^{-1}*")"),
    title = "Alfalfa biomass among entries",
    subtitle = "Estimated marginal means averaged across sampling times",
    caption = "Different letters indicate significant differences (adjusted pairwise comparisons, P < 0.05)."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.position = "none"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  )

p_entry

# Order entries by decreasing estimated marginal mean
entry_order <- as.data.frame(emm_entry) %>%
  arrange((emmean)) %>%
  pull(Entry)

# Apply the order to plotting data
data_inter <- data_inter %>%
  mutate(Entry = factor(Entry, levels = entry_order))

label_df <- label_df %>%
  mutate(Entry = factor(Entry, levels = entry_order))

# Estimated marginal means for Entry
emm_entry <- emmeans(model_Bms, ~ Entry)

# Tukey-adjusted significance groups
cld_entry <- cld(
  emm_entry,
  adjust = "tukey",
  Letters = letters
)

# Extract significance letters
letters_df <- as.data.frame(cld_entry) %>%
  dplyr::select(Entry, .group) %>%
  mutate(.group = trimws(.group))

# Determine label positions using observed data
label_df <- data_inter %>%
  group_by(Entry) %>%
  summarise(
    y = max(Alfalfa_Biomass_kgha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(letters_df, by = "Entry") %>%
  mutate(
    y = y + 0.06 * max(y, na.rm = TRUE)
  ) %>%
  arrange((y))

# Order entries by estimated marginal mean
entry_order <- as.data.frame(emm_entry) %>%
  arrange((emmean)) %>%
  pull(Entry)

data_inter <- data_inter %>%
  mutate(Entry = factor(Entry, levels = entry_order))

label_df <- label_df %>%
  mutate(Entry = factor(Entry, levels = entry_order))

# Boxplot
p_entry_box <- ggplot(
  data_inter,
  aes(x = Entry, y = Alfalfa_Biomass_kgha)
) +
  geom_boxplot(
    fill = "#661100",
    color = "black",
    alpha = 0.6,
    width = 0.7,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.15,
    size = 1.8,
    alpha = 0.45,
    color = "black"
  ) +
  geom_text(
    data = label_df,
    aes(x = Entry, y = y, label = .group),
    inherit.aes = FALSE,
    size = 4.5
  ) +
  labs(
    x = NULL,
    y = expression("Alfalfa biomass (kg ha"^{-1}*")"),
    title = "Alfalfa biomass differences among entries",
    subtitle = "Observed biomass distribution across sampling times",
    caption = "Different letters indicate significant differences based on Tukey-adjusted comparisons of estimated marginal means."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  )

p_entry_box


library(emmeans)
library(ggplot2)
library(dplyr)

# Estimated marginal means for Time
emm_time <- emmeans(model_Bms, ~ Time)

# Convert to dataframe
plot_time <- as.data.frame(emm_time)

plot_time <- as.data.frame(emm_time) %>%
  mutate(
    Time = factor(
      Time,
      levels = c(
        "2025_Summer",
        "2025_Fall",
        "2026_Summer",
        "2026_Fall"
      )
    )
  )

# Plot
p_time <- ggplot(plot_time,
                 aes(x = Time, y = emmean, group = 1)) +
  
  geom_line(
    color = "#661100",
    linewidth = 1.3
  ) +
  
  geom_point(
    color = "#661100",
    size = 4
  ) +
  
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.12,
    linewidth = 0.8
  ) +
  
  labs(
    x = "Sampling time",
    y = expression("Alfalfa biomass (kg ha"^{-1}*")"),
    title = "Alfalfa biomass over time"
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 13)
  )

p_time







data_mono <- NY %>%
  group_by(Entry) %>%
  filter(any(treatment == "monoculture")) %>%
  ungroup()
model_MBms <- lmer(Alfalfa_Biomass_kgha ~ Entry*treatment + Time + (1|Block), data=data_mono)
anova(model_MBms)
# biomass differs between treatments
emm_trt <- emmeans(model_MBms, ~ treatment)
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




# t-test function 
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("AVC1", "AVC6")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("AVC2", "AVC7")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("AVC3", "AVC9")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("AVC4", "AVC11")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("AVC5", "AVC14")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-WI1")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-WI1")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-MN1")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-MN1")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-KS1")),
       var.equal = TRUE)
t.test(Alfalfa_Biomass_kgha ~ Entry,
       data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-KS1")),
       var.equal = TRUE)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("AVC1", "AVC6"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("AVC2", "AVC7"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("AVC3", "AVC9"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("AVC4", "AVC11"))
)
emmeans(model_compare, pairwise ~ Entry)
###

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("AVC5", "AVC14"))
)
emmeans(model_compare, pairwise ~ Entry)
###

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-WI1"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-WI1"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-MN1"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-MN1"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-L-Base", "IWAF-L-KS1"))
)
emmeans(model_compare, pairwise ~ Entry)

model_compare <- lmer(
  Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
  data = filter(data_inter, Entry %in% c("IWAF-H-Base", "IWAF-H-KS1"))
)
emmeans(model_compare, pairwise ~ Entry)



library(lme4)
library(emmeans)
library(dplyr)
library(gt)
library(purrr)

# Define entry comparisons
comparisons <- list(
  c("AVC1", "AVC6"),
  c("AVC2", "AVC7"),
  c("AVC3", "AVC9"),
  c("AVC4", "AVC11"),
  c("AVC5", "AVC14"),
  c("IWAF-L-Base", "IWAF-L-WI1"),
  c("IWAF-H-Base", "IWAF-H-WI1"),
  c("IWAF-L-Base", "IWAF-L-MN1"),
  c("IWAF-H-Base", "IWAF-H-MN1"),
  c("IWAF-L-Base", "IWAF-L-KS1")
)

# Function to fit model and extract results
get_comparison <- function(entries) {
  
  subset_data <- data_inter %>%
    filter(Entry %in% entries)
  
  model <- lmer(
    Alfalfa_Biomass_kgha ~ Entry + Time + (1|Block),
    data = subset_data
  )
  
  # Estimated marginal means
  emm <- emmeans(model, ~ Entry)
  emm_df <- as.data.frame(emm)
  
  # Pairwise comparison
  pair_df <- as.data.frame(pairs(emm))
  
  # Extract means
  mean1 <- emm_df$emmean[emm_df$Entry == entries[1]]
  mean2 <- emm_df$emmean[emm_df$Entry == entries[2]]
  
  # Extract P-value
  pval <- pair_df$p.value[1]
  
  # Determine higher biomass entry
  higher <- if (pval < 0.05) {
    if (mean1 > mean2) entries[1] else entries[2]
  } else {
    ""
  }
  
  data.frame(
    Selection = paste(entries[1], "vs", entries[2]),
    Higher_biomass = higher,
    Difference_kg_ha = abs(mean1 - mean2),
    P_value = pval,
    Significant = pval < 0.05
  )
}

# Run comparisons
results_table <- map_dfr(comparisons, get_comparison)


results_table %>%
  gt() %>%
  
  cols_hide(columns = Significant) %>%

cols_label(
  Selection = "Selection",
  Higher_biomass = "Higher biomass",
  Difference_kg_ha = "Difference (kg/ha)",
  P_value = "P-value"
) %>%
  
  fmt_number(
    columns = Difference_kg_ha,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  
  fmt_number(
    columns = P_value,
    decimals = 4
  ) %>%
  
  # Highlight significant rows in light purple
  tab_style(
    style = cell_fill(color = "#EDE3F5"),
    locations = cells_body(
      rows = Significant == TRUE
    )
  ) %>%
  
  tab_header(
    title = "Alfalfa Biomass Comparisons",
    subtitle = "Estimated marginal means from mixed-effects models"
  ) %>%
  
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 20,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.color = "#999999",
    table.border.bottom.color = "#999999"
  ) %>%
  
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  
  tab_source_note(
    source_note = paste(
      "Higher biomass is reported only for significant comparisons (P < 0.05).",
      "Difference represents the absolute difference between estimated marginal means.",
      "Purple shading indicates statistical significance."
    )
  )

