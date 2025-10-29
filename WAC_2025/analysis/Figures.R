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


NY_Biomass_SeletCat <- subset(data, LOC == "NY")

custom_colors <- c(
  "base_IWG" = "darkgreen",   
  "base_Maize" = "darkgreen",  
  "other" = "purple",
  "WI_IWG" = "lightgreen",
  "WI_Maize" = "lightgreen",
  "MN_IWG" = "lightgreen",
  "MN_Maize" = "lightgreen",
  "KS_IWG" = "lightgreen"
)

NY_Biomass_SeletCat <- NY_Biomass_SeletCat %>%
  mutate(sel_base = if_else(
    Entry %in% c("IWAF-L-Base", "IWAF-H-Base"),
    "base_IWG",
    selected
  ))%>%
  mutate(sel_base2 = if_else(
    sel_base %in% c("base"), "base_Maize", sel_base
  ))%>%
  mutate(selection_cat = if_else(
    is.na(selected_type) | selected_type == "",
    sel_base2,
    selected_type
  ))

ggplot(NY_Biomass_SeletCat, aes(x = selection_cat, y = as.numeric(First_Summer_ALF_Biomass_sqrt), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(
    x = "Selection Category",
    y = "Biomass (kg/ha)",
    title = " "
  ) +
  theme_bw(base_size = 14) +
  theme(
    #    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )






# box plot with pannels for each location, sig indicators for within location differences
# KS has no data

library(dplyr)
library(purrr)
library(ordinal)
library(emmeans)
library(multcomp)
library(ggplot2)

data2 <- data %>% filter(!is.na(Alfalfa_Regrowth))

cld_list <- data2 %>%
  group_by(LOC) %>%
  group_split() %>%
  map(~{
    d <- droplevels(.x)
    loc_name <- unique(d$LOC)
    
    # Skip LOC if not enough variation
    if(n_distinct(d$Alfalfa_Regrowth) < 2){
      message("Skipping letters for ", loc_name, ": not enough variation")
      return(NULL)
    }
    
    # Fit CLMM
    fit <- tryCatch(
      clmm(Alfalfa_Regrowth ~ Entry + (1|Rep), data = d),
      error = function(e){
        message("CLMM failed for ", loc_name)
        return(NULL)
      }
    )
    
    if(is.null(fit)) return(NULL)
    
    # Compute emmeans & CLD letters, skip if Hessian fails
    emm <- tryCatch(
      emmeans(fit, pairwise ~ Entry, adjust = "tukey"),
      error = function(e){
        message("Skipping letters for ", loc_name, ": Hessian not positive definite")
        return(NULL)
      }
    )
    
    if(is.null(emm)) return(NULL)
    
    cld_loc <- multcomp::cld(emm$emmeans, Letters = letters, adjust = "sidak") %>%
      as.data.frame() %>%
      mutate(LOC = loc_name,
             .group = gsub(" ", "", .group))
    
    # Compute y positions
    y_pos <- d %>%
      group_by(LOC) %>%
      group_by(Entry) %>%
      summarise(y_pos = max(as.numeric(Alfalfa_Regrowth), na.rm = TRUE) + 3,
                .groups = "drop")
    
    left_join(cld_loc, y_pos, by = "Entry") %>%
      left_join(d %>% distinct(Entry, selection_cat), by = "Entry")
  }) %>%
  discard(is.null)

cld_plot_df <- bind_rows(cld_list)  # may be empty if no LOCs have letters

# Plot faceted by LOC
p <- ggplot(data2, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(x = "Entry", y = "Regrowth Vigor (0–5)",
       title = " ") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ LOC)

# Add letters only if available
if(nrow(cld_plot_df) > 0){
  p <- p + geom_text(data = cld_plot_df,
                     aes(x = Entry, y = 6.5, label = .group),
                     angle = 60, size = 5, fontface = "bold")
}

p

# change order of entries

loc_names <- c("New York", "Wisconsin")

ny_order <- cld_plot_df %>%
  filter(LOC == "NY") %>%
  arrange(.group) %>%          # order by significance group
  pull(Entry) %>%
  unique()

data2 <- data2 %>%
  mutate(Entry = factor(Entry, levels = ny_order))

cld_plot_df <- cld_plot_df %>%
  mutate(Entry = factor(Entry, levels = ny_order))

p <- ggplot(data2, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(x = "Entry", y = "Regrowth Vigor (0–5)",
       title = "Regrowth Vigor by Entry – Same Order Across Locations") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ LOC,
             labeller = labeller(
               LOC = c(
                 "NY" = "New York",
                 "WI" = "Wisconsin")
               ))

if(nrow(cld_plot_df) > 0){
  p <- p + geom_text(data = cld_plot_df,
                     aes(x = Entry, y = 6.5, label = .group),
                     angle = 60, size = 5, fontface = "bold")
}

p





# Anther box plot but simpler model so that WI has sig letters too

library(ordinal)
library(emmeans)
library(multcomp)
library(ggplot2)
library(dplyr)

data2 <- data %>% filter(!is.na(Alfalfa_Regrowth))

# Keep NY-based order
ny_order <- cld_plot_df %>%
  filter(LOC == "NY") %>%
  arrange(.group) %>%
  pull(Entry) %>%
  unique()

data2 <- data2 %>%
  mutate(Entry = factor(Entry, levels = ny_order))

# Compute letters per LOC using simpler CLM
cld_list_simple <- data2 %>%
  group_by(LOC) %>%
  group_split() %>%
  map(~{
    d <- droplevels(.x)
    loc_name <- unique(d$LOC)
    
    # Skip LOC if not enough variation
    if(n_distinct(d$Alfalfa_Regrowth) < 2){
      message("Skipping letters for ", loc_name, ": not enough variation")
      return(NULL)
    }
    
    # Fit CLMM
    fit <- tryCatch(
      clmm(Alfalfa_Regrowth ~ Entry + (1|Rep), data = d),
      error = function(e){
        message("CLMM failed for ", loc_name)
        return(NULL)
      }
    )
    
    if(is.null(fit)) {
      fit <- tryCatch(
        clmm(Alfalfa_Regrowth ~ Entry, data = d),
        error = function(e){
          message("CLMM failed for ", loc_name)
          return(NULL)
        }
    }
    # Compute emmeans & CLD letters, skip if Hessian fails
    emm <- tryCatch(
      emmeans(fit, pairwise ~ Entry, adjust = "tukey"),
      error = function(e){
        message("Skipping letters for ", loc_name, ": Hessian not positive definite")
        return(NULL)
      }
    )
    
    if(is.null(emm)) return(NULL)
    
    cld_loc <- multcomp::cld(emm$emmeans, Letters = letters, adjust = "sidak") %>%
      as.data.frame() %>%
      mutate(LOC = loc_name,
             .group = gsub(" ", "", .group))
    
    # Compute y positions
    y_pos <- d %>%
      group_by(LOC) %>%
      group_by(Entry) %>%
      summarise(y_pos = max(as.numeric(Alfalfa_Regrowth), na.rm = TRUE) + 3,
                .groups = "drop")
    
    left_join(cld_loc, y_pos, by = "Entry") %>%
      left_join(d %>% distinct(Entry, selection_cat), by = "Entry")
  }) %>%
  discard(is.null)

cld_plot_df <- bind_rows(cld_list_simple)  # may be empty if no LOCs have letters


cld_plot_df_simple <- bind_rows(cld_list_simple)

# Plot faceted by LOC with NY entry order and letters
p_simple <- ggplot(data2, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(x = "Entry", y = "Regrowth Vigor (0–5)",
       title = "Regrowth Vigor by Entry – Simpler Model") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ LOC)

# Add letters if available
if(nrow(cld_plot_df_simple) > 0){
  p_simple <- p_simple + 
    geom_text(data = cld_plot_df_simple,
              aes(x = Entry, y = 6.5, label = .group),
              angle = 45, size = 5, fontface = "bold")
}

p_simple



library(ordinal)
library(emmeans)
library(multcomp)
library(dplyr)
library(ggplot2)
library(purrr)

data2 <- data %>% filter(!is.na(Alfalfa_Regrowth))

# Compute letters per LOC using simpler CLM
cld_list_simple <- data2 %>%
  group_by(LOC) %>%
  group_split() %>%
  map(~{
    d <- droplevels(.x)
    loc_name <- unique(d$LOC)
    
    # Skip LOC if not enough variation
    if(n_distinct(d$Alfalfa_Regrowth) < 2){
      message("Skipping letters for ", loc_name, ": not enough variation")
      return(NULL)
    }
    
    # Try full CLMM, fallback to simpler CLM if fails
    fit <- tryCatch(
      clmm(Alfalfa_Regrowth ~ Entry + (1|Rep), data = d),
      error = function(e){
        message("CLMM failed for ", loc_name, ", switching to simpler CLM")
        tryCatch(clm(Alfalfa_Regrowth ~ Entry, data = d),
                 error = function(e2){ message("CLM also failed for ", loc_name); return(NULL) })
      }
    )
    
    if(is.null(fit)) return(NULL)
    
    # Compute emmeans & CLD letters, skip if Hessian fails
    emm <- tryCatch(
      emmeans(fit, pairwise ~ Entry, adjust = "tukey"),
      error = function(e){
        message("Skipping letters for ", loc_name, ": Hessian not positive definite")
        return(NULL)
      }
    )
    
    if(is.null(emm)) return(NULL)
    
    cld_loc <- multcomp::cld(emm$emmeans, Letters = letters, adjust = "sidak") %>%
      as.data.frame() %>%
      mutate(LOC = loc_name,
             .group = gsub(" ", "", .group))
    
    # Compute y positions per Entry
    y_pos <- d %>%
      group_by(Entry) %>%
      summarise(y_pos = max(as.numeric(Alfalfa_Regrowth), na.rm = TRUE) + 0.3,
                .groups = "drop")
    
    left_join(cld_loc, y_pos, by = "Entry") %>%
      left_join(d %>% distinct(Entry, selection_cat), by = "Entry")
  }) %>%
  discard(is.null)

cld_plot_df_simple <- bind_rows(cld_list_simple)

# Plot faceted by LOC with letters
p_simple <- ggplot(data2, aes(x = Entry, y = as.numeric(Alfalfa_Regrowth), fill = selection_cat)) +
  geom_boxplot(color = "black", width = 0.6) +
  scale_fill_manual(name = "Selection Category",
                    values = custom_colors,
                    labels = c("Base", "Other", "Selected")) +
  labs(x = "Entry", y = "Regrowth Vigor (0–5)",
       title = "Regrowth Vigor by Entry – Simpler Model") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ LOC)

# Add letters if available
if(nrow(cld_plot_df_simple) > 0){
  p_simple <- p_simple + 
    geom_text(data = cld_plot_df_simple,
              aes(x = Entry, y = 6.5, label = .group),
              angle = 45, size = 5, fontface = "bold")
}

p_simple
