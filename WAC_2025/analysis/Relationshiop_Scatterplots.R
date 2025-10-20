data2 %>%
  ggplot(aes(x = First_Summer_ALF_Biomass_sqrt, y = First_Summer_ALF_Height, color = selected)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_y_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_color_manual(values = c("red", "orange", "lightblue", "green", "blue", "purple", "grey")) +
  theme_minimal() +
  labs(x = "Biomass (kg/ha)",
       y = "Height (cm)",
       title = "Relationship between Alfalfa Biomass and Height \nAcross Different Selection backgrounds")

data2 %>%
  ggplot(aes(x = First_Summer_ALF_Biomass_sqrt, y = First_Summer_ALF_Height, color = selection_location)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_y_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_color_manual(values = c("#4477AA", "#EE6677", "#228833", "grey")) +
  theme_minimal() +
  labs(x = "Biomass (kg/ha)",
       y = "Height (cm)",
       title = "Relationship between Alfalfa Biomass and Height \nAcross Different Selection Locations")

data2 %>%
  ggplot(aes(x = First_Summer_ALF_Biomass_sqrt, y = First_Summer_ALF_Height, color = LOC)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_y_continuous(labels = comma_format(big.mark = ",")) + # requires the package scales
  scale_color_manual(values = c("#4477AA", "#EE6677")) +
  theme_minimal() +
  labs(x = "Biomass (kg/ha)",
       y = "Height (cm)",
       title = "Relationship between Alfalfa Biomass and Height \nAcross Different Locations")
