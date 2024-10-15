# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the dataset
data <- data.frame(
  Treatment = c("111", "112", "121", "122", "211", "212", "221", "222", "311", "312", "321", "322"),
  IPGTT0 = c(149.80, 167.00, 182.60, 157.40, 155.40, 147.60, 141.00, 164.40, 148.20, 151.60, 151.40, 136.40),
  IPGTT15 = c(179.20, 175.00, 203.80, 198.20, 155.80, 152.80, 157.80, 126.60, 164.00, 182.20, 164.40, 163.60),
  IPGTT30 = c(162.40, 152.40, 192.60, 204.20, 157.80, 139.00, 143.00, 169.00, 175.80, 161.80, 152.40, 172.20),
  IPGTT75 = c(179.40, 132.67, 170.20, 184.80, 146.80, 156.60, 135.80, 156.75, 146.40, 160.80, 157.75, 135.40)
)

# Reshape the data
data_long <- data %>%
  pivot_longer(cols = starts_with("IPGTT"), names_to = "Time", values_to = "Glucose")

# Extract breed and diet information
data_long <- data_long %>%
  mutate(
    Breed = case_when(
      substr(Treatment, 1, 1) == "1" ~ "Funaab Alpha",
      substr(Treatment, 1, 1) == "2" ~ "Ross",
      substr(Treatment, 1, 1) == "3" ~ "Marshall"
    ),
    Diet = case_when(
      substr(Treatment, 2, 3) == "11" ~ "Control",
      substr(Treatment, 2, 3) == "12" ~ "2.5% Vitex doniana",
      substr(Treatment, 2, 3) == "21" ~ "2.5% Plantain peel",
      substr(Treatment, 2, 3) == "22" ~ "2.5% Plantain+Vitex"
    ),
    Time = as.numeric(gsub("IPGTT", "", Time))
  )

# Create the plot
ggplot(data_long, aes(x = Time, y = Glucose, color = Diet, linetype = Diet)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Breed, ncol = 3) +
  labs(
    title = "Glucose Tolerance in Different Chicken Breeds",
    x = "Time (minutes)",
    y = "Blood glucose (mg/dL)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "brown"),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = c(0, 15, 30, 75)) +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"))
