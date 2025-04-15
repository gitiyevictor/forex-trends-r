# Load libraries
library(tidyverse)
library(lubridate)

# Read data
forex_data <- read_csv("kes_usd_2020_2024.csv")

# Preview data
glimpse(forex_data)

# Convert date column
forex_data <- forex_data %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date, label = TRUE))

# Plot 1: KES/USD trend over time
ggplot(forex_data, aes(x = date, y = kes_per_usd)) +
  geom_line(color = "#0072B2") +
  labs(
    title = "KES to USD Exchange Rate (2020–2024)",
    x = "Date", y = "KES per USD"
  ) +
  theme_minimal()

# Plot 2: Monthly average exchange rate
monthly_avg <- forex_data %>%
  group_by(year, month) %>%
  summarise(avg_kes = mean(kes_per_usd), .groups = "drop")

ggplot(monthly_avg, aes(x = interaction(year, month), y = avg_kes)) +
  geom_col(fill = "#E69F00") +
  labs(
    title = "Monthly Average KES per USD (2020–2024)",
    x = "Month", y = "Average Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
