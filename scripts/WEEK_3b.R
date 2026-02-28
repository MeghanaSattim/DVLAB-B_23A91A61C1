# ── airquality Bar Charts ────────────────────────────────────────────────────
data(airquality)
library(ggplot2)
library(tidyr)

# Convert Month to factor
airquality$Month <- factor(airquality$Month, labels = c("May","Jun","Jul","Aug","Sep"))

# ── Base R ───────────────────────────────────────────────────────────────────

# 1. Count bar chart
barplot(table(airquality$Month),
        main = "Monthly Observation Frequency in Air Quality Data",
        xlab = "Month (Summer 1973)", ylab = "Number of Days Recorded",
        col = "mediumpurple")

# 2. Aggregated bar chart (mean ozone)
mean_ozone <- tapply(airquality$Ozone, airquality$Month, mean, na.rm = TRUE)
barplot(mean_ozone,
        main = "Average Ground-Level Ozone Concentration by Month",
        xlab = "Month (New York, 1973)", ylab = "Mean Ozone (ppb)",
        col = "tomato")

# 3. Grouped bar chart
group_means <- rbind(
  Ozone = tapply(airquality$Ozone,   airquality$Month, mean, na.rm = TRUE),
  Solar = tapply(airquality$Solar.R, airquality$Month, mean, na.rm = TRUE) / 10
)
barplot(group_means, beside = TRUE, col = c("steelblue", "darkorange"),
        legend.text = TRUE,
        main = "Side-by-Side Comparison: Ozone vs Solar Radiation (Solar ÷ 10)",
        xlab = "Month", ylab = "Mean Value (Scaled)")

# 4. Stacked bar chart
barplot(group_means, beside = FALSE, col = c("steelblue", "darkorange"),
        legend.text = TRUE,
        main = "Cumulative View: Ozone & Solar Radiation Combined (Solar ÷ 10)",
        xlab = "Month", ylab = "Combined Mean Value (Scaled)")

# ── ggplot2 ──────────────────────────────────────────────────────────────────

# 5. Count bar chart
ggplot(airquality, aes(x = Month)) +
  geom_bar(fill = "darkslateblue") +
  labs(
    title = "How Many Days Were Recorded Each Month?",
    subtitle = "Based on New York airquality dataset (1973)",
    x = "Month", y = "Number of Observations"
  ) +
  theme_minimal()

# 6. Aggregated bar chart
ggplot(airquality, aes(x = Month, y = Ozone)) +
  stat_summary(fun = mean, geom = "bar", fill = "firebrick", na.rm = TRUE) +
  labs(
    title = "Peak Ozone Levels: Which Month Was Most Polluted?",
    subtitle = "Mean ozone concentration (ppb), NA values excluded",
    x = "Month", y = "Mean Ozone (ppb)"
  ) +
  theme_minimal()

# 7. Grouped bar chart
ggplot(airquality, aes(x = Month, y = Ozone, fill = Month)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", na.rm = TRUE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Monthly Ozone Levels at a Glance",
    subtitle = "Each bar represents the monthly mean ozone (ppb)",
    x = "Month", y = "Mean Ozone (ppb)", fill = "Month"
  ) +
  theme_minimal()

# 8. Stacked bar chart
airquality_long <- pivot_longer(airquality, cols = c(Ozone, Solar.R),
                                names_to = "Measure", values_to = "Value")

ggplot(airquality_long, aes(x = Month, y = Value, fill = Measure)) +
  stat_summary(fun = mean, geom = "bar", position = "stack", na.rm = TRUE) +
  scale_fill_manual(values = c("Ozone" = "coral2", "Solar.R" = "mediumseagreen"),
                    labels = c("Ozone (ppb)", "Solar Radiation (Lang)")) +
  labs(
    title = "Ozone & Solar Radiation: Monthly Composition",
    subtitle = "Stacked means show relative contribution of each variable per month",
    x = "Month", y = "Mean Value", fill = "Measurement"
  ) +
  theme_minimal()