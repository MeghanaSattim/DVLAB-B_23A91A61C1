data(airquality)

# Understand the data
str(airquality)
View(airquality)
class(airquality)
?airquality

# Convert Month to factor with labels for better readability
airquality$Month <- factor(airquality$Month,
                           labels = c("May", "Jun", "Jul", "Aug", "Sep"))

# Basic Box Plot
boxplot(airquality$Ozone)

# Add Title & Labels
boxplot(airquality$Ozone,
        main = "Box Plot for Ozone Concentration",
        ylab = "Ozone (ppb)",
        col = "skyblue")

# Grouped Box Plot (by Month)
boxplot(Ozone ~ Month,
        data = airquality,
        main = "Ozone Concentration by Month",
        xlab = "Month",
        ylab = "Ozone (ppb)",
        col = c("pink", "green", "skyblue", "orange", "lavender"))

# Multi-Variable Box Plot

boxplot(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")],
        main = "Multi Variable Box Plot - airquality Parameters",
        col = c("pink", "green", "skyblue", "orange"),
        na.action = na.omit)

# Using Custom Color Palette
month_colors <- c("May"  = "red",
                  "Jun"  = "steelblue",
                  "Jul"  = "green",
                  "Aug"  = "orange",
                  "Sep"  = "purple")
boxplot(
  Ozone ~ Month,
  data = airquality,
  col = month_colors,
  main = "Ozone Concentration by Month (Custom Color Palette)",
  xlab = "Month",
  ylab = "Ozone (ppb)"
)

library(ggplot2)

ggplot(airquality,
       aes(x = Month, y = Ozone)) +
  geom_boxplot() +
  theme_minimal()

# Using Manual Color Palette
ggplot(airquality, aes(x = Month, y = Ozone, fill = Month)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "May" = "red",
      "Jun" = "steelblue",
      "Jul" = "green",
      "Aug" = "orange",
      "Sep" = "purple"
    )
  ) +
  labs(
    title = "Ozone Concentration by Month",
    x = "Month",
    y = "Ozone (ppb)"
  ) +
  theme_minimal()