View(airquality)
?airquality
class(airquality)
hist(airquality$Temp)
hist(airquality$Temp,
     main= "Basic Histogram of air quality",
     xlab = "Temperature",
     ylab = "Frequency",
     labels = TRUE)
hist(airquality$Temp,
     main= "Basic Histogram of air quality",
     xlab = "Temperature",
     ylab = "Frequency",
     labels = TRUE,
     col="pink",
     border="blue")
hist(airquality$Temp,
     breaks=5,
     main= "Basic Histogram of air quality",
     xlab = "Temperature",
     ylab = "Frequency",
     labels = TRUE,
     col="pink",
     border="purple")
library(ggplot2)
ggplot(
  airquality,
  aes(x=Temp)
)+
  geom_histogram(
    bins=8,
    fill="magenta",
    col="green")+
  labs(
    title="Air quality GGPLOT Histogram",
    y = "Frequency")+
  facet_wrap(year)
theme_light()