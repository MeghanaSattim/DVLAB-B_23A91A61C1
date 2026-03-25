#install the required libraries
install.packages("maps")
install.packages("mapdata")
install.packages("mapproj")
#load the libraries
library(maps)
library(mapdata)
library(mapproj)
#basic world map(base r)
map("world")
#map with title and colors
map(
  "world",
  col="lightblue",
  fill=TRUE,
  bg="white",
  main="World Map Visualization"
)
#map of a specific country
map(
  "world",
  regions="India",
  fill=TRUE,
  col="green",
  main="Map of India"
)
#map with points (locations)
#example: major citiwa in india
map("world",fill=TRUE,col="lightgray")
points(
  x=c(77.2090,72.8777,80.2707),
  y=c(28.6139,19.0760,13.0827),
  col="red",
  pch=19
)
# Cities plotted:  
# | City    | Coordinates        |
# | ------- | ------------------ |
# | Delhi   | (77.2090, 28.6139) |
# | Mumbai  | (72.8777, 19.0760) |
# | Chennai | (80.2707, 13.0827) |

# Labeling Locations  
map("world", fill = TRUE, col = "lightyellow")
points(77.2090,28.6139,pch=19,col="red")
text(
  77.2090,
  28.6139,
  labels = "Delhi",
  pos = 4)
# Map Using ggplot2
library(ggplot2)
world_map <- map_data("world")
# Basic map:  
ggplot(world_map, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  coord_fixed() +
  theme_minimal()
# Highlighting a Country  
ggplot(world_map, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgray", color = "black") +
  geom_polygon(
    data = subset(world_map, region == "India"),
    fill = "green") +
  coord_fixed() +
  theme_minimal()		
# India becomes highlighted.
# Map with Data Points (ggplot)
cities <- data.frame(
  city = c("Delhi","Mumbai","Chennai"),
  lon = c(77.2090,72.8777,80.2707),
  lat = c(28.6139,19.0760,13.0827))

ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  geom_point(
    data = cities,
    aes(x = lon, y = lat),
    color = "red",
    size = 3
  ) +
  coord_fixed(xlim = c(65, 100), ylim = c(5, 40)) +
  theme_minimal()

# Maps often require plong# Maps often require projections.
map(
  "world",
  proj = "orthographic",
  orientation = c(20,80,0)
)

map(
  "state",
  fill = TRUE,
  col = rainbow(50),
  main = "US State Map"
)