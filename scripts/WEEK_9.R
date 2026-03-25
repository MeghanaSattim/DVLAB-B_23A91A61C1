#Install package
install.packages("leaflet")
#Load library
library(leaflet)
#Basic Map
leaflet() %>% addTiles()
#Map centered on India
leaflet() %>%
  addTiles() %>%
  setView(lng = 78.9629, lat = 20.5937, zoom = 5)
#Marker Example
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 77.2088, lat = 28.6139, popup = "New Delhi")
#Cities Data
cities <- data.frame(
  name = c("Delhi","Mumbai","Chennai","Kolkata"),
  lat = c(28.6139,18.9582,13.0827,22.5744),
  lng = c(77.2088,72.8321,80.2707,88.3629))
#Multiple markers
leaflet(data = cities) %>% addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = ~name)
#Circle markers
leaflet(data = cities) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = 6,
    color = "red",
    popup = ~name
  )
#Add population data
cities$pop <- c(19000000,20000000,11000000,15000000)
#Color palette
pal <- colorNumeric("Reds", cities$pop)
#Map with color palette
leaflet(data = cities) %>% addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~pop/2000000, color = ~pal(pop),
    popup = ~name )
#Add legend
leaflet(data = cities) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~pop/2000000,
    color = ~pal(pop), popup = ~name
  ) %>%
  addLegend(
    "bottomright", pal = pal,
    values = ~pop,
    title = "Population"
  )
#Different map styles
leaflet() %>% addProviderTiles("OpenStreetMap")
leaflet() %>% addProviderTiles("CartoDB.DarkMatter")
leaflet() %>% addProviderTiles("CartoDB.Positron")
leaflet() %>% addProviderTiles("Esri.WorldImagery")
#Advanced map with layers
leaflet(data = cities) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = 8,
    color = "blue",
    group = "Cities"
  ) %>%
  addLayersControl(
    overlayGroups = c("Cities"),
    options = layersControlOptions(collapsed = FALSE) )