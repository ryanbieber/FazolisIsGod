library(ggmap)
library(ggplot2)
origin <- data.frame(lon = -92.50606, lat = 44.08087)

latlong <- geocode("1600 Pennsylvania Avenue NW Washington, DC 20500")
la = get_map(location = c(lon = latlong$lon, lat = latlong$lat), maptype = "terrain", source = "google", zoom = 12)
map = ggmap(sf)