library(tidyverse)
library(sf)
library(leaflet)


states = USAboundaries::us_states()

# grepl searches for patterns in string names
tmp = states %>%
  filter(grepl("South", state_name))
plot(tmp$geometry, col = "red")

# In part 4 of lab, search for I for example

# Q1 is making tesselations
# 2 - give summary statistics
# 3 - counting number of dams in each, using PIP
# 4 - decide which tesselation best represents the data, look at distribution and purpose

state.of.interest = "Alabama"

soi = filter(states, state_name == state.of.interest)

# What are its neighbors?
adjoining = st_filter(states, soi, .predicate = st_touches)
plot(adjoining$geometry, col = "red")

# Joiing whichever state is closest to each point
closest = st_make_grid(soi, n = 70, square = FALSE) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

plot(closest)

vor = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast %>%
  st_sf() %>%
  st_join(closest) %>%
  group_by(state_name) %>%
  summarise() %>%
  st_intersection(soi)


leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = st_transform(vor, 4326), fillColor = ~colorFactor("YlOrRd", state_name)(state_name), color = NA) %>%
  addPolygons(data = st_transform(soi, 4326),
              fillColor = "transparent", color = "black", group = "SOI") %>%
  addPolygons(data = st_transform(adjoining, 4326), fillColor = ~colorFactor("YlOrRd", state_name)(state_name), col = NA) %>%
  addLayersControl(overlayGroups = c("SOI"))

