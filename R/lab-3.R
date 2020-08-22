## Project: Lab 03 TR
## Script purpose: Week 3
## Date: August 20

library(tidyverse)
library(sf)
library(units)
library(ggrepel)
library(gghighlight)

region = data.frame(region = state.region, state_name = state.name)

south = USAboundaries::us_states() %>%
  right_join(region, by = "state_name") %>%
  filter(region == "South")

# Use this if you don't want to take the time for a ggplot
plot(south)

plot(south$geometry)

# Pick a specific column name
plot(south['aland'])

# Read cities
cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(south, .predicate = st_intersects)

plot(south$geometry)
plot(cities$geometry, add = TRUE, pch = 16, cex = 0.1)

class(cities)
# We know its not yet spatial, but it can be
# We need to assign crs, has lat and lon, know it is crs = 4326


south_c = st_combine(south) %>%
  st_cast("MULTILINESTRING")

# Great Circle distances (not in degrees) would take forever to calculate
# Equal area projection

south_c = st_transform(south_c, 5070)
cities = st_transform(cities, 5070)

cities = cities %>%
  mutate(dist_to_state = st_distance(cities, south_c),
         dist_to_state = units::set_units(dist_to_state, "km"),
         dist_to_state = units::drop_units(dist_to_state))

big_cities = cities %>%
  group_by(state_name) %>%
  slice_max(population, n = 1)

ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(col = dist_to_state), size = .1) +
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red") +
  ggthemes::theme_map() +
  ggrepel::geom_label_repel(
    data = big_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 4)


# New Feature- only apply color to a certain condition
ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(col = dist_to_state), size = .1) +
  gghighlight::gghighlight(population > 10000)
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red") +
  ggthemes::theme_map()


