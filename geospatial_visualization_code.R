# ==== SETUP ====
# Install once (remove the # to run):
# install.packages(c("tidyverse","sf","rnaturalearth","rnaturalearthdata",
#                    "hexbin","viridis","leaflet","gstat"))
# remotes::install_github("ropensci/rnaturalearthhires")   # state boundaries

library(tidyverse)
library(sf)
library(rnaturalearth)
library(hexbin)
library(viridis)
library(leaflet)
library(gstat)

sf_use_s2(FALSE)

# Load the dataset (keep the CSV in the working directory)
eq <- read.csv("india_earthquake_events.csv")
str(eq)
summary(eq)

# Convert to a spatial (sf) object
eq_sf <- st_as_sf(eq, coords = c("Longitude", "Latitude"),
                  crs = 4326, remove = FALSE)

# India state boundaries
india <- ne_states(country = "india", returnclass = "sf")
india_outline <- st_union(india)

# Common map theme
theme_map <- theme_void() +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey35"),
        legend.position = "right")

base_map <- geom_sf(data = india, fill = "grey96",
                    colour = "grey70", linewidth = 0.2)

# ==== 1. PROPORTIONAL SYMBOL MAP ====
ggplot() +
  base_map +
  geom_point(data = eq,
             aes(Longitude, Latitude, size = Magnitude, colour = Depth_km),
             alpha = 0.6) +
  scale_size_continuous(range = c(1, 9)) +
  scale_colour_viridis_c(option = "magma", direction = -1,
                         name = "Depth (km)") +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  labs(title = "Proportional Symbol Map of Earthquakes in India",
       subtitle = "Symbol size = magnitude, colour = focal depth") +
  theme_map

# ==== 2. CHOROPLETH MAP ====
# Assign every earthquake to a state using a spatial join
eq_state <- st_join(eq_sf, india[, "name"])

state_stats <- eq_state %>%
  st_drop_geometry() %>%
  filter(!is.na(name)) %>%
  group_by(name) %>%
  summarise(events = n(), mean_mag = mean(Magnitude), .groups = "drop")

india_choro <- left_join(india, state_stats, by = "name")

ggplot(india_choro) +
  geom_sf(aes(fill = events), colour = "white", linewidth = 0.25) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       na.value = "grey92", name = "Events") +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  labs(title = "Choropleth Map: Earthquake Count by State / UT",
       subtitle = "Grey = no recorded events") +
  theme_map

# ==== 3. SPATIAL HEAT MAP ====
ggplot() +
  base_map +
  stat_density_2d(data = eq,
                  aes(Longitude, Latitude, fill = after_stat(level)),
                  geom = "polygon", alpha = 0.55, bins = 12) +
  scale_fill_viridis_c(option = "inferno", direction = -1,
                       name = "Density") +
  geom_sf(data = india, fill = NA, colour = "grey60", linewidth = 0.15) +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  labs(title = "Spatial Heat Map of Earthquake Concentration",
       subtitle = "Kernel density estimate of event locations") +
  theme_map

# ==== 4. HEXBIN MAP ====
ggplot() +
  base_map +
  geom_hex(data = eq, aes(Longitude, Latitude), bins = 22, alpha = 0.85) +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Events") +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  labs(title = "Hexbin Map of Earthquake Frequency",
       subtitle = "Each hexagon counts the events inside it") +
  theme_map

# ==== 5. CONTOUR MAP ====
ggplot() +
  base_map +
  geom_point(data = eq, aes(Longitude, Latitude),
             colour = "grey40", size = 0.6, alpha = 0.5) +
  geom_density_2d(data = eq, aes(Longitude, Latitude),
                  colour = "black", linewidth = 0.4, bins = 10) +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  labs(title = "Density Contour Map of Seismic Activity",
       subtitle = "Closer contour lines indicate a steeper change in density") +
  theme_map

# ==== 6. FACETED MAP (SMALL MULTIPLES BY YEAR) ====
ggplot() +
  base_map +
  geom_point(data = eq,
             aes(Longitude, Latitude, size = Magnitude, colour = Magnitude),
             alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  scale_colour_viridis_c(option = "magma", direction = -1) +
  coord_sf(xlim = c(67, 98), ylim = c(6, 37)) +
  facet_wrap(~ Year, ncol = 4) +
  labs(title = "Year-wise Spatial Pattern of Earthquakes (2018 - 2025)") +
  theme_map +
  theme(strip.text = element_text(face = "bold"))

# ==== 7. INTERACTIVE CLUSTER MAP ====
pal <- colorNumeric("YlOrRd", domain = eq$Magnitude)

leaflet(eq) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    radius = ~Magnitude * 1.6,
    color = ~pal(Magnitude), fillOpacity = 0.75, stroke = FALSE,
    popup = ~paste0("<b>", State_UT, "</b><br>Date: ", Date,
                    "<br>Magnitude: ", Magnitude,
                    "<br>Depth: ", Depth_km, " km",
                    "<br>Damage: ", Damage_Level),
    clusterOptions = markerClusterOptions()
  ) %>%
  addLegend("bottomright", pal = pal, values = ~Magnitude,
            title = "Magnitude")

# ==== 8. IDW SPATIAL INTERPOLATION SURFACE ====
crs_proj  <- 7755                      # WGS 84 / India NSF LCC (metres)
eq_proj   <- st_transform(eq_sf, crs_proj)
india_prj <- st_transform(india_outline, crs_proj)

# 40 km prediction grid clipped to India
grid <- st_make_grid(india_prj, cellsize = 40000, what = "polygons") %>%
  st_sf() %>%
  st_filter(india_prj)

centres  <- st_centroid(grid)
idw_pred <- idw(Magnitude ~ 1, locations = eq_proj,
                newdata = centres, idp = 2)
grid$pred <- idw_pred$var1.pred

ggplot() +
  geom_sf(data = grid, aes(fill = pred), colour = NA) +
  geom_sf(data = india_prj, fill = NA, colour = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Predicted\nmagnitude") +
  labs(title = "IDW Interpolated Magnitude Surface",
       subtitle = "Inverse distance weighting, power = 2") +
  theme_map
