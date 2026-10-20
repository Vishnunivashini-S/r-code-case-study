# Run once if the packages are not installed:
install.packages(c("ggplot2", "dplyr", "tidyr", "viridis", "akima",
                   "sf", "geodata", "stringr"))

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Import the supplied CSV file (a file picker window will open)
solar <- read.csv(file.choose(), stringsAsFactors = FALSE)
solar$Date  <- as.Date(solar$Date)
solar$Month <- factor(solar$Month,
                      levels = c("January", "March", "May", "July", "October"))

# Inspect the data
dim(solar)        # expected: 50 rows, 17 columns
names(solar)
head(solar)
summary(solar)
str(solar)


# ---------- 6.1 ----------
ggplot(solar, aes(x = reorder(City, GHI_kWh_m2_day, FUN = median),
                  y = GHI_kWh_m2_day, fill = City)) +
  geom_boxplot(show.legend = FALSE, outlier.colour = "red") +
  coord_flip() +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Solar Irradiation (GHI) Distribution by City",
       x = "City", y = "GHI (kWh/m2/day)") +
  theme_minimal()


# ---------- 6.2 ----------
ggplot(solar, aes(x = Month, y = Specific_Yield_kWh_kWp_day, fill = Month)) +
  geom_violin(trim = FALSE, alpha = 0.75, show.legend = FALSE) +
  geom_boxplot(width = 0.12, fill = "white", outlier.size = 1) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Violin Plot of Solar Panel Yield by Month",
       x = "Month", y = "Specific yield (kWh per kWp per day)") +
  theme_minimal()


# ---------- 6.3 ----------
irradiation <- solar %>%
  select(GHI_kWh_m2_day, DNI_kWh_m2_day) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Irradiation")

ggplot(irradiation, aes(x = Irradiation, fill = Type)) +
  geom_density(alpha = 0.55, adjust = 1.1) +
  scale_fill_manual(
    values = c(GHI_kWh_m2_day = "#E69F00", DNI_kWh_m2_day = "#0072B2"),
    labels = c(GHI_kWh_m2_day = "GHI (global horizontal)",
               DNI_kWh_m2_day = "DNI (direct normal)")) +
  labs(title = "Density of Global (GHI) and Direct (DNI) Irradiation",
       x = "Irradiation (kWh/m2/day)", y = "Estimated density",
       fill = "Radiation type") +
  theme_minimal() +
  theme(legend.position = "bottom")


# ---------- 6.4 ----------
ggplot(solar, aes(x = Month,
                  y = reorder(City, GHI_kWh_m2_day, FUN = mean),
                  fill = GHI_kWh_m2_day)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", GHI_kWh_m2_day)), size = 3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "GHI\n(kWh/m2/day)") +
  labs(title = "Heat Map of Solar Irradiation: City by Month",
       x = "Month", y = "City") +
  theme_minimal()


# ---------- 6.5 ----------
library(akima)

# Mean GHI per city (one value per location)
city_avg <- solar %>%
  group_by(City, Longitude, Latitude) %>%
  summarise(Mean_GHI = mean(GHI_kWh_m2_day), .groups = "drop")

# Interpolate the city values onto a regular grid
surf <- with(city_avg, akima::interp(
  x = Longitude, y = Latitude, z = Mean_GHI,
  nx = 100, ny = 100, linear = TRUE, duplicate = "mean"))

grid <- expand.grid(Longitude = surf$x, Latitude = surf$y)
grid$GHI <- as.vector(surf$z)
grid <- grid[is.finite(grid$GHI), ]

ggplot(grid, aes(x = Longitude, y = Latitude, fill = GHI)) +
  geom_raster() +
  geom_point(data = city_avg, aes(x = Longitude, y = Latitude),
             inherit.aes = FALSE, colour = "black", size = 1.8) +
  geom_text(data = city_avg, aes(x = Longitude, y = Latitude, label = City),
            inherit.aes = FALSE, size = 3, vjust = -0.9) +
  coord_equal() +
  scale_fill_viridis_c(option = "plasma", name = "Mean GHI\n(kWh/m2/day)") +
  labs(title = "Pseudocolor Map of Interpolated Mean GHI",
       x = "Longitude (degrees)", y = "Latitude (degrees)") +
  theme_minimal()


# ---------- 6.6 ----------
# Uses grid and city_avg created in section 6.5
brks <- seq(4.25, 6.00, by = 0.25)

ggplot(grid, aes(x = Longitude, y = Latitude, z = GHI)) +
  geom_contour_filled(breaks = brks) +
  geom_contour(breaks = brks, colour = "white", linewidth = 0.3) +
  geom_point(data = city_avg, aes(x = Longitude, y = Latitude),
             inherit.aes = FALSE, colour = "black", size = 1.8) +
  geom_text(data = city_avg, aes(x = Longitude, y = Latitude, label = City),
            inherit.aes = FALSE, size = 3, vjust = -0.9) +
  coord_equal() +
  scale_fill_viridis_d(name = "GHI range\n(kWh/m2/day)") +
  labs(title = "Contour Plot of Interpolated Mean GHI",
       x = "Longitude (degrees)", y = "Latitude (degrees)") +
  theme_minimal()


# ---------- 6.7 ----------
# Rebuild a coarser surface so this block can run on its own
library(akima)
city_avg <- solar %>%
  group_by(City, Longitude, Latitude) %>%
  summarise(Mean_GHI = mean(GHI_kWh_m2_day), .groups = "drop")

surf3d <- with(city_avg, akima::interp(
  x = Longitude, y = Latitude, z = Mean_GHI,
  nx = 50, ny = 50, linear = TRUE, duplicate = "mean"))

z <- surf3d$z
nr <- nrow(z); nc <- ncol(z)

# Colour each surface facet by its average height
zfacet <- (z[-1, -1] + z[-1, -nc] + z[-nr, -1] + z[-nr, -nc]) / 4
pal    <- viridis(100)
fcol   <- pal[cut(zfacet, 100)]

persp(x = surf3d$x, y = surf3d$y, z = z,
      theta = 35, phi = 30, expand = 0.55,
      col = fcol, border = NA, shade = 0.3,
      ticktype = "detailed", nticks = 4,
      xlab = "Longitude", ylab = "Latitude", zlab = "Mean GHI",
      main = "3D Height-Field Surface of Mean GHI")


# ---------- 6.8 ----------
library(sf)
library(geodata)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)

# Download India state boundaries (requires internet)
india_states <- geodata::gadm(country = "IND", level = 1, path = tempdir())
states_sf <- sf::st_as_sf(india_states)

# Mean GHI for each state in the CSV
state_mean <- solar %>%
  group_by(State) %>%
  summarise(Mean_GHI = mean(GHI_kWh_m2_day), .groups = "drop")

# Normalise state names before joining
clean_name <- function(x) str_replace_all(str_to_lower(x), "[^a-z]", "")
state_mean$join_name <- clean_name(state_mean$State)
states_sf$join_name  <- clean_name(states_sf$NAME_1)

# Check for unmatched names (should print character(0))
setdiff(state_mean$join_name, states_sf$join_name)

map_data <- left_join(states_sf, state_mean, by = "join_name")

ggplot(map_data) +
  geom_sf(aes(fill = Mean_GHI), colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90",
                       name = "Mean GHI\n(kWh/m2/day)") +
  labs(title = "Mean Solar Irradiation (GHI) by State",
       subtitle = "Grey regions have no observations in this sample",
       caption = "Source: India Solar Energy case-study dataset") +
  theme_minimal()
