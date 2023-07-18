# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

library(tidyverse)
library(cowplot)
source("tidytuesday/2023/istance.R")

tuesdata <- tidytuesdayR::tt_load('2023-07-11')
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps
zonann_temps <- tuesdata$zonann_temps


##========= calculate over the two hemispheres =================================
## NORTHERN HEMISPERE

nh_temps_long <- nh_temps %>%
  filter(Year != 2023) %>%
  select(c("Year", "DJF", "MAM", "JJA", "SON")) %>%
  pivot_longer(!Year, names_to = "met_season", values_to = "temperature")

## create instance charts with different colour gradients

is_nh <- istance(nh_temps_long) +
  theme(plot.background = element_rect(
    fill = "grey98",
    colour = NA))

## SOUTHERN HEMISPERE
sh_temps_long <- sh_temps %>%
  filter(Year != 2023) %>%
  select(c("Year", "DJF", "MAM", "JJA", "SON")) %>%
  pivot_longer(!Year, names_to = "met_season",
               values_to = "temperature")

is_sh <- istance(sh_temps_long) +
  theme(plot.background = element_rect(
    fill = "grey98",
    colour = NA))



## create map of northern hemisphere
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
nh <- ggplot(data = world) +
  geom_sf(fill = "beige") +
  coord_sf(ylim=c(90,0)) +
  theme_minimal() +
  theme(axis.text = element_blank())

sh <- ggplot(data = world) +
  geom_sf(fill = "beige") +
  coord_sf(ylim=c(-90,0)) +
  theme_minimal()


## add all together
library(cowplot)
plot_panel <- plot_grid(nh, sh,
                        is_nh, is_sh,
                        labels = c("Northern Hemisphere",
                                   "Southern Hemisphere"),
                        ncol = 2) +
  theme(plot.margin = margin(20, 3, 30, 8),
        plot.background = element_rect(
          fill = "grey98",
          colour = NA))

legend <- get_legend(
  # create some space to the left of the legend
  is_nh +
    theme(legend.box.margin = margin(0, 0, 0, 12))
)

## create title layer
title_gg <- ggplot() +
  labs(title = "Is the world getting hotter?",
       subtitle = "Surface temperature change across the years for the two hemispheres") +
  theme(
    plot.title = element_text(size = 24,
                              hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(size = 16,
                                 hjust = 0.5),
    plot.margin = margin(18, 0, 12, 0),
    plot.background = element_rect(
      fill = "grey98", colour = NA)
  )

panel <- plot_grid(title_gg,
                   plot_panel,
                   ncol = 1, rel_heights = c(0.1, 1),
                   align = "v") +
  theme(
    plot.background = element_rect(
      fill = "grey98",
      colour = NA)
  )

panel_footnote <- add_sub(panel,
                          label = "Design: Rita Giordano.  Source: NASA GISS Surface Temperature Analysis (GISTEMP v4)",
                          fontface = "bold",
                          size = 10,
                          hjust = -0.3)
txt <- "The labels in the stripes charts are `metereological seasons`: \nDJF: Dec-Jan-Feb, JJA: Jun-Jul-Aug,\n MAM: Mar-Apr-May, SON: Sep-Oct-Nov"

ggdraw(panel_footnote) +
  geom_text(
    aes(x = 0.2, y = 0.04, label = txt),
    size = 12/.pt,
  )
