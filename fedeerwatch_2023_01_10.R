
library(tidyverse)
library(lubridate)
library(ggthemes)
library(googlesheets4)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(extrafont)
extrafont::font_import()

tuesdata <- tidytuesdayR::tt_load('2023-01-10')
tuesdata <- tidytuesdayR::tt_load(2023, week = 02)

feederwatch <- tuesdata$PFW_2021_public
site_data <- tuesdata$PFW_count_site_data_public_2021


## read googlsheet data on
gs4_deauth()
gs_bird <-
  "https://docs.google.com/spreadsheets/d/18Mzy3PMC2rNp_nQrn9b4PQO4Z7KoHlTSL1xfjeSwuoI/edit#gid=2040245914"
## extract sheed id
sheet_id <- as_sheets_id(gs_bird)
sheet <- c("Summary of Data Fields", "Observation Field Details",
           "Site Description Field Details", "Species Codes",
           "Taxonomic Updates")

species_code <- read_sheet(
  ss = gs_bird,
  sheet = sheet[4],
  skip = 1,
  col_names = TRUE
)

loc_id_count <- feederwatch %>%
  count(loc_id)

feederwatch_tidy <- feederwatch %>%
  filter(valid == 1) %>%
  select("loc_id", "latitude", "longitude", "Month", "Day", "Year",
         "species_code", "how_many", "obs_id")

birds_species <- left_join(
  feederwatch_tidy,
  species_code %>%
    select(species_code = SPECIES_CODE, PRIMARY_COM_NAME, SCI_NAME),
  by = "species_code"
) %>%
  mutate(bird_name = paste(PRIMARY_COM_NAME, " (", SCI_NAME, ")", sep = ""))

## Mutate primary common name to a factor, this will help to have line chart
## ordered by the most seen species.
species_count_3 <- birds_species %>%
  count(bird_name) %>%
  arrange(desc(n)) %>%
  top_n(3) %>%
  mutate(bird_name = factor(bird_name, levels = bird_name))


# select only the top 5 birds seen
bird_species_3 <- birds_species %>%
  filter(bird_name %in% species_count_3$bird_name) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d"),
         year_month = format(as.Date(date), "%Y-%m"))

## calculate weeks, this will help to calculate the average number of birds per
## week
### calculate friday that is the first day the measurement start.
friday<- bird_species_3[wday(bird_species_3$date) == 6, ]
start_date <- min(friday$date) # select first friday
bird_species_3$week <- floor(as.numeric(difftime(bird_species_3$date,
                                                  start_date, units = "weeks")))


week_count <- bird_species_3 %>%
  group_by(week, bird_name) %>% # use your grouping variables in addition to week
  summarise(weekly_count = sum(how_many), week_begin_date = min(date)) %>%
  mutate(first_day_week = wday(week_begin_date, label=TRUE))

## Calculate minimum and maximum observation per birds. This points will be displayed 
## on the sparkling lines.

mins_count <- week_count %>%
  group_by(bird_name) %>%
  slice(which.min(weekly_count))

maxs_count <- week_count %>%
  group_by(bird_name) %>%
  slice(which.max(weekly_count))

colour_bird <- c("Dark-eyed Junco" = "#d7191c",
                 "Downy Woodpecker" = "#fdae61",
                 "Northern Cardinal" = "#ffffbf")


p1 <- week_count %>%
  ggplot(aes(x = week_begin_date, y = weekly_count, group = 1, colour = bird_name)) +
  stat_smooth(se = FALSE, span = 0.1) +
  scale_colour_manual(values = c("#d7191c", "#fdae61","#1f78b4" )) +
  facet_wrap(bird_name ~ .,
             scales = "free_y", nrow = 3, as.table = TRUE) +
  geom_point(data = mins_count, colour = "#a6611a") +
  geom_text(
    data = mins_count,
    aes(label = round(weekly_count)),
    size = 5,
    vjust = -0.5,
    hjust = 0.5,
    colour = "#a6611a"
  ) +
  geom_point(data = maxs_count, colour = "grey30") +
  geom_text(
    data = maxs_count,
    aes(label = round(weekly_count)),
    size = 5,
    vjust = -0.5,
    hjust = 0.5,
    colour = "grey30") +
  #theme(legend.position = "none") +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color = "grey85"),
        panel.grid.minor.x = element_line(color = "grey85", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16,
                                  hjust = 0.1,
                                  face = "bold",
                                  family = "Lexend",
                                  margin = margin(15, 0, 15, 0)),
        axis.text.x = element_text(size = 12),
        plot.title =  element_text(size = 18,
                                   hjust = 0.5,
                                   face = "bold",
                                   family = "Lexend",
                                   margin = margin(10, 0, 10, 0)),
        plot.caption =  element_text(size = 10,
                                     #hjust = 0.5,
                                     face = "bold",
                                     family = "Lexend")
        ) +
  coord_cartesian(clip = "off") +
  scale_x_date(
    date_breaks = "4 weeks",
    date_minor_breaks = "1 weeks",
    date_labels = "%Y-%m-%d",
    limits = c(as.Date("2020-11-13"), NA)
    ) +
  labs(title = "Weekly observation")

## create map to show where this birds were seen.

north_america <- map_data("world", region = c("Canada", "USA"))

north_am <- ne_countries(scale = 'medium',
                            type = 'map_units',
                            returnclass = 'sf',
                            continent = "North America")

map <- ggplot() +
  geom_sf(data = north_am, colour = "grey60",
          fill = "#ffffd4",
          alpha = 0.1)

p2<- map +
  geom_point(data = bird_species_3,
             alpha = 0.5,
             size = 1,
             aes(x = longitude,
                 y = latitude,
                 colour = bird_name)) +
  scale_colour_manual(values = c("#d7191c", "#fdae61", "#1f78b4")) +
  ylim(min(bird_species_5$latitude) - 3 , max(bird_species_5$latitude) + 5 ) +
  xlim(min(bird_species_5$longitude) - 3 , max(bird_species_5$longitude)) +
  theme_void() +
  theme(
        legend.position = "none",
        plot.title = element_text(size = 18,
                                  hjust = 0.5,
                                  face = "bold",
                                  family = "Lexend",
                                  margin = margin(0, 0, 12, 0))) +
  labs(title = "Birds' observation location")

## Add the two charts together
(p1 + p2) +
  plot_annotation(title = "The three most observed bird species in North America by citizen scientist.",
                  subtitle = "November 2020 to April 2021",
                  caption = "Design: Rita Giordano,  Source: FedeerWatch by Cornell Lab of Ornithology and Birds Canada",
                  theme = theme(
                    plot.title = element_text(size = 24,
                                              hjust = 0.5,
                                              face = "bold",
                                              family = "Lexend",
                                              margin = margin(0, 0, 12, 0)),
                    plot.subtitle = element_text(size = 20,
                                                 hjust = 0.5,
                                                 face = "bold",
                                                 family = "Lexend",
                                                 margin = margin(0, 0, 12, 0)),
                    plot.caption = element_text(size = 10,
                                                hjust = 0.98,
                                                face = "bold",
                                                family = "Lexend"),
                    # plot.background = element_rect(colour = "#F8FFFF",
                    #                              fill = "#F8FFFF")
                    )
                  ) +
  guide_area() +
  plot_layout(heights = c(26, 4, 0, 0), widths = c(3, 4, 0, 0))
