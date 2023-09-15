library(tidytuesdayR)
library(tidyverse)
library(voronoiTreemap)
library(VoronoiPlus)
library(patchwork)
library(ggrepel)
library(monochromeR)
library(RColorBrewer)
library(purrr)
library(cowplot)

tuesdata <- tt_load('2023-09-12')

all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity

colnames(all_countries)[3] <- "country_iso3"

## join to all_countries the countries names
all_countries_name <- left_join(
  all_countries %>%
    select(-region_code),
  country_regions %>%
    select(region_name, country_name, country_iso3),
  by = "country_iso3"
)

all_countries_ne <- all_countries_name %>%
  filter(region_name == "Northern Europe") %>%
  select(hoursPerDayCombined, Category, Subcategory)

## create colours palette
paired <- brewer.pal(name = "Paired", 8)

## create voronoi tree map for Northern Europe
europe <- voronoi_treemap(
  hoursPerDayCombined ~ Category + Subcategory,
  data = all_countries_ne)

groups <- groups_df(europe)
subgroups <- subgroups_df(europe)


Denmark <- all_countries_name %>%
  filter(country_name == "Denmark") %>%
  select(hoursPerDayCombined, Category, Subcategory)

dk_vor <- voronoi_treemap(
  hoursPerDayCombined ~ Category + Subcategory,
  data = Denmark)

dk_groups <- groups_df(dk_vor)

dk_subgroups <- subgroups_df(dk_vor)


sweden <- all_countries_name %>%
  filter(country_name == "Sweden") %>%
  select(hoursPerDayCombined, Category, Subcategory)

se_vor <- voronoi_treemap(
  hoursPerDayCombined ~ Category + Subcategory,
  data = sweden)

se_groups <- groups_df(se_vor)

se_subgroups <- subgroups_df(se_vor)

finland <- all_countries_name %>%
  filter(country_name == "Finland") %>%
  select(hoursPerDayCombined, Category, Subcategory)

fi_vor <- voronoi_treemap(
  hoursPerDayCombined ~ Category + Subcategory,
  data = finland)

fi_groups <- groups_df(fi_vor)

fi_subgroups <- subgroups_df(fi_vor)

iceland <- all_countries_name %>%
  filter(country_name == "Iceland") %>%
  select(hoursPerDayCombined, Category, Subcategory)

is_vor <- voronoi_treemap(
  hoursPerDayCombined ~ Category + Subcategory,
  data = iceland)

is_groups <- groups_df(is_vor)

is_subgroups <- subgroups_df(is_vor)

## create voronoi map

## create label centred on the polygon

label_ne <- subgroups %>%
  group_by(group) %>%
  reframe(x = mean(x), y = mean(y), value = max(value))

ne <- voronoi(groups, subgroups, name = "Northern Europe")
ne_text <- ne +
  geom_text_repel(
    filter(label_ne, value > 5),
    mapping = aes(x, y, label = group, size = 8)
  )

## create legend chart
legend <- groups %>%
  distinct(group, .keep_all = TRUE) %>%
  select(group, colour) %>%
  group_by(group)

legend$x <- 1
legend$id <- row_number(unique(groups$group))

gg_leg <- legend %>%
  ggplot(aes(x, id, colour = group)) +
  geom_point(size = 0) +
  theme_void() +
  scale_colour_manual(values = paired) +
  geom_text(aes(x, id, label = group, hjust = 0),
            size = 8) +
  theme(legend.position = "none",
       plot.margin = unit(c(1,-1,1,1),"cm"),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       ) +
  scale_x_continuous(limit = c(0.999,1.15), expand = c(0, 0))

dk <- voronoi(dk_groups, dk_subgroups, "Denmark")

se <- voronoi(se_groups, se_subgroups, "Sweden")

fi <- voronoi(fi_groups, fi_subgroups, "Finland")

no <- voronoi(no_groups, no_subgroups, "Norway")

is <- voronoi(is_groups, is_subgroups, "Iceland")

layout <- c(
  area(1, 1, 2, 2),
  area(1, 3),
  area(3, 3),
  area(2, 3),
  area(3, 2),
  area(3, 1)
)

theme_border <- theme_gray() +
  theme(plot.background = element_rect(fill = "#FFFFFF", colour = NA))

ne_text + dk + gg_leg +se + fi +  is +
  plot_layout(design = layout,
              guides = 'collect') +
  plot_annotation(
    title = "The Global Human Day",
    subtitle =
    "
    The daily activities of people living in Northern Europe occupy 24 hours per day.
    These activities are split into eight main groups.
    Sleep and bedrest, in the somatic maintenance group, take up the majority of the time.
    On average, people in Northern Europe spend 8.8 hours per day.
    In Finland, the average time spent in sleep and bedrest is 8.9 hours per day.
    ",
    caption = "Design: Rita Giordano, Source: The Human Chronome Project",
    theme = theme_border
  ) & theme(plot.title = element_text(size = 24, hjust = 0.4,
                                      face = "bold"),
            plot.subtitle = element_text(size = 20, hjust = 0, face = "bold"),
            plot.caption = element_text(size = 10, face = "bold"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none")
