
library(tidyverse)
library(ggpubr)
library(circlize)
library(cowplot)
library(RColorBrewer)
library(camcorder)
source("R/circos.R")

## load the data
survivalists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

# Items data set

item <- left_join(
  survivalists,
  loadouts,
  by = c("season", "name")) %>%
  distinct()

item_count <- item %>%
  count(item)


##dot chart using ggpubr

gg_record(
  dir = "img/recording",
  device = "png",
  width = 10,      # width of saved image
  height = 6,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

p1 <- ggdotchart(
  item_count,
  x = "item",
  y = "n",
  color = "#93DECA",
  sorting = "descending",
  rotate = TRUE,
  add = "segments",
  dot.size = 8,
  label = round(item_count$n),
  font.label = list(color = "#284450", size = 10, vjust = 0.5),
  ggtheme = theme_void()
  ) +
  xlab("") + ylab("") +
  ggtitle(" ") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(colour = "white"),
    axis.title.y = element_text(),
    plot.title = element_text(colour = "white", hjust = 0.5)
    )

## Build the matrix for the chord diagram
## Count items by each season
item_n <- item %>%
  group_by(season, item) %>%
  summarise(n_item = sum(item_number))

item_wider <- item_n %>%
  pivot_wider(names_from = item, values_from = n_item) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

item_matrix <-  item_wider %>%
  column_to_rownames("season") %>%
  as.matrix()

## Create colour palette for the chord diagram
cb_palette <- brewer.pal(9, "RdBu") ## colour blind friendly

## create structure to assign to each season a specific color based on the
## items.
role_colour <- structure(cb_palette, names = item_wider$season)
colnames(item_wider[2:28])
item_wider_colour <- structure(rep("#F7F7F7",
                                            length(colnames(item_wider[2:28]))),
                               names = colnames(item_wider[2:28]))
## create a colour grid for the chord diagram
col_grid <- c(item_wider_colour, role_colour)

## formula format for circlize charts
p2 <- ~circos_function(item_matrix, "", col_grid)

title_gg <- ggplot() +
  labs(title = "Which item do I need? A pot or a soap?",
       subtitle = "The participants of Alone, the survival TV show, were allowed to bring with them 10 items. The most popular across all the seasons was a pot.") +
  theme(
    plot.title = element_text(size = 24, colour = "white",
                              hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(size = 16, colour = "white", hjust = 0.5),
    plot.background = element_rect(fill = alpha("#284450", 0.8),
                                   colour = alpha("#284450", 0.8)),
    plot.margin = margin(18, 0, 12, 0)
  )

subtitle_gg <- ggplot() +
  labs(caption = "Source: TV series Alone, Design: Rita Giordano") +
  theme(
    plot.caption = element_text(size = 10, colour = "white",
                                hjust = 0.85,
                                face = "bold"),
    plot.background = element_rect(fill = alpha("#284450", 0.8),
                                   colour = alpha("#284450", 0.8))
    #plot.margin = margin(18, 0, 12, 0)
  )

plot_row <- plot_grid(p1,
                      p2,
                      labels = c("Favourite items across all seasons",
                                 "Items per each season"),
                      ncol = 2,
                      label_colour = "white",
                      label_size = 14,
                      hjust = 0,
                      label_x = 0.31,
                      #align = 'v',
                      rel_widths = c(0.2, 0.3),
                      rel_heights = c(0.01, 0.001)
) +
  theme(plot.margin = margin(14, 3, 4, 8))
subtitle_cow <- add_sub(plot_row,
                        "Source: Alone, Design: Rita Giordano",
                        fontface = "bold",
                        size = 10,
                        color = "white",
                        hjust = -1.2)

plot_grid(title_gg,
          ggdraw(subtitle_cow),
          ncol = 1, rel_heights = c(0.1, 1)) +
  theme(plot.background = element_rect(fill = alpha("#284450", 1)))

