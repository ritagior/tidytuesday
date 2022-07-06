# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(tidyverse)
library(lubridate)
library(showtext)
library(PostcodesioR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggdist)
library(patchwork)
library(vroom)
library(fs)
library(gghighlight)
tuesdata <- tidytuesdayR::tt_load('2022-06-28')
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)

paygap <- tuesdata$paygap

write_csv(paygap, "data/paygap_uk.csv")

clean_df <- paygap |>
  janitor::clean_names() |>
  mutate(
    across(c(due_date, date_submitted),lubridate::as_datetime),
    employer_name = str_remove_all(employer_name, "\""),
    employer_name = str_replace_all(employer_name, ", |,", ", ")
  )

clean_df |>
  glimpse()

clean_df |>
  write_csv("data/paygap_uk_clean.csv")

#extract year from due date
paygap_df <- clean_df %>%
  mutate(year = year(due_date),
         county = gsub('[[:digit:]]+', '',
                       str_extract(post_code, "\\w{2}"))) %>%
  select(-c("address", "company_number", "sic_codes",
            "company_link_to_gpg_info", "responsible_person", "current_name",
            "submitted_after_the_deadline", "due_date", "date_submitted"))
county <- paygap_df %>%
  count(county)

england <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf',
                        continent = "Europe")
england <- england %>%
  filter(geounit == "England")

## select only company in England
wales_pc <- c("CF", "LD", "LL", "NP", "SA")
ni <- "BT"
scot <- c("AB", "DD", "DG", "EH", "FK", "G", "HS","IV", "KA", "KW", "KY", "ML",
          "PA", "PH", "TD", "ZE")
paygap_en <- paygap_df %>%
  select(employer_name, diff_median_hourly_percent, post_code, county, year,
         ) %>%
  filter(!county %in% wales_pc,
         !county %in% ni,
         !county %in% scot,
         !is.na(post_code))

coor_en <- sapply(1:nrow(paygap_en), function(x) {
  postcode_lookup(paygap_en$post_code[x])
})

paygap_en$lat <- sapply(1:nrow(paygap_en), function(x) {
  coor_en[[x]]$latitude
})

paygap_en$lon <- sapply(1:nrow(paygap_en), function(x) {
  coor_en[[x]]$longitude
})

## diff_median_hourly_percent > 0 : male higher pay
## diff_median_hourly_percent ==  0: equal pay
## diff_median_hourly_percent < 0 : female higher pay
paygap_en_2021 <- paygap_en %>%
  filter(!is.na(lat),
         year == 2021) %>%
  mutate(pay = case_when(
    diff_median_hourly_percent > 0 ~ "male",
    diff_median_hourly_percent == 0 ~ "equal",
    TRUE ~ "female"
  ))
pay <- paygap_en_2021 %>%
  count(pay) %>%
  mutate(percent = round((n / nrow(paygap_en_2021)) * 100))

company <- paygap_en %>%
  count(employer_name)

slice_min(paygap_en_2021 %>%
            filter(county == "CB"), n = 1, diff_median_hourly_percent)
slice_max(paygap_en_2021 %>%
            filter(county == "CB"), n = 1, diff_median_hourly_percent)

string <- usefunc::str_wrap_break("In England the 79% of the registered companies pay men higher than women. 13% pay women higher than men. Only 8% pay their employee an equal amount regardless the gender. In Cambridgeshire very few company give fair pay.",
                                  40)

p1 <- ggplot() +
  geom_sf(data = england, colour = "#225ea8") +
  geom_point(data = paygap_en_2021 %>%
               filter(diff_median_hourly_percent >= -200),
             aes(x = lon, y = lat, fill = diff_median_hourly_percent),
             size = 3,
             pch = 21,
             colour = "transparent",
             alpha = 0.8) +
  theme_void() +
  labs(title = "England's Gender Pay Gap",
       tag = string
  ) +
  guides(fill=guide_colorbar(title.position="top",
                             title.hjust =0.5)) +
  scale_fill_gradient2(name = "Median % Pay Gap",
                       high = "#0571b0",
                       low = "#ca0020",
                       mid = "#f7f7f7",
                       limits = c(-250, 150), breaks = c(-200, 0, 100)) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.5, 5, 0.5, -1), unit = "cm"),
        plot.tag = element_text(size = 10, colour = "#225ea8", family="sans", face="italic"),
        plot.tag.position = c(1.3, 0.7) ,
        plot.title = element_text(colour = "#225ea8", size=28, hjust = 0,
                                  vjust=0, family="serif", face="italic"),
        legend.text = element_text(size = 10, colour = "#225ea8", family="sans", face="italic"),
        legend.title = element_text(size = 10, colour = "#225ea8", family="sans", face="italic"),
  )


paygap_cb <- paygap_en %>%
  filter(county == "CB",
         !is.na(lat)) %>%
  mutate(employer_name = tolower(employer_name)) %>%
  left_join(., clean_df %>%
              mutate(employer_name = tolower(employer_name)) %>%
              select(employer_name, employer_size)) %>%
  distinct()

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p2 <- paygap_cb %>%
  ggplot(aes(x = year, y =diff_median_hourly_percent,
             colour = employer_name)) +
  geom_line(size = 0.5) + theme_minimal() +
  gghighlight(min(diff_median_hourly_percent) > -5,
              max(diff_median_hourly_percent) < 5,
              calculate_per_facet = FALSE) +
  ylab("Median % pay gap") +
  #scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = cbbPalette) +
  facet_wrap(~ employer_size) +
  # theme(axis.title.x = element_text( size=22),
  #       axis.text.x  = element_text(size=16),
  #       axis.title.y = element_text( size=22),
  #       axis.text.y  = element_text(size=16),
  #       legend.title = element_text(size = 18),
  #       legend.text = element_text(size = 14)) +
  theme(text = element_text(color = "#225ea8"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "grey",
                                          linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white",
                                        color = "grey")) +
        #strip.text = element_text(size = 24)) +
  ggtitle("Company with fair pay in Cambridgeshire")
  # theme(plot.title = element_text(size = 28, face = "bold"),
  #       plot.caption=element_text(size=14, face="italic")) +
  # labs(caption = "Source: UK gender pay gap")

slice_min(paygap_cb, n = 10, diff_median_hourly_percent)
slice_max(paygap_cb, n = 5, diff_median_hourly_percent)


p = p1 + inset_element(p2, left = 1, right = 2, top = 0.6, bottom = 0.1)
p


