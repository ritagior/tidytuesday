## set of function to create the voronoi map

## plot voronoi map
voronoi <- function(df1, df2,  name) {
  ggplot() +
    geom_polygon(data = df1,
                 mapping = aes(x = x, y = y, group = group, fill = group),
                 colour = "grey98",
                 linewidth = 1) +
    geom_polygon(data = df2,
                 mapping = aes(x = x, y = y, group = group, alpha = alpha),
                 fill = "grey98",
                 colour = "grey98",
                 linewidth = 0.3) +
    scale_alpha_identity() +
    theme_void() +
    scale_fill_manual(values = paired) +
    ggtitle(name) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, hjust = 0.5)
    )
}


### create dataframe groups
groups_df <- function(df) {
  groups <- df %>%
    filter(level == 1) %>%
    group_by(group) %>%
    mutate(colour = case_when(
      group == "Food provision" ~ paired[1],
      group == "Nonfood provision" ~ paired[2],
      group == "Technosphere modification" ~ paired[3],
      group == "Maintenance of surroundings" ~ paired[4],
      group == "Somatic maintenance" ~ paired[5],
      group == "Deliberate neural restructuring" ~ paired[6],
      group == "Organization" ~ paired[7],
      group == "Experience oriented" ~ paired[8]
    ))
}

subgroups_df <- function(df) {

  ## create subgroup variable df
  ## n is the number of subgroups for each group
  subgroups <- df %>%
    filter(level == 2) %>%
    group_by(parent) %>%
    mutate(n = length(unique(group)))

  ## Create a vector containing n
  n_colour_vec <- subgroups %>%
    group_by(parent) %>%
    distinct(n) %>%
    pull(n)

  ## create the alpha for each group
  alpha <- sapply(n_colour_vec, function(x) seq(0.1, 0.5, length.out = x))

  alpha_u <- unlist(alpha)
  group_colours <- subgroups %>%
    distinct(group) %>%
    bind_cols(alpha = alpha_u) %>%
    left_join(subgroups, .)
}
