## The trendies baby names in Poland 2000 - 2019 analysis

## @knitr read_libraries_data
## Read in libraries and data

library(readr)
library(dplyr)
library(gganimate)
library(hrbrthemes)
library(Hmisc)
library(forecast)
library(ggrepel)
library(DT)

baby_names <- read_csv("babynames/babynames.csv") %>%
  mutate(Name = capitalize(tolower(Name))) %>%
  group_by(Sex, Year) %>%
  mutate(ratio = Quantity / sum(Quantity),
         rank = dense_rank(desc(Quantity))) %>%
  ungroup()

## @knitr constants
## Define constants
plot_colors <- c("#b69cfd", "#e6faa2", "#bda5ab", "#c4f418", "#fb8c77", 
                 "#a0b460", "#f67afe", "#fe8f06", "#f6eefa", "#f4d403")
plot_font <- "Roboto Condensed"
grey_col <- "#929299"


## @knitr helper_functions
## Helper functions #
## data filtering, creating plots and DT
filter_sex <- function(baby_names_data, sex_label) {
  baby_names_data %>% 
    dplyr::filter(Sex == sex_label)
}

filter_year <- function(baby_names_data, year) {
  baby_names_data %>% 
    dplyr::filter(Year == year)
}

select_top_n_names <- function(baby_names_data, n) {
  baby_names_data %>% 
    top_n(n, Quantity)
}

filter_for_given_names <- function(baby_names_data, names_to_filter) {
  baby_names_data %>%
    dplyr::filter(Name %in% names_to_filter)
}

## DT
create_dt_top_names <- function(data) {
  data %>% 
    DT::datatable(rownames = FALSE, 
                  colnames = c('Rank' = 'rank'),
                  options = list(
                    #autoWidth = TRUE,
                    columnDefs = list(
                      list(className = 'dt-left', targets = "_all")),
                    initComplete = JS(
                    "function(settings, json) {",
                    "$('body').css({'font-family': 'Roboto Condensed'});",
                    "}"
                  )))
}

## Plotting data

plot_names_trends <- function(baby_names_data, title, plot_colors, grey_col, plot_font) {
  ggplot(baby_names_data, aes(Year, Quantity,
                              group = Name, color = Name)) +
    geom_line(linetype = 2) +
    scale_color_manual(values = plot_colors) +
    scale_x_continuous(limits = c(2000, 2019), breaks = seq(2000, 2019, 2)) +
    geom_point(size = 2) + 
    geom_text_repel(aes(label = Name), hjust = 0, direction = "y", family = plot_font) + 
    coord_cartesian(clip = 'off') + 
    labs(x = 'Year', y = 'Babies named', title = title,
         subtitle = "Trend change 2000-2018") + 
    theme_ft_rc(axis_title_size = 10) + 
    theme(plot.margin = margin(5.5, 40, 5.5, 5.5), legend.position = "none") +
    transition_reveal(Year)
}

add_names_floating_labels <- function(x_pos = 2, hjust_value = -0.1,
                                      label_position, label_name, plot_font, nrows_x) {
  geom_text_repel(aes(label = label_name, y = label_position, x = rep(x_pos, nrows_x)),
                  size = 3.5, direction = "y", hjust = hjust_value, color = grey_col, 
                  family = plot_font)
}

add_year_titles <- function(year_label, hjust_val, x_pos, plot_font, max_y){
  geom_text(label = year_label, x = x_pos, y = max_y + 400, hjust = hjust_val,
            size = 4, family = plot_font)
}

draw_segment_border <- function(x_val, color_name) {
  geom_vline(xintercept = x_val, linetype = "dashed", size = 0.1, color = color_name)
}

plot_most_recent_change_top_names <- function(baby_names_data, grey_col,
                                              plot_colors, plot_font, title, subtitle) {
  
  min_y <- min(baby_names_data$Quantity_2019, baby_names_data$Quantity_2018)
  max_y <- max(baby_names_data$Quantity_2019, baby_names_data$Quantity_2018)
  nrows_x <- nrow(baby_names_data)
  
  ggplot(baby_names_data) +
    geom_segment(aes(x = 1, xend = 2, y = Quantity_2018,
                     yend = Quantity_2019, col = class), size = 0.75, show.legend = F) +
    draw_segment_border(1, grey_col) +
    draw_segment_border(2, grey_col) +
    scale_color_manual(values = c("up" = plot_colors[4], "const" = grey_col,
                                  "down" = plot_colors[8])) +
    xlim(0.5, 2.5) + 
    ylim(min_y - 10, max_y + 300) +
    add_names_floating_labels(1, 1.1, baby_names_data$Quantity_2018,
                              baby_names_data$left_label, plot_font, nrows_x) + 
    add_names_floating_labels(2, -0.1, baby_names_data$Quantity_2019,
                              baby_names_data$right_label, plot_font, nrows_x) +
    add_year_titles("2018", -0.1, 1, plot_font, max_y) +
    add_year_titles("2019", 1.1, 2, plot_font, max_y) +
    labs(x = "", y = "Babies named", title = title, subtitle = subtitle)
}

adjust_theme <- function(plot) {
  plot +
    theme_ft_rc() + 
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank())
}

## @knitr filter_data
## Filter data for analysis and plots
girls_names <- baby_names %>% 
  filter_sex("K")

girls_2018 <- girls_names %>% 
  filter_year(2018)

girls_2018_top10 <- girls_2018 %>% 
  select_top_n_names(10)

girls_most_popular_names <- girls_names %>%
  filter_for_given_names(girls_2018_top10$Name)

boys_names <- baby_names %>% 
  filter_sex("M")

boys_2018 <- boys_names %>% 
  filter_year(2018)

boys_2018_top10 <- boys_2018 %>% 
  select_top_n_names(10)

boys_most_popular_names <- boys_names %>%
  filter_for_given_names(boys_2018_top10$Name)

## @knitr animate_girls
## GGanimate the trendiest baby girl names
animation_girls_names_trends <- girls_most_popular_names %>%
  plot_names_trends("Most popular girl names in 2018", plot_colors, grey_col, plot_font)

gganimate::animate(animation_girls_names_trends, fps = 2)

## @knitr animate_boys
## GGanimate the trendiest baby boy names
animation_boys_names_trends <- boys_most_popular_names %>%
  plot_names_trends("Most popular boy names in 2018", plot_colors, grey_col, plot_font)
gganimate::animate(animation_boys_names_trends, fps = 3)

## @knitr dt_girls
## DT top girl names 2000-2018
girls_names %>%
  group_by(Year) %>%
  select_top_n_names(10) %>%
  select(rank, Year, Name) %>%
  create_dt_top_names()

## @knitr dt_boys
## DT top boy names 2000-2018
boys_names %>% 
  group_by(Year) %>%
  select_top_n_names(10) %>%
  select(rank, Year, Name) %>%
  create_dt_top_names()

## @knitr forecasting
## Forecast 1 year ahead using auto.arima
forecast_baby_names <- function(baby_names_data, last_year_data) {
  baby_names_data %>% 
    filter(Name %in% top_n(last_year_data, 20, Quantity)$Name) %>%  
    group_by(Name, Sex) %>%
    arrange(Name, Year) %>% 
    do(data.frame(
      Quantity_2019 = round(forecast(auto.arima(.$Quantity), h = 1)$mean, 0))
    ) %>%
    ungroup() %>% 
    mutate(Year = 2019,
           rank_2019 = dense_rank(desc(Quantity_2019)))
}

## Prepare forecasted data for plotting
prepare_data_2_years_change_plot <- function(forecast_data, history_data) {
  top_10_names_2019 <- forecast_data %>%
    dplyr::top_n(10, Quantity_2019)
  
  top_10_names_2019 %>% 
    left_join(history_data %>%
                filter_for_given_names(top_10_names_2019$Name) %>% 
                select(Name, Quantity_2018 = Quantity, rank_2018 = rank),
              by = "Name") %>% 
    mutate(left_label = paste0(Name, " ranked #", rank_2018),
           right_label = paste0(Name, " ranked #", rank_2019),
           class = case_when(Quantity_2019 > Quantity_2018 ~ "up",
                             Quantity_2018 == Quantity_2019 ~ "const",
                             TRUE ~ "down"))
}

## @knitr forecast_girls
## Make forecast for baby girl names
forecast_2019_girls <- girls_names %>% 
  forecast_baby_names(girls_2018)

girls_2018_2019 <- forecast_2019_girls %>%
  prepare_data_2_years_change_plot(girls_2018)

## @knitr forecast_boys
## and boys
forecast_2019_boys <- boys_names %>% 
  forecast_baby_names(boys_2018)

boys_2018_2019 <- forecast_2019_boys %>%
  prepare_data_2_years_change_plot(boys_2018)

## @knitr plot_forecast_girls
## Plot 2018 and 2019 top names girl
girls_2018_2019 %>% 
  plot_most_recent_change_top_names(grey_col, plot_colors, plot_font,
                                    "Top 10 girl names in 2019",
                                    "No. babies named and their position in ranking 2018 vs. 2019") %>%
  adjust_theme()

## @knitr plot_forecast_boys
## Plot 2018 and 2019 top names boy
boys_2018_2019 %>% 
  plot_most_recent_change_top_names(grey_col, plot_colors, plot_font,
                                    "Top 10 boy names in 2019",
                                    "No. babies named and their position in ranking 2018 vs. 2019") %>%
  adjust_theme()
