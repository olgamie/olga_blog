## Visualizing the length and paid rate of a paid leave available to mothers in OECD countries (2016)

## @knitr plot_leave_vs_paid_rate
library(readxl)
library(plotly)
library(dplyr)

## Read data #
OECD_parental_leave <- read_excel("parental-leave/oecd_parental_leave.xlsx")

parental_leave <- OECD_parental_leave %>% dplyr::filter(
  !Country %in% c("OECD average", "EU average", "Eurozone ave.")) %>%
  mutate(avg_leave = mean(`Total paid leave in weeks`),
         avg_payment = mean(`Total paid leave avg payment rate (%)`))

plot_annotations <- parental_leave %>%
  dplyr::filter(!Country %in% c("New Zealand", "Italy", "Denmark", "Spain",
                                "Japan", "Mexico", "Costa Rica", "Israel"))

## Constants #
max_payment_rate <- 100
min_payment_rate <- 0
x_max_segment_border <- max(parental_leave$`Total paid leave in weeks`) * 1.1
three_mths_in_weeks <- 13
grey_col <- "#757575"

## Plot helper functions #
draw_vline <- function(x = 0, color, dash) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width = 0.5, dash = dash)
  )
}

draw_hline <- function(y = 0, color) {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, width = 0.5, dash = "dot")
  )
}

draw_segment <- function(x0, x1, y0, y1, color){
  list(type = "rect", layer = "below",
       fillcolor = color, line = list(color = color),
       opacity = 1,
       x0 = x0, x1 = x1, xref = "x",
       y0 = y0, y1 = y1, yref = "y")
}

add_xtick_labels_manually <- function(plot, font_face) {
  x_axis_labels <- c("0-3 mths", "3-6 mths", "6-12 mths", "1-2 years", "2-3 years")
  x_axis_y_pos <- rep(-7, 5)
  x_axis_x_pos <- c(7.5, 19.5, 39, 78, 130)
  plot %>%
    add_annotations(text = x_axis_labels,
                    y = x_axis_y_pos,
                    x = x_axis_x_pos,
                    textangle = 45,
                    showarrow = FALSE,
                    font = font_face)
}

add_division_labels <- function(plot, avg_payment, avg_leave, font_face) {
  plot %>% 
    add_annotations(text = c("OECD average<br>leave duration",
                             "OECD average<br>payment rate",
                             "<b>More money & time<br>than OECD avg</b>",
                             "<b>More money, less time<br>than OECD avg</b>",
                             "<b>Less money, more time<br>than OECD avg</b>",
                             "<b>Less money & time<br>than OECD avg</b>"),
                    y = c(10, avg_payment, 80, 80, 20, 20),
                    x = c(avg_leave, 150, 175, 6, 175, 6),
                    textangle = c(0, 0, 90, -90, 90, -90),
                    showarrow = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
                    font = font_face)
}

add_markers_labels <- function(plot, plot_annotations) {
  plot %>% 
    add_annotations(text = plot_annotations$Country,
                    x = plot_annotations$`Total paid leave in weeks`,
                    y = plot_annotations$`Total paid leave avg payment rate (%)` + 3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE,
                    font = list(family = "Roboto Condensed", size = 10))
}

## Plot variables #
font_face <- list(family = "Roboto Condensed")
avg_leave <- parental_leave$avg_leave[1]
avg_payment <- parental_leave$avg_payment[1]
y_axis_layout <- list(zeroline = TRUE, showgrid = FALSE, automargin = TRUE,
                      ticksuffix = "%", title = "Payment rate",
                      tickfont = font_face,
                      titlefont = font_face)
x_axis_layout <- list(zeroline = TRUE, showgrid = FALSE,
                      automargin = TRUE, showticklabels = FALSE,
                      title = "Total paid leave",
                      titlefont = font_face)


## Plotly scatter plot payment rate vs total paid parental leave OECD countries #
parental_leave %>%
  plot_ly(x = ~`Total paid leave in weeks`,
          y = ~`Total paid leave avg payment rate (%)`,
          hoverinfo = 'text',
          marker = list(size = 10, color = '#FF4EE8',
                        line = list(color = '#CC3FB9', width = 2)),
          hoverlabel = list(font = font_face),
          text = ~glue::glue('Mothers in {country} get in total
                             {n_weeks} weeks of paid leave
                             compensated {perc_rate}% of their salary.',
                     country = Country,
                     n_weeks = `Total paid leave in weeks`,
                     perc_rate = `Total paid leave avg payment rate (%)`)) %>%
 add_markers_labels(plot_annotations) %>% 
 add_division_labels(avg_payment, avg_leave, font_face) %>% 
 add_xtick_labels_manually(font_face)%>% 
  layout(
    title = list(
      text = "Total paid leave and rates available to mothers across OECD countries, 2016",
      font = font_face),
    shapes = list(
      draw_segment(min_payment_rate, avg_leave,
                   min_payment_rate, avg_payment,
                   "#FFF380"),
      draw_segment(avg_leave, x_max_segment_border,
                   avg_payment, max_payment_rate,
                   "#14B3EB"),
      draw_segment(min_payment_rate, avg_leave, 
                   avg_payment, max_payment_rate,
                   "#FFE804"),
      draw_segment(avg_leave, x_max_segment_border,
                   min_payment_rate, avg_payment,
                   "#8DD7F4"),
      draw_vline(three_mths_in_weeks, grey_col, ""),
      draw_vline(three_mths_in_weeks * 2, grey_col, ""),
      draw_vline(three_mths_in_weeks * 4, grey_col, ""),
      draw_vline(three_mths_in_weeks * 8, grey_col, ""),
      draw_vline(three_mths_in_weeks * 12, grey_col, ""),
      draw_vline(avg_leave, "black", "dot"),
      draw_hline(avg_payment, "#black")),
    yaxis = y_axis_layout,
    xaxis = x_axis_layout)