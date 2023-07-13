library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(forcats)
library(scales)
library(stringr)
library(ggpubr)
library(extrafont)
library(data.table)
library(lubridate)
library(patchwork)

####CHART SETTINGS####
config <- list(
  quantity_of_candles = 30,
  lower_quantile_setting = 0,
  higher_quantile_setting = 0.95,
  boxplot_width = 0.8,
  success_color = "#50C878",
  success_color_alpha = 0.7,
  fail_color = "#E04034",
  fail_color_alpha = 0.6,
  line_thickness = 0.4,
  line_color = "#808080",
  geom_point_alpha = 0.2,
  geom_point_size = 1.6,
  geom_point_stroke_size = NA,
  maximum_size_of_y_axis_mult = 1.5,
  main_font_family = "Courier New",
  title_group_name = "group_name",
  legend_line_color = "#636363",
  legend_line_size = 0.3,
  legend_line_height = 0.9,
  legend_text_color = "#636363",
  legend_font_family = "Courier New",
  legend_font_size = 2.7,
  legend_header_font_size = 4.0,
  legend_font_width = 2
)

generate_sample_data <- function(n, start_time, end_time, labels, elapsed_interval, 
                                 success_prob, outlier_prob = 0.05, outlier_range = c(15000, 30000)) {
  timestamps <- as.POSIXct(
    runif(n, as.numeric(as.POSIXct(start_time)), as.numeric(as.POSIXct(end_time))), 
    origin = "1970-01-01"
  )
  
  n_outliers <- floor(n * outlier_prob)
  n_normal <- n - n_outliers
  
  normal_responses <- sample(seq(elapsed_interval[1], elapsed_interval[2]), n_normal, replace = TRUE)
  outlier_responses <- sample(seq(outlier_range[1], outlier_range[2]), n_outliers, replace = TRUE)
  
  all_responses <- sample(c(normal_responses, outlier_responses))
  
  data.frame(
    timeStamp = timestamps,
    group = sample(labels, n, replace = TRUE),
    response = all_responses,
    success = as.logical(rbinom(n, 1, success_prob))
  )
}

read_data_from_csv <- function(filepath, timestamp_format = "%Y/%m/%d %H:%M:%OS") {
  data <- fread(
    filepath,
    header = TRUE,
    select = c("timeStamp", "label", "elapsed", "success")
  )
  
  data$timeStamp <- as.POSIXct(data$timeStamp, format = timestamp_format)
  setnames(data, c("timeStamp", "group", "response", "success"))
  
  return(data)
}

read_data_from_csv_alt <- function(filepath) {
  fread(
    filepath,
    header = TRUE,
    select = c("timeStamp", "label", "elapsed", "success"),
    col.names = c("timeStamp", "group", "response", "success"),
    colClasses = c(
      timeStamp = "POSIXct",
      label = "character",
      elapsed = "numeric", 
      success = "logical"
    ),
    sep = ",",
    dec = ".",
    encoding = "UTF-8"
  )
}

create_legend_chart <- function(config) {
  set.seed(1)
  y <- rnorm(100)
  
  legend_df <- data.frame(
    x = 1,
    y0 = min(y),
    lower_quantile = quantile(y, 0.25),
    med = quantile(y, 0.42),
    upper_quantile = quantile(y, 0.75),
    y100 = max(y)
  )
  
  random_points <- data.frame(
    y = runif(12, min = legend_df$upper_quantile, max = legend_df$upper_quantile * 2),
    success = sample(c(TRUE, FALSE), 12, replace = TRUE)
  )
  
  ggplot(legend_df, aes(x)) +
    geom_rect(aes(
      fill = "success_true", 
      xmin = x - config$boxplot_width / 2, 
      xmax = x + config$boxplot_width / 2,
      ymin = lower_quantile, 
      ymax = upper_quantile * 0.5
    ), alpha = config$success_color_alpha) +
    
    geom_rect(aes(
      fill = "success_false", 
      xmin = x - config$boxplot_width / 2, 
      xmax = x + config$boxplot_width / 2,
      ymin = upper_quantile * 0.5, 
      ymax = upper_quantile
    ), alpha = config$fail_color_alpha) +
    
    geom_boxplot(aes(
      ymin = lower_quantile, 
      lower = lower_quantile, 
      middle = med, 
      upper = upper_quantile, 
      ymax = upper_quantile
    ), stat = "identity", width = config$boxplot_width, 
    linewidth = config$line_thickness, fill = alpha("white", 0), colour = config$line_color) +
    
    geom_point(data = random_points, aes(y = y, x = 1, color = success), 
               alpha = config$geom_point_alpha, size = config$geom_point_size, stroke = NA) +
    
    scale_color_manual(values = c("TRUE" = config$success_color, "FALSE" = config$fail_color)) +
    scale_fill_manual(values = c("success_true" = config$success_color, "success_false" = config$fail_color)) +
    
    geom_rect(aes(
      fill = "black", 
      xmin = 1, xmax = 6,
      ymin = lower_quantile, 
      ymax = max(random_points$y) * 1.1
    ), alpha = 0.0) +
    
    annotate("text", x = 3.9, y = max(random_points$y) * 1.1,
             color = config$legend_text_color, label = "Legend:", 
             lineheight = config$legend_line_height, size = config$legend_header_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("segment", x = 1.8, xend = 1.8, 
             y = min(random_points$y), yend = max(random_points$y),
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("segment", x = 1.2, xend = 1.8, 
             y = max(random_points$y), yend = max(random_points$y),
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("segment", x = 1.2, xend = 1.8, 
             y = min(random_points$y), yend = min(random_points$y),
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("text", x = 3.8, y = (legend_df$upper_quantile * 2 * 0.85 + 0.09) * 0.85,
             color = config$success_color, label = "•Passed", 
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("text", x = 3.8, y = legend_df$upper_quantile * 2 * 0.85 * 0.85,
             color = config$fail_color, label = "•Failed", 
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("text", x = 3.9, y = legend_df$upper_quantile * 2 * 0.85,
             color = config$legend_text_color, 
             label = str_wrap(paste0("Outliers higher than ", config$higher_quantile_setting * 100, "p value"), width = 13), 
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width, vjust = 0) +
    
    annotate("segment", x = 1.2, xend = 2.0, 
             y = legend_df$upper_quantile, yend = legend_df$upper_quantile,
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("text", x = 3.9, y = legend_df$upper_quantile,
             color = config$legend_text_color,
             label = str_wrap(paste0(config$higher_quantile_setting * 100, "p value"), width = 10),  
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("segment", x = 1.2, xend = 2.0, 
             y = legend_df$upper_quantile * 0.75, yend = legend_df$upper_quantile * 0.75,
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("text", x = 3.9, y = legend_df$upper_quantile * 0.75,
             color = config$fail_color, 
             label = str_wrap("Failed requests ratio", width = 10), 
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("segment", x = 1.2, xend = 2.0, 
             y = legend_df$upper_quantile * 0.3, yend = legend_df$upper_quantile * 0.3,
             color = config$legend_line_color, linewidth = config$legend_line_size) + 
    
    annotate("text", x = 4, y = legend_df$upper_quantile * 0.3,
             color = config$success_color, 
             label = str_wrap("Passed requests ratio", width = 10), 
             lineheight = config$legend_line_height, size = config$legend_font_size, 
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("segment", x = 1.2, xend = 2.0, 
             y = legend_df$med, yend = legend_df$med,
             color = config$legend_line_color, linewidth = config$legend_line_size) + 
    
    annotate("text", x = 4, y = legend_df$med,
             color = config$legend_text_color, 
             label = str_wrap("Median value", width = 10), 
             size = config$legend_font_size, lineheight = config$legend_line_height,
             family = config$legend_font_family, fontface = config$legend_font_width) +
    
    annotate("segment", x = 1.2, xend = 2.0, 
             y = legend_df$lower_quantile, yend = legend_df$lower_quantile,
             color = config$legend_line_color, linewidth = config$legend_line_size) +
    
    annotate("text", x = 4, y = legend_df$lower_quantile,
             color = config$legend_text_color, 
             label = str_wrap(paste0(config$lower_quantile_setting * 100, "p value"), width = 10), 
             size = config$legend_font_size, family = config$legend_font_family, 
             fontface = config$legend_font_width) +
    
    theme(
      aspect.ratio = 3.5 / 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = "grey", fill = NA, linewidth = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 8.5, face = "bold"),
      plot.margin = margin(0, 0, 0, 20, "pt")
    )
}


generate_candles_chart <- function(data_for_candles, selected_group, config, legend_chart) {
  theme_set(theme_light())
  
  data_filtered <- filter(data_for_candles, group == selected_group)
  
  unique_dates <- unique(floor_date(data_filtered$timeStamp, unit = "days"))
  formatted_date <- if (length(unique_dates) == 1) {
    format(unique_dates, "%d %b %Y")
  } else {
    paste0(format(min(unique_dates), "%d"), "-", format(max(unique_dates), "%d"), " ", 
           format(max(unique_dates), "%b %Y"))
  }
  
  breaks <- seq(min(data_filtered$timeStamp), max(data_filtered$timeStamp) + 1, 
                length.out = config$quantity_of_candles + 1)
  data_filtered$interval <- cut(data_filtered$timeStamp, breaks = breaks)
  
  boxplot_data <- data_filtered %>%
    group_by(group, interval) %>%
    reframe(
      min_val = min(response),
      max_val = max(response),
      lower_quantile = quantile(response, config$lower_quantile_setting),
      median_val = median(response),
      upper_quantile = quantile(response, config$higher_quantile_setting),
      success_true_ratio = sum(success == TRUE) / n(),
      success_false_ratio = sum(success == FALSE) / n()
    )
  
  # Calculate percentiles and filter outliers (combined operation)
  percentile <- data_filtered %>% 
    group_by(group, interval) %>%
    summarize(percentile = quantile(response, config$higher_quantile_setting, na.rm = TRUE), 
              .groups = "drop")
  
  values_above_percentile <- data_filtered %>%
    inner_join(percentile, by = c("group", "interval")) %>%
    filter(response > percentile)
  
  # Calculate y-axis limits
  y_axis_high <- config$maximum_size_of_y_axis_mult * max(boxplot_data$upper_quantile, na.rm = TRUE)
  y_axis_low <- 0
  
  # Separate outliers into two groups: visible and extreme
  filtered_outliers <- filter(values_above_percentile, response > y_axis_high)
  visible_outliers <- filter(values_above_percentile, response <= y_axis_high)
  
  # Calculate outlier plot limits (consolidated logic)
  has_outliers <- nrow(filtered_outliers) > 0
  outlier_y_limits <- if (has_outliers) {
    c(min(filtered_outliers$response), max(filtered_outliers$response))
  } else {
    c(0, 0)
  }
  
  main_plot <- ggplot(boxplot_data, aes(x = factor(interval))) +
    geom_rect(aes(
      fill = "success", 
      xmin = as.numeric(interval) + 0.5 - config$boxplot_width / 2, 
      xmax = as.numeric(interval) + 0.5 + config$boxplot_width / 2,
      ymin = lower_quantile, 
      ymax = lower_quantile + ((upper_quantile - lower_quantile) * success_true_ratio)
    ), alpha = config$success_color_alpha) +
    
    geom_rect(aes(
      fill = "fail", 
      xmin = as.numeric(interval) + 0.5 - config$boxplot_width / 2, 
      xmax = as.numeric(interval) + 0.5 + config$boxplot_width / 2,
      ymin = lower_quantile + ((upper_quantile - lower_quantile) * success_true_ratio), 
      ymax = upper_quantile
    ), alpha = config$fail_color_alpha) +
    
    geom_boxplot(aes(
      ymin = lower_quantile, 
      lower = lower_quantile, 
      middle = median_val, 
      upper = upper_quantile, 
      ymax = upper_quantile
    ), stat = "identity", position = position_nudge(x = .5),
    width = config$boxplot_width, fill = alpha("white", 0), 
    size = config$line_thickness, colour = config$line_color) +
    
    geom_point(data = visible_outliers, 
               aes(y = response, color = success), 
               alpha = config$geom_point_alpha, 
               size = config$geom_point_size, 
               stroke = config$geom_point_stroke_size, 
               position = position_nudge(x = .5)) +
    
    scale_color_manual(values = c("TRUE" = config$success_color, "FALSE" = config$fail_color)) +
    scale_fill_manual(values = c("success" = config$success_color, "fail" = config$fail_color)) +
    
    labs(x = "Time Interval, HH:MM:SS", 
         y = "Response Time, ms", 
         title = paste0("Response Time Candles over time (", formatted_date, ")")) +
    guides(fill = "none") +
    scale_x_discrete(labels = function(x) format(strptime(x, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) +
    scale_y_continuous(labels = label_comma(big.mark = ","), 
                       limits = c(y_axis_low, y_axis_high)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      text = element_text(family = config$main_font_family),
      axis.title.y = element_text(margin = margin(r = 12, unit = "pt")),
      axis.title.x = element_text(margin = margin(t = 12, unit = "pt")),
      plot.title.position = 'plot',
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    ) +
    facet_wrap(~ group, scales = "free", ncol = 1)
  
  outlier_plot <- ggplot(boxplot_data, aes(x = factor(interval))) +
    geom_boxplot(aes(
      ymin = -lower_quantile, 
      lower = -lower_quantile, 
      middle = -median_val, 
      upper = -upper_quantile, 
      ymax = -upper_quantile
    ), stat = "identity", position = position_nudge(x = .5),
    width = config$boxplot_width, fill = alpha("white", 0), 
    size = config$line_thickness, colour = config$line_color) +
    
    {if (has_outliers) {
      geom_point(data = filtered_outliers, 
                 aes(y = response, color = success), 
                 alpha = config$geom_point_alpha, 
                 size = config$geom_point_size, 
                 stroke = config$geom_point_stroke_size, 
                 position = position_nudge(x = .5))
    }} +
    
    scale_color_manual(values = c("TRUE" = config$success_color, "FALSE" = config$fail_color)) +
    guides(fill = "none") +
    scale_x_discrete(labels = function(x) format(strptime(x, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) +
    scale_y_continuous(
      labels = function(x) ifelse(x == 0, "", label_comma(big.mark = ",")(x)),
      breaks = outlier_y_limits,
      limits = outlier_y_limits
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = config$main_font_family),
      plot.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  combined_plot <- (outlier_plot / main_plot) + 
    plot_layout(heights = c(0.15, 0.85), axes = "collect", axis_titles = "collect")
  
  final_plot <- combined_plot | legend_chart 
  
  return(final_plot)
}

generate_charts_for_all_groups <- function(data, config, legend_chart) {
  unique_groups <- unique(data$group)
  
  charts <- lapply(unique_groups, function(grp) {
    generate_candles_chart(data, grp, config, legend_chart)
  })
  
  names(charts) <- unique_groups
  return(charts)
}


### EXECUTION ###
data <- generate_sample_data(
  n = 10000,
  start_time = "2024-01-01 00:00:00",
  end_time = "2024-01-01 01:00:00",
  labels = c("Type1", "Type2", "Type3"),
  elapsed_interval = c(100, 1000),
  success_prob = 0.8,
  outlier_prob = 0.05,
  outlier_range = c(15000, 30000)
)

legend_chart <- create_legend_chart(config)
charts <- generate_charts_for_all_groups(data, config, legend_chart)
print(charts)