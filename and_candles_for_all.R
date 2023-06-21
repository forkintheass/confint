# Load required libraries
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

# Read the input CSV file
data <- fread("~/Documents/r/sample_results_long.csv", 
              header=TRUE, 
              select=c("timeStamp", "label", "elapsed", "success"))

data$timeStamp <- as.POSIXct(data$timeStamp, format = "%Y/%m/%d %H:%M:%OS")
colnames(data) <- c("timeStamp", "group","response", "success")

#data <- fread("C:\\sample.csv",
#              header=TRUE, 
#              select=c("timeStamp", "label", "elapsed", "success"),
#              col.names = c("timeStamp", "label", "elapsed", "success"),
#              colClasses=c(timeStamp = "POSIXct",label = "character",elapsed = "numeric", success = "logical"),
#              sep = ",",
#              dec = ".",
#              encoding = "UTF-8")

theme_set (theme_light())

#####GENERATE CANDLES CHART FUNCTION#####
generate_candles_chart <- function (data_for_candles, values_more_than_percentile) {
  
  #calculate y-axis limits
  y_axis_high_limit = maximum_size_of_y_axis_mult*max(data_for_candles$upper_quantile, na.rm = TRUE)
  y_axis_low_limit = 0
  
  #filter and settings for the data that didn't fit on maid
  filtered_values_that_do_not_fit <- filter(values_more_than_percentile, response > y_axis_high_limit)
  y_axis_low_limit_add_chart = round(min(filtered_values_that_do_not_fit$response))
  
  #filling absent intervals with some empty data
  #otherwise might be prblem with gridline sync and intervals will be skipped also
  absent_intervals <- setdiff(unique(data_for_candles$interval), filtered_values_that_do_not_fit$interval)
  na_values <- data.frame(interval = absent_intervals, response = -1)
  filtered_values_that_do_not_fit <- bind_rows(filtered_values_that_do_not_fit, na_values)
  
  candles_chart <- ggplot(data_for_candles, aes(x = factor(interval))) +
    
    geom_rect(aes(fill = "success", 
                  xmin = as.numeric(interval) + 0.5 - boxplot_width / 2, 
                  xmax = as.numeric(interval) + 0.5 + boxplot_width / 2,
                  ymin = lower_quantile, 
                  ymax = lower_quantile+((upper_quantile-lower_quantile) * success_true_ratio)
    ), 
    alpha = success_color_alpha) +
    
    geom_rect(aes(fill = "fail", 
                  xmin = as.numeric(interval) + 0.5 - boxplot_width / 2, 
                  xmax = as.numeric(interval) + 0.5 + boxplot_width / 2,
                  ymin = lower_quantile+((upper_quantile-lower_quantile) * success_true_ratio), 
                  ymax = upper_quantile), 
              alpha = fail_color_alpha) +
    
    geom_boxplot(aes(ymin = lower_quantile, 
                     lower = lower_quantile, 
                     middle = median_val, 
                     upper = upper_quantile, 
                     ymax = upper_quantile),
                 stat = "identity", 
                 position = position_nudge(x=.5),
                 width = boxplot_width, 
                 fill = alpha("white",0), 
                 size = line_thickness, 
                 colour=line_color) +
    
    geom_point(data=values_more_than_percentile, 
               aes(y = response, color = success), 
               alpha=geom_point_alpha, 
               size = geom_point_size, 
               stroke = geom_point_stroke_size,
               position = position_nudge(x=.5)) +
    
    scale_color_manual(values = c("TRUE" = success_color, "FALSE" = fail_color)) +
    
    #geom_jitter() - use for different representation
    
    scale_fill_manual(values = c("success" = success_color, "fail" = fail_color)) +
    
    labs(x = "Time Interval, HH:MM:SS", y = "Response Time, ms", title = paste("Response Time Candles over time")) +
    guides(fill = "none") +
    scale_x_discrete(labels = function(x) format(strptime(x, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) +
    scale_y_continuous(labels = label_comma(big.mark = ","), 
                       limits=c(y_axis_low_limit,y_axis_high_limit)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          text = element_text(family = main_font_family),
          axis.title.y = element_text(margin = margin(r = 12, unit = "pt")),
          axis.title.x = element_text(margin = margin(t = 12, unit = "pt")),
          plot.title.position = 'plot',
          plot.title = element_text(hjust = 0.5, face="bold"),
          legend.position = "none") +
    facet_wrap( ~ group, scales = "free", ncol = 1) #use this option to display all graphs on one page
  
  #build an additional chart to show outliers that didn't fit
  additional_scale_chart <- ggplot(filtered_values_that_do_not_fit, aes(x = factor(interval))) +
    
    #boxplot is added in order to avoid sync issues of gridlines
    geom_boxplot(data=filtered_boxplot_data,
                 aes(ymin = -lower_quantile, 
                     lower = -lower_quantile, 
                     middle = -median_val, 
                     upper = -upper_quantile, 
                     ymax = -upper_quantile),
                 stat = "identity", 
                 position = position_nudge(x=.5),
                 width = boxplot_width, 
                 fill = alpha("white",0), 
                 size = line_thickness, 
                 colour=line_color) +
    
    geom_point(aes(y = response, color = success), 
               alpha=geom_point_alpha, 
               size = geom_point_size, 
               stroke = geom_point_stroke_size,
               position = position_nudge(x=.5)) +
    
    scale_color_manual(values = c("TRUE" = success_color, "FALSE" = fail_color)) +
    
    #geom_jitter() - use for different representation
    
    guides(fill = "none") +
    scale_x_discrete(labels = function(x) format(strptime(x, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) +
    scale_y_continuous(labels = label_comma(big.mark = ","),
                       breaks = c(y_axis_low_limit_add_chart, max(filtered_values_that_do_not_fit$response)),
                       limits = c(y_axis_low_limit_add_chart, max(filtered_values_that_do_not_fit$response))) +
    
    theme_minimal() +
    theme(text = element_text(family = main_font_family),
          plot.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
    )
  
  #worked under windows, didn't work under macos, commented.
  #combined_plot <- graph5 + candles_chart + plot_layout(ncol = 1, heights = c(0.15, 0.85))
  
  combined_plot <- ggarrange(additional_scale_chart, candles_chart, ncol = 1, heights = c(0.15, 0.85), align="v")
  
  arranged_graphs2 <- ggarrange(combined_plot, legend_chart, nrow = 1, ncol = 2, widths = c(0.85, 0.15))
  
  return(arranged_graphs2)
}

#####END OF GENERATE CHART FUNCTION#####

######CHART SETTINGS#####
quantity_of_candles = 30
lower_quantile_setting = 0
higher_quantile_setting = 0.95
boxplot_width = 0.8
success_color = "#50C878"
success_color_alpha = 0.7
fail_color = "#E04034"
fail_color_alpha = 0.6
line_thickness = 0.4
line_color = "#808080"
geom_point_alpha = 0.2
geom_point_size = 1.6
geom_point_stroke_size = NA
maximum_size_of_y_axis_mult = 1.5
main_font_family = "Courier New"
title_group_name = "group_name"
legend_line_color = "#636363"
legend_line_size = 0.3
legend_line_height = 0.9
legend_text_color = "#636363"
legend_font_family = "Courier New"
legend_font_size = 2.7
legend_header_font_size = 4.0
legend_font_width = 2

######GENERATE LEGEND######
set.seed(1)
y <- rnorm(100)
legend_data_frame_generated <- data.frame(
  x = 1,
  y0 = min(y),
  lower_quantile = quantile(y, 0.25),
  med = quantile(y, 0.42),
  upper_quantile = quantile(y, 0.75),
  y100 = max(y)
)

random_points_for_legend <- data.frame(y = runif(12, min = legend_data_frame_generated$upper_quantile, 
                                                 max = legend_data_frame_generated$upper_quantile*2))
random_points_for_legend$success <- sample(c(TRUE, FALSE), nrow(random_points_for_legend), replace = TRUE)

legend_chart <- ggplot(legend_data_frame_generated, aes(x)) +
  
  geom_rect(aes(fill = "success_true", 
                xmin = x - boxplot_width/2, 
                xmax = x + boxplot_width/2,
                ymin = lower_quantile, 
                ymax = upper_quantile * 0.5), 
            alpha = success_color_alpha) +
  
  geom_rect(aes(fill = "success_false", 
                xmin = x - boxplot_width/2, 
                xmax = x + boxplot_width/2,
                ymin = upper_quantile * 0.5, 
                ymax = upper_quantile), 
            alpha = fail_color_alpha) +
  
  geom_boxplot(
    aes(ymin = lower_quantile, 
        lower = lower_quantile, 
        middle = med, 
        upper = upper_quantile, 
        ymax = upper_quantile),
    stat = "identity", 
    width = boxplot_width, 
    size = line_thickness, 
    fill = alpha("white",0), 
    colour=line_color
  ) +
  
  geom_point(data=random_points_for_legend, 
             aes(y = y, x = 1, color = success), 
             alpha=geom_point_alpha, 
             size = geom_point_size, 
             stroke = NA) +
  
  scale_color_manual(values = c("TRUE" = success_color, "FALSE" = fail_color)) +
  
  scale_fill_manual(values = c("success_true" = success_color, 
                               "success_false" = fail_color)) +
  
  geom_rect(aes(fill = "black", 
                xmin = 1, 
                xmax = 6,
                ymin = lower_quantile, 
                ymax = max(random_points_for_legend)*1.1
                ), 
            alpha = 0.0) +
  
  annotate("text", x = 3.9, 
           y = max(random_points_for_legend)*1.1,
           color = legend_text_color, 
           label = "Legend:", 
           lineheight = legend_line_height,
           size = legend_header_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("segment", x = 1.8, xend = 1.8, 
           y = min(random_points_for_legend$y),
           yend = max(random_points_for_legend$y),
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("segment", x = 1.2, xend = 1.8, 
           y = max(random_points_for_legend$y),
           yend = max(random_points_for_legend$y),
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("segment", x = 1.2, xend = 1.8, 
           y = min(random_points_for_legend$y),
           yend = min(random_points_for_legend$y),
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 3.8, 
           y = (legend_data_frame_generated$upper_quantile*2*0.85+0.09)*0.85,
           color = success_color, 
           label = "•Passed", 
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("text", x = 3.8, 
           y = legend_data_frame_generated$upper_quantile*2*0.85*0.85,
           color = fail_color, 
           label = "•Failed", 
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile*2*0.85,
           color = legend_text_color, 
           label = str_wrap(paste0("Outliers higher than ", higher_quantile_setting*100, "p value"), width=13), 
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width,
           vjust = 0) +
  
  annotate("segment", x = 1.2, xend = 2.0, 
           y = legend_data_frame_generated$upper_quantile, 
           yend = legend_data_frame_generated$upper_quantile,
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile,
           color = legend_text_color,
           label = stringr::str_wrap(paste0(higher_quantile_setting*100,"p value"), width=10),  
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("segment", x = 1.2, xend = 2.0, 
           y = legend_data_frame_generated$upper_quantile * 0.75, 
           yend = legend_data_frame_generated$upper_quantile * 0.75,
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile * 0.75,
           color = fail_color, 
           label = stringr::str_wrap("Failed requests ratio", width=10), 
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("segment", x = 1.2, xend = 2.0, 
           y = legend_data_frame_generated$upper_quantile*0.3, 
           yend = legend_data_frame_generated$upper_quantile * 0.3,
           color = legend_line_color,
           size = legend_line_size) + 
  
  annotate("text", x = 4, 
           y = legend_data_frame_generated$upper_quantile*0.3,
           color = success_color, 
           label = stringr::str_wrap("Passed requests ratio", width=10), 
           lineheight = legend_line_height,
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("segment", x = 1.2, xend = 2.0, 
           y = legend_data_frame_generated$med,
           yend = legend_data_frame_generated$med,
           color = legend_line_color,
           size = legend_line_size) + 
  
  annotate("text", x = 4, 
           y = legend_data_frame_generated$med,
           color = legend_text_color, 
           label = stringr::str_wrap("Median value", width=10), 
           size = legend_font_size, 
           lineheight = legend_line_height,
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  annotate("segment", x = 1.2, xend = 2.0, 
           y = legend_data_frame_generated$lower_quantile, 
           yend = legend_data_frame_generated$lower_quantile,
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 4, 
           y = legend_data_frame_generated$lower_quantile,
           color = legend_text_color, 
           label = stringr::str_wrap(paste0(lower_quantile_setting*100,"p value"), width=10), 
           size = legend_font_size, 
           family = legend_font_family, 
           fontface = legend_font_width) +
  
  theme(aspect.ratio=3.5/1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(color = "grey", fill=NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 9, face="bold"),
        plot.margin = margin(60,0,80,0, "pt"),
  )

####END OF LEGEND####

####MAIN CHART START####

breaks <- seq(min(data$timeStamp), max(data$timeStamp) +1, length.out = quantity_of_candles + 1)
data$interval <- cut(data$timeStamp, breaks = breaks)

#calculate the boxplots data
boxplot_data <- data %>%
  group_by(group, interval) %>%
  reframe(
    min_val = min(response),
    max_val = max(response),
    lower_quantile = quantile(response, lower_quantile_setting),
    median_val = median(response),
    upper_quantile = quantile(response, higher_quantile_setting),
    success_true_ratio = sum(success == TRUE) / n(),
    success_false_ratio = sum(success == FALSE) / n()
  )

#if the intervals are empty, fill them with empty values, so they are not skipped on chart
boxplot_data <- boxplot_data %>%
  complete(group, interval, fill = list(
    min_val = NA,
    max_val = NA,
    lower_quantile = NA,
    median_val = NA,
    upper_quantile = NA,
    success_true_ratio = NA,
    success_false_ratio = NA
  ))

#get the percentiles for all the intervals
percentile <- data %>% 
  group_by(group, interval) %>%
  summarize(percentile = quantile(response, higher_quantile_setting, na.rm = TRUE))

percentile <- as.data.frame(percentile) %>%
  complete(group, interval, fill = list(
    percentile = NA
  ))

# Filter data for response values higher than percentile within each interval
values_more_than_percentile <- data %>%
  inner_join(percentile, by = c("group", "interval")) %>%
  group_by(group) %>%
  filter(response > percentile)

#now divide by groups if needed
unique_groups <- unique(boxplot_data$group)

for (grp in unique_groups) {
  
  #filter data by groups
  filtered_boxplot_data <- filter(boxplot_data, group == grp)
  filtered_values_more_than_percentile <- filter(values_more_than_percentile, group == grp)
  
  output_chart <- generate_candles_chart(filtered_boxplot_data, filtered_values_more_than_percentile)
  print(output_chart)
  
}
