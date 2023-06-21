# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)
library(scales)
library(stringr)

# Read the input CSV file
data <- fread("~/Documents/r/sample_results_short.csv", 
              header=TRUE, 
              select=c("timeStamp", "label", "elapsed", "success"))

data$timeStamp <- as.POSIXct(data$timeStamp, format = "%Y/%m/%d %H:%M:%OS")
colnames(data) <- c("timeStamp", "group","response", "success")

print(names(data))

x_axis_name <- names(data)[2]
y_axis_name <- names(data)[1]
title <- sprintf("Confidence interval for %s", names(data)[1])

#update theme for visualization
theme_set (theme_light())
theme_update (
  text = element_text(family = "Courier New"),
  axis.text = element_text(size = 10),
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  axis.title.y = element_text(margin = margin(r = 8, unit = "pt")),
  axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
  legend.position = "none"
)

# Calculate mean and confidence intervals for each group
group_stats <- data %>%
  group_by(group) %>%
  summarise(
    mean = mean(response),
    lower_ci_90 = mean - qt(0.90, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_90 = mean + qt(0.90, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    lower_ci_95 = mean - qt(0.95, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_95 = mean + qt(0.95, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    lower_ci_99 = mean - qt(0.99, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_99 = mean + qt(0.99, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    lower_ci_999 = mean - qt(0.999, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_999 = mean + qt(0.999, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    percentile_95 = quantile(response, 0.95)
  )

print (group_stats)

# Find absolute maximum from group_stats
# If you want to zoom in the values, then set
# limit_x_left <- 0.85*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))
limit_x_left = 500
limit_x_right = 750
#limit_x_right <- 1.15*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))

theme_set (theme_light())

theme_update (
  text = element_text(family = "Courier"),
  axis.text = element_text(size = 9),
  plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
  axis.title.y = element_text(margin = margin(r = 8, unit = "pt")),
  axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
  legend.position = "none"
)

limit_x_left <- 0.85*min(unlist(group_stats[, sapply(group_stats, is.numeric)]))
limit_x_right <- 1.15*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))

#converting group in factor variable
group_stats$group <- factor(group_stats$group, levels = group_stats$group[order(group_stats$mean, decreasing = TRUE)])

# Define custom color palette for each group
group_colors <- plasma(length(unique(group_stats$group)), end = 0.85)
print(group_colors)

# Generate the graph
graph <- ggplot(group_stats, aes(y = fct_rev(group), x = mean, color = group)) +
  geom_errorbarh(aes(xmin = lower_ci_90, xmax = upper_ci_90), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_errorbarh(aes(xmin = lower_ci_95, xmax = upper_ci_95), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_errorbarh(aes(xmin = lower_ci_99, xmax = upper_ci_99), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_point(size = 3) +
  labs(x = x_axis_name, y = y_axis_name, title = title, color = "Legend") +
  xlim(limit_x_left, limit_x_right) +
  scale_color_manual(values = group_colors)

density_plots <- data %>%
  group_by(group) %>%
  do(
    graph2 = ggplot(., aes(x = response)) +
      #  geom_histogram(aes(y=..density..), binwidth = 100, colour="lightblue", fill="lightblue", alpha = 0.2) +
      geom_density(aes(color=group), stat="density", size=1, fill = "NA") +
      geom_vline(data=., aes(xintercept=mean(response)),  colour="red",
                 linetype="dashed", size=0.7, alpha = 0.5) +
      geom_vline(data=., aes(xintercept=quantile(response, 0.95)), colour="blue",
                 linetype="dashed", size=0.7, alpha = 0.5) +
      #  labs(color = group) +
      xlab("Response") +
      ylab("Density") +
      ggtitle(paste("Density plot:", unique(.$group))) +
      #geom_text(data = ., aes(x = mean(response), y = 0.002, label = paste0("Mean: ", round(mean(response), 1), "\n")), 
      #          angle = 90, vjust = -0.5, color = "red", size = 3.5, family="Courier") +
      #geom_text(data = ., aes(x = mean(response), y = 0.002, label = paste0("Mean: ", round(mean(response), 1))), 
              #angle = 90, vjust = -0.5, color = "red", size = 3.5, family="Courier") +
      #geom_text(data = ., aes(x = quantile(response, 0.95), y = 0.002, label = paste0("95p: ", round(quantile(response, 0.95), 1))), 
                #angle = 90, vjust = -0.5, color = "blue", size = 3.5, family="Courier") +
      annotate ("text", mean(.$response), y=max(density(.$response)$y), 
                label = paste0("Mean: ", round(mean(.$response), 1)), 
                angle = 90, vjust = -1, hjust = 1, color = "red", size = 3.5, family="Courier") + 
      annotate ("text", quantile(.$response, 0.95), y=max(density(.$response)$y), 
                label = paste0("95p: ", round(mean(.$response), 1)), 
                angle = 90, vjust = -1, hjust = 1, color = "blue", size = 3.5, family="Courier")
  )

#------------------Build Candles BEGIN--------------------------#
quantity_of_candles = 20
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
geom_point_size = 1.8
geom_point_stroke_size = 0
legend_line_color = "#636363"
legend_line_size = 0.3
legend_line_height = 0.9
legend_text_color = "#636363"
legend_font_family = "Courier New"

breaks <- seq(min(data$timeStamp), max(data$timeStamp) +1, length.out = quantity_of_candles + 1)
print(breaks)
#data$interval <- cut(data$timeStamp, breaks = breaks, labels = breaks[-1])
data$interval <- cut(data$timeStamp, breaks = breaks)


boxplot_data <- data %>%
  group_by(interval) %>%
  reframe(
    min_val = min(response),
    max_val = max(response),
    lower_quantile = quantile(response, lower_quantile_setting),
    median_val = median(response),
    upper_quantile = quantile(response, higher_quantile_setting),
    success_true_ratio = sum(success == "true") / n(),
    success_false_ratio = sum(success == "false") / n()
  )

percentile <- data %>% 
  group_by(interval) %>%
  summarize(percentile = quantile(response, higher_quantile_setting, na.rm = TRUE))

# Filter data for response values higher than percentile within each interval
filtered_data <- data %>%
  inner_join(percentile, by = "interval") %>%
  filter(response > percentile)

graph3 <- ggplot(boxplot_data, aes(x = factor(interval))) +
  
  geom_rect(aes(fill = "success", 
                xmin = as.numeric(interval) + 0.5 - boxplot_width / 2, 
                xmax = as.numeric(interval) + 0.5 + boxplot_width / 2,
                ymin = lower_quantile, 
                ymax = upper_quantile * success_true_ratio), 
            alpha = success_color_alpha) +
  
  geom_rect(aes(fill = "fail", 
                xmin = as.numeric(interval) + 0.5 - boxplot_width / 2, 
                xmax = as.numeric(interval) + 0.5 + boxplot_width / 2,
                ymin = upper_quantile * success_true_ratio, 
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
  
  geom_point(data=filtered_data, 
             aes(y = response), 
             alpha=geom_point_alpha, 
             size = geom_point_size, 
             stroke = geom_point_stroke_size, 
             position = position_nudge(x=.5)) +
  
  #geom_jitter() - use for different representation
  
  scale_fill_manual(values = c("success" = success_color, "fail" = fail_color)) +
  
  labs(x = "Time Interval", y = "Response Time (ms)", title = "Response Time Distribution over time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Courier New"),
        plot.title = element_text(hjust = 0.5, face="bold"),
        axis.title.y = element_text(margin = margin(r = 12, unit = "pt")),
        axis.title.x = element_text(margin = margin(t = 12, unit = "pt")),
        ) +
  guides(fill = "none") +
  #scale_x_datetime(labels = function(x) format(x, "%H:%M:%S")) +
  scale_x_discrete(labels = function(x) format(strptime(x, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")) +
  scale_y_continuous(labels = label_comma(big.mark = ","))

print(graph3)


set.seed(1)
y <- rnorm(100)
legend_data_frame_generated <- data.frame(
  x = 1,
  y0 = min(y),
  lower_quantile = quantile(y, 0.25),
  med = median(y),
  upper_quantile = quantile(y, 0.75),
  y100 = max(y)
)

random_points_for_legend <- data.frame(y = runif(10, min = legend_data_frame_generated$upper_quantile, 
                                max = legend_data_frame_generated$upper_quantile*1.5))

graph4 <- ggplot(legend_data_frame_generated, aes(x)) +
  
  geom_rect(aes(fill = "success_true", 
                xmin = x - boxplot_width/2, 
                xmax = x + boxplot_width/2,
                ymin = lower_quantile, 
                ymax = upper_quantile * 0.6), 
            alpha = success_color_alpha) +
  
  geom_rect(aes(fill = "success_false", 
                xmin = x - boxplot_width/2, 
                xmax = x + boxplot_width/2,
                ymin = upper_quantile * 0.6, 
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
             aes(y = y, x = 1), 
             alpha=geom_point_alpha, 
             size = geom_point_size, 
             stroke = geom_point_stroke_size) +
  
  scale_fill_manual(values = c("success_true" = success_color, 
                               "success_false" = fail_color)) +
  
  theme(aspect.ratio=3.5/1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(color = "grey", fill=NA, size = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 9, face="bold"),
        plot.margin = margin(60,0,80,0, "pt"),
        ) +
  
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
           size = 3.5, 
           family = legend_font_family, 
           fontface = 2) +
  
  annotate("segment", x = 1.2, xend = 1.9, 
           y = legend_data_frame_generated$upper_quantile*1.5*0.85, 
           yend = legend_data_frame_generated$upper_quantile*1.5*0.85,
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile*1.5*0.85,
           color = legend_text_color, 
           label = str_wrap(paste0("Outliers higher than ", higher_quantile_setting*100, "p value"), width=13), 
           lineheight = legend_line_height,
           size = 2.5, 
           family = legend_font_family, 
           fontface = 2) +
  
  annotate("segment", x = 1.2, xend = 2.3, 
           y = legend_data_frame_generated$upper_quantile, 
           yend = legend_data_frame_generated$upper_quantile,
           color = legend_line_color,
           size = legend_line_size) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile,
           color = legend_text_color,
           label = stringr::str_wrap(paste0(higher_quantile_setting*100,"p value"), width=10),  
           lineheight = legend_line_height,
           size = 2.5, 
           family = legend_font_family, 
           fontface = 2) +
  
  annotate("segment", x = 1.2, xend = 2.5, 
         y = legend_data_frame_generated$upper_quantile * 0.775, 
         yend = legend_data_frame_generated$upper_quantile * 0.775,
         color = legend_line_color,
         size = legend_line_size) +
  
  annotate("text", x = 3.9, 
           y = legend_data_frame_generated$upper_quantile * 0.775,
           color = fail_color, 
           label = stringr::str_wrap("Failed requests ratio", width=10), 
           lineheight = legend_line_height,
           size = 2.5, 
           family = legend_font_family, 
           fontface = 2) +
  
  annotate("segment", x = 1.2, xend = 2.5, 
           y = legend_data_frame_generated$upper_quantile*0.45, 
           yend = legend_data_frame_generated$upper_quantile * 0.45,
           color = legend_line_color,
           size = legend_line_size) + 
  
  annotate("text", x = 4, 
           y = legend_data_frame_generated$upper_quantile*0.45,
           color = success_color, 
           label = stringr::str_wrap("Passed requests ratio", width=10), 
           lineheight = legend_line_height,
           size = 2.5, 
           family = legend_font_family, 
           fontface = 2) +

  annotate("segment", x = 1.2, xend = 2.5, 
           y = legend_data_frame_generated$med, 
           yend = legend_data_frame_generated$med,
           color = legend_line_color,
           size = legend_line_size) + 
  
  annotate("text", x = 4, 
           y = legend_data_frame_generated$med,
           color = legend_text_color, 
           label = stringr::str_wrap("Median value", width=10), 
           size = 2.5, 
           lineheight = legend_line_height,
           family = legend_font_family, 
           fontface = 2) +
  
  annotate("segment", x = 1.2, xend = 2.5, 
           y = legend_data_frame_generated$lower_quantile, 
           yend = legend_data_frame_generated$lower_quantile,
           color = legend_line_color,
           size = legend_line_size) +

  annotate("text", x = 4, 
         y = legend_data_frame_generated$lower_quantile,
         color = legend_text_color, 
         label = stringr::str_wrap(paste0(lower_quantile_setting*100,"p value"), width=10), 
         size = 2.5, 
         family = legend_font_family, 
         fontface = 2)
  
print(graph4)

arranged_graphs <- ggarrange(graph3, graph4, nrow = 1, ncol = 2, widths = c(0.85, 0.15))

print(arranged_graphs)
#------------------Build Candles END--------------------------#

# Display the graph
#  print(density_plots$graph2)
#  print(graph)


