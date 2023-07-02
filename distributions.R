library(scales)
library(data.table)
library(ggplot2)
library(dplyr)
library(ggpattern)

####GENERATE DATA FOR YOUR CANDLES###
generate_dataframe <- function(n, start_time, end_time, labels, elapsed_interval, success_prob) {
  # Generate random timestamps within the defined range
  timestamps <- as.POSIXct(runif(n, as.numeric(as.POSIXct(start_time)), as.numeric(as.POSIXct(end_time))), origin = "1970-01-01")
  
  # Randomly select labels from the pre-defined types
  labels <- sample(labels, n, replace = TRUE)
  
  # Randomly generate elapsed times within the defined interval
  elapsed <- sample(seq(elapsed_interval[1], elapsed_interval[2]), n, replace = TRUE)
  
  # Randomly generate success values with the defined probability
  success_binom <- rbinom(n, 1, success_prob)
  success <- ifelse(success_binom == 0, FALSE, TRUE)
  
  # Create the dataframe
  df <- data.frame(timeStamp = timestamps, label = labels, elapsed = elapsed, success = success)
  
  return(df)
}

# Example usage
n <- 1000
start_time <- "2024-01-01 00:00:00"
end_time <- "2024-01-01 01:00:00"
labels <- c("Type1", "Type2", "Type3")
elapsed_interval <- c(100, 10000) # Define the interval for elapsed time
success_prob <- 0.8 # Probability for success

data_original <- generate_dataframe(n, start_time, end_time, labels, elapsed_interval, success_prob)
colnames(data_original) <- c("timeStamp", "group","response", "success")

n <- 5000
start_time <- "2024-02-01 00:00:00"
end_time <- "2024-02-01 23:00:00"
labels <- c("Type3", "Type4", "Type7")
elapsed_interval <- c(700, 20000) # Define the interval for elapsed time
success_prob <- 0.8 # Probability for success

data_comparison <- generate_dataframe(n, start_time, end_time, labels, elapsed_interval, success_prob)
colnames(data_comparison) <- c("timeStamp", "group","response", "success")

#####Distribution for single data#####
distrib_single <- function(data, type, limit) {
  
  ####CHART SETTINGS####
  bar_color = "#6a994e"
  bar_border = "#386641"
  intercept_color = "#bc4749"
  intercept_width = 0.7
  intercept_line_alpha = 0.8
  intercept_text_alpha = intercept_line_alpha
  intercept_text_color = intercept_color
  intercept_text_size = 3.7
  data_labels_color = bar_border
  data_labels_size = 3.0
  chart_title = "Distribution"
  y_title = "Percentage of total quantity of requests"
  x_title = "Response bins, ms"
  
  #filter the data for the particular request
  data_filtered <- data %>% filter(group == type)
  
  median = median(data_filtered$response)
  
  upper = limit
  
  #set bin size to the 1/10 of the sla
  bin_size = upper/10
  
  all_bins <- data.frame(bin = seq(0, upper, by = bin_size))
  
  data_summary <- data_filtered %>%
    mutate(bin = if_else(response < upper, response %/% bin_size * bin_size, upper)) %>%
    summarize(n = n(), .by = bin) %>%
    right_join(all_bins, by = "bin") %>%
    mutate(n = replace_na(n, 0))
  
  if (median > max(data_summary$bin)) {
    intercept_line_alpha = 0.0
    xintercept_value = 0
    annotate_x = max(data_summary$bin) + bin_size
    annotate_angle = 0
    annotate_label = paste0("Median: ", round(median, 1), " →")
    
  } else {
    xintercept_value <- median
    annotate_x = median
    annotate_angle = 90
    annotate_label = paste0("Median: ", round(median, 1))  
  }
  
  distrib <- ggplot(data_summary, aes(bin + bin_size/2, n / sum(n))) +
    geom_col(fill = bar_color,
             color = bar_border ) +
    geom_vline(xintercept = xintercept_value,
               colour = intercept_color,
               linetype = "dashed", 
               linewidth = intercept_width, 
               alpha = intercept_line_alpha) +
    annotate("text", 
             x = annotate_x, 
             y = 1, 
             label = annotate_label, 
             angle = annotate_angle, 
             vjust = 1.5, 
             hjust = 0.9, 
             color = intercept_text_color, 
             size = intercept_text_size,
             alpha = intercept_text_alpha) +
    scale_x_continuous(breaks = scales::breaks_width(bin_size),
                       labels = function(x) {
                         ifelse(x == upper, 
                                paste0(">", scales::number_format(accuracy = 1)(x)), 
                                ifelse(x > upper, 
                                       paste0(""), 
                                       scales::number_format(accuracy = 1)(x)
                                )
                         )
                       },
                       limits = c(0, max(data_summary$bin)+bin_size)) + 
    scale_y_continuous(labels = scales::percent,
                       limits = c(-0.015, 1)) +
    geom_text(aes(y = n / sum(n) + 0.075, label = scales::percent((n / sum(n)), accuracy = 0.1)),
              vjust = 1.5,
              color = data_labels_color,
              size = data_labels_size) +
    labs(x = x_title, 
         y = y_title,
         title = chart_title,
         subtitle = type) +
    theme_minimal() +
    theme(axis.text.x = element_text(margin = margin(t = 10, b = 10)),
          plot.subtitle = element_text(color = "#777777"))
  
  return(distrib)
}

#####Distribution comparison#####
distrib_compar <- function(data_orig, 
                           data_orig_name, 
                           data_compar, 
                           data_compar_name,
                           type, 
                           limit) {
  ####CHART SETTINGS####
  bar_color = "#6a994e"
  bar_border = "#386641"
  intercept_color = "#bc4749"
  intercept_width = 0.7
  intercept_line_alpha = 0.8
  intercept_text_alpha = intercept_line_alpha
  intercept_text_color = intercept_color
  intercept_text_size = 3.7
  data_labels_color = bar_border
  data_labels_size = 3.0
  data_labels2_size = data_labels_size - 0.5
  pattern_alpha = 0.7
  pattern_border = "#333333"
  
  data_filtered_main <- data_orig %>% filter(group == type)
  data_filtered_comparison <- data_compar %>% filter(group == type)
  
  median = median(data_filtered_main$response)
  
  upper = limit
  
  bin_size = upper/10
  
  all_bins <- data.frame(bin = seq(0, upper, by = bin_size))
  
  data_summary_main <- data_filtered_main %>%
    mutate(bin = if_else(response < upper, response %/% bin_size * bin_size, upper)) %>%
    summarize(n = n(), .by = bin) %>%
    right_join(all_bins, by = "bin") %>%
    mutate(n = replace_na(n, 0))
  
  data_summary_comparison <- data_filtered_comparison %>%
    mutate(bin = if_else(response < upper, response %/% bin_size * bin_size, upper)) %>%
    summarize(n = n(), .by = bin) %>%
    right_join(all_bins, by = "bin") %>%
    mutate(n = replace_na(n, 0))
  
  data_summary_main$type <- data_orig_name
  data_summary_comparison$type <- data_compar_name
  combined_data <- rbind(data_summary_main, data_summary_comparison)
  
  if (median > max(combined_data$bin)) {
    intercept_line_alpha = 0.0
    xintercept_value = 0
    annotate_x = max(combined_data$bin) + bin_size
    annotate_angle = 0
    annotate_label = paste0("Median: ", round(median, 1), " →")
    
  } else {
    xintercept_value <- median
    annotate_x = median
    annotate_angle = 90
    annotate_label = paste0("Median: ", round(median, 1))  
  }
  
  fill_values <- setNames(c(bar_color, rgb(1, 1, 1, 0)), c(data_orig_name, data_compar_name))
  
  #CHART
  distrib_comparison <- ggplot(combined_data, aes(bin + bin_size/2, n / sum(n)), fill = type) +
    geom_col(aes(fill = type),
             data = subset(combined_data, type == data_orig_name),
             color = bar_border) +
    geom_col_pattern(aes(fill = type),
                     data = subset(combined_data, type == data_compar_name),
                     pattern = "stripe",
                     pattern_angle = 45,
                     pattern_density = .015,
                     pattern_spacing = .015,
                     pattern_fill = "#c7c7c7",
                     color = pattern_border,
                     linewidth = 0.4,
                     pattern_alpha = pattern_alpha
    ) +
    geom_vline(xintercept = xintercept_value,
               colour = intercept_color,
               linetype = "dashed", 
               linewidth = intercept_width, 
               alpha = intercept_line_alpha) +
    annotate("text", 
             x = annotate_x, 
             y = 1, 
             label = annotate_label, 
             angle = annotate_angle, 
             vjust = 1.5, 
             hjust = 0.9, 
             color = intercept_text_color, 
             size = intercept_text_size,
             alpha = intercept_text_alpha) +
    scale_x_continuous(breaks = scales::breaks_width(bin_size),
                       labels = function(x) {
                         ifelse(x == upper, 
                                paste0(">", scales::number_format(accuracy = 1)(x)), 
                                ifelse(x > upper, 
                                       paste0(""), 
                                       scales::number_format(accuracy = 1)(x)
                                )
                         )
                       },
                       limits = c(0, max(data_summary_main$bin)+bin_size)) + 
    scale_y_continuous(labels = scales::percent,
                       limits = c(-0.05, 1)) +
    geom_text(data = subset(combined_data, type == data_orig_name),
              aes(label = scales::percent((n / sum(n)), accuracy = 0.1)), 
              vjust = 1.5, 
              y = -0.01, 
              color = data_labels_color,
              size = data_labels_size) +
    geom_text(data = subset(combined_data, type == data_orig_name),
              aes(label = scales::comma(n)), 
              vjust = 1.5, 
              y = -0.06, 
              color = data_labels_color,
              size = data_labels2_size) +
    labs(x = "Response time, ms", 
         y = "Percentage of total requests",
         title = "Distribution diagram",
         subtitle = type) +
    scale_fill_manual(values = fill_values,
                      # labels = c(data_compar_name, data_orig_name),
                      limits = c(data_orig_name, data_compar_name)) +
    theme_minimal() +
    theme(axis.text.x = element_text(margin = margin(t = 10, b = 10)),
          legend.title = element_blank(),
          plot.subtitle = element_text(color = "#777777"),
          legend.position = "inside",
          legend.position.inside = c(0.85, 0.85)
    )
  
  return(distrib_comparison)
  
}

distrib_single(data_original, "Type1", 4000)
distrib_compar(data_original, "Original Data", data_comparison, "Comparison Data", "Type3", 5000)