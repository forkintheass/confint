##CONF_INT + DENSITY PLOT##
# Function to generate two graphs: conf int and density plot
generate_graph_conf <- function(data, x_axis_name, y_axis_name, title, include_percentile, reverse = FALSE) {

    # Calculate the 90th percentile of all values
    percentile_num = include_percentile/100
    percentile = quantile(data$response, percentile_num)
    
    # Filter data to include only groups with values within the 90th percentile
    data <- data %>%
      group_by(group) %>%
      mutate (
        upper_ci_99 = mean(response) + qt(0.99, df = n() - 1) * (sd(response) / sqrt(n()-1))  
      ) %>%
      ungroup()
    
  if (reverse) {
    data <- data %>%
      group_by(group) %>%
      filter(upper_ci_99 < percentile)
  } else {
    data <- data %>%
      group_by(group) %>%
      filter(upper_ci_99 > percentile)    
  }

  
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
    ) %>%
    arrange(desc(mean))
  
  #print (group_stats)
  
  all_stats <- data %>%
    summarise(
      all_percentile = quantile(response, 0.80),
    )
  
  #print(all_stats$all_percentile_80)
  
  # Define custom color palette for each group
  group_colors <- plasma(length(unique(group_stats$group)), end = 0.85)
  
  # Find absolute maximum from group_stats
  # If you want to zoom in the values, then set
  # limit_x_left = 0
  # limit_x_right = 30000
  limit_x_left <- 0.85*min(unlist(group_stats[, sapply(group_stats, is.numeric)]))
  limit_x_right <- 1.15*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))
  
  #converting group in factor variable
  group_stats$group <- factor(group_stats$group, levels = group_stats$group[order(group_stats$mean, decreasing = FALSE)])
  
  # Generate the graph
  graph1 <- ggplot(group_stats, aes(y = group, x = mean, color = group)) +
    geom_errorbarh(aes(xmin = lower_ci_90, xmax = upper_ci_90), height = 0.0, linewidth = 2.5, alpha = 0.25) +
    geom_errorbarh(aes(xmin = lower_ci_95, xmax = upper_ci_95), height = 0.0, linewidth = 2.5, alpha = 0.25) +
    geom_errorbarh(aes(xmin = lower_ci_99, xmax = upper_ci_99), height = 0.0, linewidth = 2.5, alpha = 0.25) +
    geom_point(size = 3) +
    labs(x = x_axis_name, y = y_axis_name, title = title, color = "Legend") +
    scale_x_continuous(labels = comma, limits = c(limit_x_left, limit_x_right)) +
    scale_color_manual(values = group_colors)
  
  # Create the density plot using facets
  graph2 <- ggplot(data, aes(x = response)) +
    #geom_histogram(aes(y=..density..), binwidth = 10000, colour="brown", fill="lightblue", alpha = 0.2) +
    geom_density(aes(color=group), stat="density", size=0.71) +
    labs(x = "Response", y = "Density", title = "Density", color = group_colors) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma, limits = c(0, all_stats$all_percentile)) +
    scale_color_manual(values = group_colors)
  #geom_vline(data=group_stats, aes(xintercept=mean),  colour="red",
  #linetype="dashed", linewidth=0.7, alpha = 0.5) +
  #geom_vline(data=group_stats, aes(xintercept=percentile_95), colour="blue",
  #linetype="dashed", linewidth=0.7, alpha = 0.5) +
  #geom_text(data = group_stats, aes(x = mean, y = 0.001, label = paste0("Mean: ", round(mean, 0), "\n")), vjust = -1.5, color = "red") +
  #geom_text(data = group_stats, aes(x = percentile_95, y = 0.002, label = paste0(round(percentile_95, 1))), angle = 90, vjust = -0.5, color = "blue", size = 3.5, family="Courier New")
  
  #return(graph)
  return(list(graph1 = graph1, graph2 = graph2))
}


##PERCENTILES##
#this function visualizes percentiles
generate_graph_percentile <- function(data, x_axis_name, y_axis_name, title) {
  
  # Calculate mean and confidence intervals for each group
  group_stats <- data %>%
    group_by(group) %>%
    summarise(
      mean = mean(response),
      percentile_90 = quantile(response, 0.90),
      percentile_95 = quantile(response, 0.95),
      percentile_99 = quantile(response, 0.99),
      percentile_50 = quantile(response, 0.5),
      min = min(response)
    ) %>%
    arrange(desc(mean))
  
  #print (group_stats)
  
  # Define custom color palette for each group
  # Adding more elements to group_colors in order lighter elements won't look so bleak
  group_colors <- plasma(length(unique(group_stats$group)), end = 0.85)
  
  # Find absolute maximum from group_stats
  # If you want to zoom in the values, then set
  # limit_x_left = 0
  # limit_x_right = 30000
  limit_x_left <- 0.85*min(unlist(group_stats[, sapply(group_stats, is.numeric)]))
  limit_x_right <- 1.15*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))
  
  #converting group in factor variable and ordering by median
  group_stats$group <- factor(group_stats$group, levels = group_stats$group[order(group_stats$percentile_50, decreasing = FALSE)])
  
  group_stats$num_group <- as.numeric(group_stats$group)
  
  # Generate the graph
  graph <- ggplot(group_stats, aes(y = group, x = percentile_50, color = group)) +
    geom_errorbarh(aes(xmin = percentile_50, xmax = percentile_90), height = 0.0, linewidth = 3.5, alpha = 0.25) +
    geom_errorbarh(aes(xmin = percentile_50, xmax = percentile_95), height = 0.0, linewidth = 3.5, alpha = 0.25) +
    geom_errorbarh(aes(xmin = percentile_50, xmax = percentile_99), height = 0.0, linewidth = 3.5, alpha = 0.25) +
    geom_errorbarh(aes(xmin = min, xmax = percentile_50), height = 0.0, linewidth = 0.8, alpha = 0.3, linetype="solid") +
    geom_point(size = 3) +
    labs(x = x_axis_name, y = y_axis_name, title = title, color = "Legend") +
    scale_color_manual(values = group_colors)+
    scale_x_continuous(labels = comma, limits = c(limit_x_left, limit_x_right)) +
    geom_segment(aes(x = percentile_90, xend = percentile_90, y = num_group + 0.15, yend = num_group - 0.15, color=group), 
                 size = 0.6) +
    geom_segment(aes(x = percentile_95, xend = percentile_95, y = num_group + 0.15, yend = num_group - 0.15, color=group), 
                 size = 0.6) +
    geom_segment(aes(x = percentile_99, xend = percentile_99, y = num_group + 0.15, yend = num_group - 0.15, color=group), 
                 size = 0.6)
  
  group_stats$num_group <- NULL
  
  return(graph)
}


##TIMESCATTER##
#this function visualizes timescatter-plot
generate_graph_scatter <- function(data, x_axis_name, y_axis_name, title) {
  
  # Define custom color palette for each group
  group_colors <- plasma(length(unique(data$group)), end = 0.85)
  
  label_plots <- data %>%
    group_by(group) %>%
    do(
      scatter_plot = ggplot(., aes(x = timeStamp, y = response, color = group)) +
        geom_point(size = 0.5, color = sample(group_colors, size = 1)) +
        scale_color_manual(values = group_colors) +
        labs(x = "Time", y = "Latency") +
        scale_y_continuous(labels = comma) +
        ggtitle(paste("Latency for:", unique(.$group)))
    )
  
  return(label_plots$scatter_plot)
}


##BARS##
#this function visualizes timescatter-plot
generate_bars <- function(data, x_axis_name, y_axis_name, title) {
  
  # Calculate average and 95th percentile for each label
  group_stats <- data %>%
    group_by(group) %>%
    summarise(
      mean = mean(response),
      percentile_95 = quantile(response, 0.95)
    ) %>%
    arrange(desc(mean))
  
  #print(group_stats)
  
  group_colors <- viridis(2, begin = 0.2, end = 0.7)
  #print(group_colors)
  
  # Find the maximum average for setting the x-axis limit
  x_limit_avg <- 1.5 * max(group_stats$mean)
  x_limit_perc <- 1.5 * max(group_stats$percentile_95)  
  # Create the bar chart for average latency
  average_chart <- ggplot(group_stats, aes(y = group, x = mean)) +
    geom_bar(stat = "identity", fill = group_colors[1], width = 0.7, position = position_nudge(y = 0.5)) +
    geom_text(aes(label = round(mean, 2)), hjust = -0.2, position = position_nudge(y = 0.5), family = "Courier New", size = 3) +
    labs(x = "Average Latency", y = "Label") +
    ggtitle("Average Latency by Label") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
      ) +
    scale_x_continuous(labels = comma, limits = c(0, x_limit_avg)) +
    scale_y_discrete(expand = expansion(mult = c(0, .055)))  # Adjust the expand argument
  
  # Create the bar chart for the 95th percentile
  percentile_chart <- ggplot(group_stats, aes(y = group, x = percentile_95)) +
    geom_bar(stat = "identity", fill = group_colors[2], width = 0.7, position = position_nudge(y = 0.5)) +
    geom_text(aes(label = round(percentile_95, 2)), hjust = -0.2, position = position_nudge(y = 0.5), family = "Courier New", size = 3) +
    labs(x = "95th Percentile", y = "") +
    ggtitle("95th Percentile")+
    theme(axis.text.y = element_blank()) +
    scale_x_continuous(labels = comma, limits = c(0, x_limit_perc)) +
    scale_y_discrete(expand = expansion(mult = c(0, .055)))  # Adjust the expand argument
  
  labels <- ggplot(group_stats, aes(y = group, x = mean)) +
    geom_bar(stat = "identity", fill = group_colors[1], width = 0, position = position_nudge(y = 0.5)) + # Change width to 0
    labs(x="x", y = "Label") +
    ggtitle("asdasdasdasd") +
    theme(
      axis.text.y = element_text(vjust = -1.2),
      axis.title.x = element_text(color = "transparent"),
      axis.text.x = element_text(color = "transparent"),
      plot.title = element_text(color = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(0, 0, 0, 10, "pt")),
      panel.border = element_blank()
      ) + 
    scale_x_continuous(labels = comma, limits = c(0, 0)) +
    scale_y_discrete(expand = expansion(mult = c(0, .055))) 
  
  # Arrange both charts on one plot
  combined_plot <- grid.arrange(labels, average_chart, percentile_chart, ncol = 3, widths = c(0.3, 0.35, 0.35))
  #combined_plot2 <- plot_grid(labels, average_chart, percentile_chart, widths = c(0.3, 0.35, 0.35), ncol = 3, nrow=1, align = "hv")
  
  return(combined_plot)
}