# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)
library(extrafont)
library(gridExtra)
library(ggpubr)
library(scales)

source("graphs.R")

# Read the input CSV file
data <- fread("~/Documents/r/sample_results_short.csv", 
              header=TRUE, 
              select=c("timeStamp", "label", "elapsed", "success"))

data$timeStamp <- as.POSIXct(data$timeStamp, format = "%Y/%m/%d %H:%M:%OS")
colnames(data) <- c("timeStamp", "group","response", "success")

#print(names(data))

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


# Filter and generate separate graphs for "*.js" and "*.css" groups
#graph_group_one <- generate_graph_conf(data %>% filter(grepl("Wagers", group)), "Response", "Request", "Confidence interval for APIs")
#graph_group_two <- generate_graph_conf(data %>% filter(!grepl("Wagers", group) & !grepl("Log", group)), "Response", "Request", "Confidence interval for APIs")
graph_group_one <- generate_graph_conf(data, "Response", "Request", "Confidence interval for APIs", 90, reverse = TRUE)
graph_group_two <- generate_graph_conf(data, "Response", "Request", "Confidence interval for APIs", 90, reverse = FALSE)
graph_group_three <- generate_graph_percentile((data), "Response", "Group", "Percentile for pages")
graph_group_four <- generate_graph_scatter((data), "Response", "Group", "Scatter plot for APIs")

print(graph_group_four)

# Use this line of code to arrange all latency graphs on one sheet (not recommended)
# arranged_graphsScatter <- do.call(grid.arrange, c(graph_group_four, ncol = 2))

# Use this if to arrange latency graphs on several sheets (by 6)
if (length(graph_group_four) > 6) {
  # Split graph_group_four into chunks of no more than 6 plots each
  graph_chunks <- split(graph_group_four, ceiling(seq_along(graph_group_four)/6))
  
  # Create a list to store arranged graphs
  arranged_graphs_list <- list()
  
  # Loop through each chunk and arrange them
  for (chunk in graph_chunks) {
    arranged_graphs_list[[length(arranged_graphs_list) + 1]] <- do.call(grid.arrange, c(chunk, ncol = 2))
  }
  
  # Print each arranged graph separately
  for (i in seq_along(arranged_graphs_list)) {
    print(arranged_graphs_list[[i]])
  }
} else {
  # If the number of plots is 6 or fewer, arrange them in a single grid
  arranged_graphsScatter <- do.call(grid.arrange, c(graph_group_four, ncol = 2))
  print(arranged_graphsScatter)
}

graph_group_five <- generate_bars(data, "1", "2", "3")

# The code below arranges graphs by pair and aligns them.

arranged_graphs1 <- ggarrange(
  graph_group_one$graph1, graph_group_two$graph1,
  ncol = 1, nrow = 2, align = "v")

arranged_graphs2 <- ggarrange(
  graph_group_one$graph2, graph_group_two$graph2,
  ncol = 1, nrow = 2, align = "v")

arranged_graphs_all <- grid.arrange(
  arranged_graphs1, arranged_graphs2,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, 2))
)

# Display the arranged graphs

#print arranged graphs
print(graph_group_one)
print(graph_group_two)
print(graph_group_three)
print(graph_group_five)
print(arranged_graphs_all)
