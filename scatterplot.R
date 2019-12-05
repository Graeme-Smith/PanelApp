library(ggplot2)
library(plotly)
library(tidyverse)

# Scatterplot of Centrality vs Bridgeness

# Import test data from csv file

plot_data <- read_delim("/home/graeme/Desktop/panelapp_network/panelNet/inst/extdata/cent_bridge_HCM.csv",
           delim = ",")

selected_panel_name <- "test_panel"

# simplify column names
colnames(plot_data) <- c("nodeID", "MLcentrality", "MLbridgeness")

# Perform log transformation to data
plot_data$log_MLcentrality <- log(plot_data$MLcentrality)
plot_data$log_MLbridgeness <- log(plot_data$MLbridgeness)

# Create plot

generate_plot <- function(df) { 
  # Remove rows with NAs caused by regions not included between panels  
  df <- df[complete.cases(df), ]
  df <- plot_data
  # Plot
  p <- df %>%
    ggplot( aes(x=log_MLcentrality, y=log_MLbridgeness)
    ) +
    #geom_point(size=2, shape=23, aes(col=nodeID)) +
    geom_jitter(size=2, shape=23, aes(col=nodeID), width = 0.5, height = 0.5) +
    theme_bw() +
    #ggtitle(paste0("Panel ", selected_panel_name, ", Centrality Vs Bridgeness")) +
    xlab("ModuLand Calculated Centrality") +
    ylab("ModuLand Calculated Bridgeness")
  p
  
  return(p)
}

static_plot <- generate_plot(plot_data)

interactive_plot <- ggplotly(static_plot)

interactive_plot