#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch")

##Functions

create_thresh_plot <- function(df_in1, df_in2, plt_title, x_label, y_label) {
  # Extract x_data
  x_data <- df_in1[, 1]
  # Calculate means and standard deviations for df_in1 and df_in2
  row_avg1 <- rowMeans(df_in1[, 2:ncol(df_in1)])
  row_sd1 <- apply(df_in1[, 2:ncol(df_in1)], 1, sd)
  row_avg2 <- rowMeans(df_in2[, 2:ncol(df_in2)])
  row_sd2 <- apply(df_in2[, 2:ncol(df_in2)], 1, sd)
  # Calculate the difference between means
  difference <- row_avg1 - row_avg2
  # Create the ggplot object
  scat_plt <- ggplot(df_in1, aes(x = x_data)) +
    # Plot row_avg1 with coral2 color and error bars
    geom_point(aes(y = row_avg1), color = "coral2", shape=17, size = 3) +
    geom_errorbar(aes(ymin = row_avg1 - row_sd1, ymax = row_avg1 + row_sd1),
                  width = 0.3, color = "coral2") +
    # Plot row_avg2 with green color
    geom_point(aes(y = row_avg2), color = "green", shape= 19, size = 3) +
    geom_errorbar(aes(ymin = row_avg2 - row_sd2, ymax = row_avg2 + row_sd2),
                  width = 0.3, color = "green") +
    # Plot difference with red color
    geom_point(aes(y = difference), color = "#9933FF", shape=18, size = 4) +
    # Add dashed lines for each mean
    geom_line(aes(y = row_avg1), linetype = "dashed", linewidth = 1, color = "black") +
    geom_line(aes(y = row_avg2), linetype = "dashed", linewidth = 1, color = "black") +
    geom_line(aes(y = difference), linewidth = 1, color = "#003366") +
    # Add labels and title
    labs(title = plt_title, x = x_label, y = y_label) +
    # Set theme
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 16, face = "bold", color = "black"),
      axis.text.y = element_text(size = 16, face = "bold", color = "black"),
      axis.title = element_text(size = 16, face = "bold")
    )
  # print the plot
  print(scat_plt)
}

create_thresh_plot <- function(df_in1, df_in2, plt_title, x_label, y_label) {
  # Extract x_data
  x_data <- df_in1[, 1]
  # Calculate means and standard deviations for df_in1 and df_in2
  row_avg1 <- rowMeans(df_in1[, 2:ncol(df_in1)])
  row_sd1 <- apply(df_in1[, 2:ncol(df_in1)], 1, sd)
  row_avg2 <- rowMeans(df_in2[, 2:ncol(df_in2)])
  row_sd2 <- apply(df_in2[, 2:ncol(df_in2)], 1, sd)
  # Calculate the difference between means
  difference <- row_avg1 - row_avg2
  # Create the ggplot object
  scat_plt <- ggplot(df_in1, aes(x = x_data)) +
    # Plot row_avg1 with coral2 color and error bars
    geom_point(aes(y = row_avg1, color = "CF"), shape = 17, size = 3) +
    geom_errorbar(aes(ymin = row_avg1 - row_sd1, ymax = row_avg1 + row_sd1),
                  width = 0.3, color = "coral2") +
    # Plot row_avg2 with green color
    geom_point(aes(y = row_avg2, color = "Ctrl"), shape = 19, size = 3) +
    geom_errorbar(aes(ymin = row_avg2 - row_sd2, ymax = row_avg2 + row_sd2),
                  width = 0.3, color = "green") +
    # Plot difference with red color
    geom_point(aes(y = difference, color = "Diff"), shape = 18, size = 4) +
    # Add dashed lines for each mean
    geom_line(aes(y = row_avg1), linewidth = 0.5, color = "coral2") +
    geom_line(aes(y = row_avg2), linewidth = 0.5, color = "green") +
    geom_line(aes(y = difference), linewidth = 1, color = "#9933FF") +
    # Add labels and title
    labs(title = plt_title, x = x_label, y = y_label) +
    # Set theme
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, face = "bold", color = "black"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    ) +
    # Add legend
    scale_color_manual(values = c("coral2", "green", "#9933FF"),
                       name = "Subjects",
                       labels = c("CF", "Ctrl", "Diff")) +
    scale_shape_manual(values = c(17, 19, 18),
                       name = "Legend",
                       labels = c("CF", "Ctrl", "Diff"))
  # Print the plot
  print(scat_plt)
}

# # Dark Blue (#003366), Orange (#FF8000), Yellow (#FFFF00), Purple (#9933FF),
# # Light Green (#90EE90)
################################################################################
# ##Load the data from CSV files
cf_hvp_thresholds <- read.csv("./IRC740H_2Dcartesian_CF/hvp_thresholds_new.csv")
ctrl_hvp_thresholds <- read.csv("./IRC740H_2Dcartesian_healthy/hvp_thresholds_new.csv")

create_thresh_plot(cf_hvp_thresholds, ctrl_hvp_thresholds, "HVP Thresholds Comparison",
                   "Threshold (% of mean)", "Average HVP")

cf_vdp_thresholds <- read.csv("./IRC740H_2Dcartesian_CF/vdp_thresholds_all_new.csv")
ctrl_vdp_thresholds <- read.csv("./IRC740H_2Dcartesian_healthy/vdp_thresholds_all_new.csv")


create_thresh_plot(cf_vdp_thresholds,ctrl_vdp_thresholds, "VDP Thresholds Comparison",
                   "Threshold (% of mean)", "Average VDP")


# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/hvp200_linreg_plot.png", plot = hvp200_linreg, width = 4.5, height = 3.7, dpi = 300)



