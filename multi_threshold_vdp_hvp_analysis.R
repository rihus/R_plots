#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch")

##Functions

create_thresh_plot <- function(df_in1, df_in2, plt_title, x_label, y_label, vline_x) {
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
    geom_point(aes(y = row_avg1, color = "CF", shape = "CF"), size = 3) +
    geom_errorbar(aes(ymin = row_avg1 - row_sd1, ymax = row_avg1 + row_sd1),
                  width = 0.3, color = "black") +
    # Plot row_avg2
    geom_point(aes(y = row_avg2, color = "Ctrl", shape = "Ctrl"), size = 3) +
    geom_errorbar(aes(ymin = row_avg2 - row_sd2, ymax = row_avg2 + row_sd2),
                  width = 0.3, color = "darkgreen") +
    # Plot difference
    geom_point(aes(y = difference, color = "Diff", shape = "Diff"), size = 2) +
    # Add dashed lines for each mean
    geom_line(aes(y = row_avg1), linetype = "dashed", linewidth = 0.75, color = "black") +
    geom_line(aes(y = row_avg2), linetype = "dashed", linewidth = 0.75, color = "black") +
    geom_line(aes(y = difference), linewidth = 0.75, color = "black") +
    # Add vertical line at provided location
    geom_vline(xintercept = vline_x, linetype = "dashed", color = "darkblue", linewidth = 1.25) +
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
    scale_color_manual(values = c("black", "darkgreen", "coral2"),
                       name = "Subjects",
                        labels = c("CF", "Ctrl", "Diff")) +
    scale_shape_manual(values = c("CF" = 17, "Ctrl" = 19, "Diff" = 15),
                       name = "Subjects",
                       labels = c("CF", "Ctrl", "Diff")) +
    guides(shape = guide_legend(override.aes = list(size = 3))
    )
  # Return the plot
  return(scat_plt)
}

# # Dark Blue (#003366), Orange (#FF8000), Yellow (#FFFF00), Purple (#9933FF),
# # Light Green (#90EE90)

################################################################################
# ##Load and plot ******************** CARTESIAN
cf_hvp_N4c <- read.csv("./IRC740H_2Dcartesian_CF/multi_threshold_analysis_results/N4_corr_hvp_thresholds_new.csv")
ctrl_hvp_N4c <- read.csv("./IRC740H_2Dcartesian_healthy/multi_threshold_analysis_results/N4_corr_hvp_thresholds_new.csv")
Cart_N4_hvp <- create_thresh_plot(cf_hvp_N4c, ctrl_hvp_N4c, "Cartesian: N4 HVP Thresholds",
                   "Threshold (% of mean)", "Average HVP", 200)
print(Cart_N4_hvp)
cf_vdp_N4c <- read.csv("./IRC740H_2Dcartesian_CF/multi_threshold_analysis_results/N4_corr_vdp_thresholds_new.csv")
ctrl_vdp_N4c <- read.csv("./IRC740H_2Dcartesian_healthy/multi_threshold_analysis_results/N4_corr_vdp_thresholds_new.csv")
Cart_N4_vdp <- create_thresh_plot(cf_vdp_N4c,ctrl_vdp_N4c, "Cartesian: N4 VDP Thresholds",
                   "Threshold (% of mean)", "Average VDP", 60)
print(Cart_N4_vdp)
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/Cart_N4_hvp_thresholds.png", plot = Cart_N4_hvp, width = 4.5, height = 3.25, dpi = 300)
ggsave("./zR_plots_4ppr/Cart_N4_vdp_thresholds.png", plot = Cart_N4_vdp, width = 4.5, height = 3.25, dpi = 300)

# ##Load and plot ******************* SPIRAL
cf_hvp_N4s <- read.csv("./IRC740H_2Dspiral_CF/multi_threshold_analysis_results/N4_corr_hvp_thresholds_new.csv")
ctrl_hvp_N4s <- read.csv("./IRC740H_2Dspiral_healthy/multi_threshold_analysis_results/N4_corr_hvp_thresholds_new.csv")
Spir_N4_hvp <- create_thresh_plot(cf_hvp_N4s, ctrl_hvp_N4s, "Spiral: N4 HVP Thresholds",
                   "Threshold (% of mean)", "Average HVP", 200)
print(Spir_N4_hvp)
cf_vdp_N4s <- read.csv("./IRC740H_2Dspiral_CF/multi_threshold_analysis_results/N4_corr_vdp_thresholds_new.csv")
ctrl_vdp_N4s <- read.csv("./IRC740H_2Dspiral_healthy/multi_threshold_analysis_results/N4_corr_vdp_thresholds_new.csv")
Spir_N4_vdp <- create_thresh_plot(cf_vdp_N4s, ctrl_vdp_N4s, "Spiral: N4 VDP Thresholds",
                   "Threshold (% of mean)", "Average VDP", 60)
print(Spir_N4_vdp)
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/Spir_N4_hvp_thresholds.png", plot = Spir_N4_hvp, width = 4.5, height = 3.25, dpi = 300)
ggsave("./zR_plots_4ppr/Spir_N4_vdp_thresholds.png", plot = Spir_N4_vdp, width = 4.5, height = 3.25, dpi = 300)

cf_hvp_FAs <- read.csv("./IRC740H_2Dspiral_CF/multi_threshold_analysis_results/FA_corr_hvp_thresholds_new.csv")
ctrl_hvp_FAs <- read.csv("./IRC740H_2Dspiral_healthy/multi_threshold_analysis_results/FA_corr_hvp_thresholds_new.csv")
Spir_FA_hvp <- create_thresh_plot(cf_hvp_FAs, ctrl_hvp_FAs, "Spiral: FA HVP Thresholds",
                   "Threshold (% of mean)", "Average HVP", 200)
print(Spir_FA_hvp)
cf_vdp_FAs <- read.csv("./IRC740H_2Dspiral_CF/multi_threshold_analysis_results/FA_corr_vdp_thresholds_new.csv")
ctrl_vdp_FAs <- read.csv("./IRC740H_2Dspiral_healthy/multi_threshold_analysis_results/FA_corr_vdp_thresholds_new.csv")
Spir_FA_vdp <- create_thresh_plot(cf_vdp_FAs, ctrl_vdp_FAs, "Spiral: FA VDP Thresholds",
                   "Threshold (% of mean)", "Average VDP", 40)
print(Spir_FA_vdp)
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/Spir_FA_hvp_thresholds.png", plot = Spir_FA_hvp, width = 4.5, height = 3.25, dpi = 300)
ggsave("./zR_plots_4ppr/Spir_FA_vdp_thresholds.png", plot = Spir_FA_vdp, width = 4.5, height = 3.25, dpi = 300)


