#If needed, install library/package
#install.packages("ggpubr")

#Load library
library(ggpubr)
library(tidyverse)
library(rstatix)
library(ggsignif)
library(ggplot2)
library(dplyr)
library(rlang)

## CF         Cartesian   Spiral  healthy                 FA-corr   Non-corr     N4-corr
c("#000000", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch")

####################Define functions#####################################
##To plot histograms
create_histogram <- function(x_values, fill_name, x_label, clr) {
  # Perform Shapiro-Wilk normality test
  shapiro_result <- shapiro.test(x_values)
  histt_plot <- ggplot() +
    geom_histogram(aes(x = x_values, fill = fill_name), alpha = 0.5, bins = 20, position = "identity") +
    labs(title = paste("Histogram of", x_label), x = x_label, y = "Bin Count",
         tag =paste("Shapiro-Wilk", "\n",
                    "W =", round(shapiro_result$statistic, 3), "\n",
                    "p =", format(shapiro_result$p.value, digits = 3))) +
    scale_fill_manual(values = c(clr), name = "Method") +
    theme_classic() +
    theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
          axis.line = element_line(color = "#000000", linewidth = 1),
          legend.position = "right", plot.tag = element_text(size=11),
          plot.tag.position = c(0.9, 0.85))
  print(histt_plot)
}

################################################################################
# ##Load the data from both CSV files - for paired Wilcoxon
Non_corr_cart_cf <- read.csv("./IRC740H_2Dcartesian_CF/snr_vdp_hvp_analysis_results/Non_corr_meansig_sdbkg_overallSNR_analysis_results.csv")
Non_corr_cart_ctrl <- read.csv("./IRC740H_2Dcartesian_healthy/snr_vdp_hvp_analysis_results/Non_corr_meansig_sdbkg_overallSNR_analysis_results.csv")
Non_corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/snr_vdp_hvp_analysis_results/Non_corr_meansig_sdbkg_overallSNR_analysis_results.csv")
Non_corr_spir_ctrl <- read.csv("./IRC740H_2Dspiral_healthy/snr_vdp_hvp_analysis_results/Non_corr_meansig_sdbkg_overallSNR_analysis_results.csv")

# #Create a combined data frame with an indicator for Sequence and Category
combined_Non_corr <- rbind(transform(Non_corr_cart_cf, Sequence = "Cartesian", Category = "CF"),
                          transform(Non_corr_cart_ctrl, Sequence = "Cartesian", Category = "Ctrl"),
                          transform(Non_corr_spir_cf, Sequence = "Spiral", Category = "CF"),
                          transform(Non_corr_spir_ctrl, Sequence = "Spiral", Category = "Ctrl"))
# #Create a new data frame selecting shared subjects for connected box plots
common_subjs <- combined_Non_corr$Subject_id[duplicated(combined_Non_corr$Subject_id) | 
                                              duplicated(combined_Non_corr$Subject_id, fromLast = TRUE)]
common_subjs_Non_corr <- combined_Non_corr[combined_Non_corr$Subject_id %in% common_subjs, , drop = FALSE]

################################################################### Normality tests and histograms
##Extract SNR values for Cartesian and Spiral sequences
cartesian_snr <- common_subjs_Non_corr$snr[common_subjs_Non_corr$Sequence == "Cartesian"]
spiral_snr <- common_subjs_Non_corr$snr[common_subjs_Non_corr$Sequence == "Spiral"]
##Normality test and plot of histograms
snr_cartesian_hist=create_histogram(cartesian_snr, "Cartesian", "SNR", "darkblue")
snr_spiral_hist=create_histogram(spiral_snr, "Spiral", "SNR", "darkred")
# #Save the plot as a png file in the current directory
ggsave("./zR_plots_4ppr/cartesian_snr_histogram.png", plot = snr_cartesian_hist, width = 5.0, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/spiral_snr_histogram.png", plot = snr_spiral_hist, width = 5.0, height = 3.8, dpi = 300)

##Extract Mean signal values for Cartesian and Spiral sequences
cartesian_mean_signal <- combined_Non_corr$mean_signal[combined_Non_corr$Sequence == "Cartesian"]
spiral_mean_signal <- combined_Non_corr$mean_signal[combined_Non_corr$Sequence == "Spiral"]
##Normality test and plot of histograms
mean_signal_cartesian_hist=create_histogram(cartesian_mean_signal, "Cartesian", "Mean Signal", "darkblue")
mean_signal_spiral_hist=create_histogram(spiral_mean_signal, "Spiral", "Mean Signal", "darkred")
# #Save the plot as a png file in the current directory
ggsave("./zR_plots_4ppr/cartesian_mean_signal_histogram.png", plot = mean_signal_cartesian_hist, width = 5.0, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/spiral_mean_signal_histogram.png", plot = mean_signal_spiral_hist, width = 5.0, height = 3.8, dpi = 300)

##Extract Noise St. Dev. values for Cartesian and Spiral sequences
cartesian_noise_sd <- combined_Non_corr$bkgd_sd[combined_Non_corr$Sequence == "Cartesian"]
spiral_noise_sd <- combined_Non_corr$bkgd_sd[combined_Non_corr$Sequence == "Spiral"]
##Normality test and plot of histograms
noise_sd_cartesian_hist=create_histogram(cartesian_noise_sd, "Cartesian", "Noise St.D.", "blue")
noise_sd_spiral_hist=create_histogram(spiral_noise_sd, "Spiral", "Noise St.D.", "red")
# #Save the plot as a png file in the current directory
ggsave("./zR_plots_4ppr/cartesian_noise_sd_histogram.png", plot = noise_sd_cartesian_hist, width = 5.0, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/spiral_noise_sd_histogram.png", plot = noise_sd_spiral_hist, width = 5.0, height = 3.8, dpi = 300)
