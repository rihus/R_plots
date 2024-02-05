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

################################################################################
# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/healthy_subjects_for_bins")


#Load data file
df_4bar_grad <- read.csv("./cf_agematched_spiral/snr_srm_fit_figs_params/srm_fit_parameters.csv")

None_slope <- df_4bar_grad$Slope[df_4bar_grad$Correction == "None"]
N4_slope <- df_4bar_grad$Slope[df_4bar_grad$Correction == "N4"]
FA_slope <- df_4bar_grad$Slope[df_4bar_grad$Correction == "FA"]

################################################################### Normality tests

shapiro.test(None_slope)
ggqqplot(None_slope, ylab='Non corrected Slope')

shapiro.test(N4_slope)
ggqqplot(N4_slope, ylab='N4 corrected Slope')

shapiro.test(FA_slope)
ggqqplot(FA_slope, ylab='FA corrected Slope')








ggdensity(cartesian_snr)
#Create a normal probability plot
qqnorm(cartesian_snr)
qqline(cartesian_snr, col = 2)  # Adding a reference line

##empirical cumulative distribution (ECDF)
ggplot(common_subjs_Non_corr, aes(snr, colour = Sequence)) + stat_ecdf() +
  stat_function(fun = function(x) pnorm(x, mean = mean(common_subjs_Non_corr$snr), sd = sd(common_subjs_Non_corr$snr)),
                geom = "step", col = 'blue', lwd = 1) +
        labs(x = "SNR",
              y = "Cumulative Probability")
