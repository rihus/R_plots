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
# ##Load the data from both CSV files
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

################################################################### Normality tests
##Extract SNR values for Cartesian and Spiral sequences
cartesian_snr <- common_subjs_Non_corr$snr[common_subjs_Non_corr$Sequence == "Cartesian"]
spiral_snr <- common_subjs_Non_corr$snr[common_subjs_Non_corr$Sequence == "Spiral"]

cartesian_msig <- common_subjs_Non_corr$mean_signal[common_subjs_Non_corr$Sequence == "Cartesian"]
spiral_msig <- common_subjs_Non_corr$mean_signal[common_subjs_Non_corr$Sequence == "Spiral"]

cartesian_bksd <- common_subjs_Non_corr$bkgd_sd[common_subjs_Non_corr$Sequence == "Cartesian"]
spiral_bksd <- common_subjs_Non_corr$bkgd_sd[common_subjs_Non_corr$Sequence == "Spiral"]

shapiro.test(cartesian_snr)
ggqqplot(cartesian_snr, ylab='Cartesian snr')
shapiro.test(spiral_snr)
ggqqplot(spiral_snr, ylab='Spiral snr')

shapiro.test(cartesian_msig)
ggqqplot(cartesian_msig, ylab='Cartesian mean signal')
shapiro.test(spiral_msig)
ggqqplot(spiral_msig, ylab='Spiral mean signal')

shapiro.test(cartesian_bksd)
ggqqplot(cartesian_bksd, ylab='Cartesian background noise')
shapiro.test(spiral_bksd)
ggqqplot(spiral_bksd, ylab='Spiral background noise')








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
