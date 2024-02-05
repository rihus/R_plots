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
# ##Load the data from both CSV files and arrange it to plot
N4_corr_cart_cf <- read.csv("./IRC740H_2Dcartesian_CF/snr_vdp_hvp_analysis_results/N4_corr_percentile_analysis_results.csv")
N4_corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/snr_vdp_hvp_analysis_results/N4_corr_percentile_analysis_results - Copy.csv")

# #Create a combined data frame with an indicator for Sequence and Category
combined_N4_thresh <- rbind(transform(N4_corr_cart_cf, Sequence = "Cartesian", Category = "CF"),
                            transform(N4_corr_spir_cf, Sequence = "Spiral", Category = "CF"))
# #Create a new data frame selecting shared subjects for connected box plots
common_subjs <- combined_N4_thresh$Subject_id[duplicated(combined_N4_thresh$Subject_id) | 
                                                duplicated(combined_N4_thresh$Subject_id, fromLast = TRUE)]
common_subjs_N4_thresh <- combined_N4_thresh[combined_N4_thresh$Subject_id %in% common_subjs, , drop = FALSE]

new_df <- data.frame(
  cart_vdp = common_subjs_N4_thresh$VDP[common_subjs_N4_thresh$Sequence == "Cartesian"],
  cart_hvp = common_subjs_N4_thresh$HyperP[common_subjs_N4_thresh$Sequence == "Cartesian"],
  spiral_vdp = common_subjs_N4_thresh$VDP[common_subjs_N4_thresh$Sequence == "Spiral"],
  spiral_hvp = common_subjs_N4_thresh$HyperP[common_subjs_N4_thresh$Sequence == "Spiral"],
  Category = common_subjs_N4_thresh$Category[common_subjs_N4_thresh$Sequence == "Cartesian"]
)
################################################################### Normality tests

shapiro.test(new_df$cart_vdp)
ggqqplot(new_df$cart_vdp, ylab='Cartesian VDP')
shapiro.test(new_df$spiral_vdp)
ggqqplot(new_df$spiral_vdp, ylab='Spiral VDP')

shapiro.test(new_df$cart_hvp)
ggqqplot(new_df$cart_hvp, ylab='Cartesian HVP')
shapiro.test(new_df$spiral_hvp)
ggqqplot(new_df$spiral_hvp, ylab='Spiral HVP')








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
