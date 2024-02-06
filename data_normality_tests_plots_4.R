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
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch")

################################################################################
# ##Load the data from both CSV files and arrange it to plot
Non_corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/snr_vdp_hvp_analysis_results/Non_corr_percentile_analysis_results.csv")
N4_corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/snr_vdp_hvp_analysis_results/N4_corr_percentile_analysis_results.csv")
FA_corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/snr_vdp_hvp_analysis_results/FA_corr_percentile_analysis_results.csv")

# ##Create a combined data frame with an indicator for Correction and Category
combined_prcntl <- rbind(transform(FA_corr_spir_cf, Correction = "FA"),
                         transform(N4_corr_spir_cf, Correction = "N4"),
                         transform(Non_corr_spir_cf, Correction = "None"))

# #Create a new data frame selecting shared subjects for connected box plots
common_subjs <- combined_prcntl$Subject_id[duplicated(combined_prcntl$Subject_id) | 
                                             duplicated(combined_prcntl$Subject_id, fromLast = TRUE)]
common_subjs_prcntl <- combined_prcntl[combined_prcntl$Subject_id %in% common_subjs, , drop = FALSE]

# combining both normal bins
common_subjs_prcntl <- common_subjs_prcntl %>%
  mutate(NVP = NormalP1 + NormalP2) %>%
  select(-NormalP1, -NormalP2)

# Reshaping the data for boxplot
df_long <- common_subjs_prcntl %>%
  tidyr::gather(key = "Parameter", value = "Value", -Correction, -Subject_id)

VDP_none <- df_long$Value[df_long$Parameter == "VDP" & df_long$Correction == "None"]
VDP_N4 <- df_long$Value[df_long$Parameter == "VDP" & df_long$Correction == "N4"]
VDP_FA <- df_long$Value[df_long$Parameter == "VDP" & df_long$Correction == "FA"]

LVP_none <- df_long$Value[df_long$Parameter == "LVP" & df_long$Correction == "None"]
LVP_N4 <- df_long$Value[df_long$Parameter == "LVP" & df_long$Correction == "N4"]
LVP_FA <- df_long$Value[df_long$Parameter == "LVP" & df_long$Correction == "FA"]

NVP_none <- df_long$Value[df_long$Parameter == "NVP" & df_long$Correction == "None"]
NVP_N4 <- df_long$Value[df_long$Parameter == "NVP" & df_long$Correction == "N4"]
NVP_FA <- df_long$Value[df_long$Parameter == "NVP" & df_long$Correction == "FA"]

EVP_none <- df_long$Value[df_long$Parameter == "EVP" & df_long$Correction == "None"]
EVP_N4 <- df_long$Value[df_long$Parameter == "EVP" & df_long$Correction == "N4"]
EVP_FA <- df_long$Value[df_long$Parameter == "EVP" & df_long$Correction == "FA"]

HVP_none <- df_long$Value[df_long$Parameter == "HVP" & df_long$Correction == "None"]
HVP_N4 <- df_long$Value[df_long$Parameter == "HVP" & df_long$Correction == "N4"]
HVP_FA <- df_long$Value[df_long$Parameter == "HVP" & df_long$Correction == "FA"]

################################################################### Normality tests

shapiro.test(VDP_none)
ggqqplot(VDP_none, ylab='Non corrected VDP')
shapiro.test(VDP_N4)
ggqqplot(VDP_N4, ylab='N4 corrected VDP')
shapiro.test(VDP_FA)
ggqqplot(VDP_FA, ylab='FA corrected VDP')

shapiro.test(LVP_none)
ggqqplot(LVP_none, ylab='Non corrected LVP')
shapiro.test(LVP_N4)
ggqqplot(LVP_N4, ylab='N4 corrected LVP')
shapiro.test(LVP_FA)
ggqqplot(LVP_FA, ylab='FA corrected LVP')

shapiro.test(NVP_none)
ggqqplot(NVP_none, ylab='Non corrected NVP')
shapiro.test(NVP_N4)
ggqqplot(NVP_N4, ylab='N4 corrected NVP')
shapiro.test(NVP_FA)
ggqqplot(NVP_FA, ylab='FA corrected NVP')

shapiro.test(EVP_none)
ggqqplot(EVP_none, ylab='Non corrected EVP')
shapiro.test(EVP_N4)
ggqqplot(EVP_N4, ylab='N4 corrected EVP')
shapiro.test(EVP_FA)
ggqqplot(EVP_FA, ylab='FA corrected EVP')

shapiro.test(HVP_none)
ggqqplot(HVP_none, ylab='Non corrected HVP')
shapiro.test(HVP_N4)
ggqqplot(HVP_N4, ylab='N4 corrected HVP')
shapiro.test(HVP_FA)
ggqqplot(HVP_FA, ylab='FA corrected HVP')




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
