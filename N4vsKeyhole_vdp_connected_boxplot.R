# Load the required libraries
#Load library
library(ggpubr)
library(tidyverse)
library(rstatix)
library(stats)
library(ggplot2)
library(dplyr)

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/human_data/VDP_analysis/CFNonCF_Bronch/IRC740H_2Dspiral_CF")


################################################################################ Connected boxplot
# ##Load the data from both CSV files - paired Wilcoxon plot VDP 60%
keyhole_vdp60 <- read.csv("keyhole_vdp60_analysis_results.csv")
N4_vdp60 <- read.csv("N4_vdp60_analysis_results.csv")

# Create a combined data frame with an indicator for the Correction of the data
keyhole_vdp60$Correction <- "FA"
N4_vdp60$Correction <- "N4"
vdp60_combined <- rbind(keyhole_vdp60, N4_vdp60)

##Statistical test (paired wilcoxon)
stat.test <- vdp60_combined %>%
  wilcox_test(VDP ~ Correction , paired = TRUE) %>%
  add_significance()
stat.test
# Box plots with p-values VDP 60% analysis
p_bxp60 <-  ggpaired(vdp60_combined, x = "Correction", y = "VDP", fill = "Correction",
                     palette = c("#095859", "#DB9C60"), width = 0.5, point.size = 2, point.color="Subject",
                     ylim = c(0, 33), line.color = "gray", line.size = 0.5,
                     legend = "none", ylab = "VDP (%)", xlab = "Correction Method") + #
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "black", face = "bold"),
        axis.title = element_text(size = 22, color = "black", face = "bold"))#,
        #axis.line.x = element_line(linewidth = 0.5), axis.line.y = element_line(linewidth = 0.5))
stat.test <- stat.test %>% add_xy_position(x = "Correction")
p_bxp60 <- p_bxp60 + stat_pvalue_manual(stat.test, label = "p.signif", y.position = 31.5,
                                        label.size = 8, bracket.size = 0.8, tip.length = 0.02)
##axis.line.x.top = element_line(linewidth = 2)
##
print(p_bxp60)
# Save the plot as a png file in the current directory
ggsave("Rplot_N4vsFA_vdp60_bxp_connected.png", plot = p_bxp60, width = 4.25, height = 3.35, dpi = 300)

#########################################
# ##Load the data from both CSV files - paired Wilcoxon plot 99th percentile
keyhole_99percentile <- read.csv("keyhole_percentile_analysis_results.csv")
N4_99percentile <- read.csv("N4_percentile_analysis_results.csv")
# Create a combined data frame with an indicator for the Correction of the data
keyhole_99percentile$Correction <- "FA"
N4_99percentile$Correction <- "N4"
percentile_combined <- rbind(keyhole_99percentile, N4_99percentile)
##Statistical test
stat.test <- percentile_combined %>%
  wilcox_test(DefectP ~ Correction , paired = TRUE) %>%
  add_significance()
stat.test
# Box plots with p-values VDP
p_bxp_prcntl <-  ggpaired(percentile_combined, x = "Correction", y = "DefectP", fill = "Correction",
                          palette = c("#095859", "#DB9C60"), width = 0.5, point.size = 2, point.color="Subject",
                          ylim = c(0, 33), line.color = "gray", line.size = 0.5,
                          legend = "none", ylab = "VDP (%)", xlab = "Correction Method") + #
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "black", face = "bold"),
        axis.title = element_text(size = 22, color = "black", face = "bold"))
stat.test <- stat.test %>% add_xy_position(x = "Correction")
p_bxp_prcntl <- p_bxp_prcntl + stat_pvalue_manual(stat.test, label = "p.signif", y.position = 31.5,
                                                  label.size = 8, bracket.size = 0.8, tip.length = 0.02)
##axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)
print(p_bxp_prcntl)
# Save the plot as a png file in the current directory
ggsave("Rplot_N4vsFA_vdpprcntl_bxp_connected.png", plot = p_bxp_prcntl, width = 4.25, height = 3.35, dpi = 300)
#########################################
# ##Load the data from both CSV files - paired Wilcoxon plot median normalized
keyhole_median <- read.csv("keyhole_median_analysis_results.csv")
N4_median <- read.csv("N4_median_analysis_results.csv")
# Create a combined data frame with an indicator for the Correction of the data
keyhole_median$Correction <- "FA"
N4_median$Correction <- "N4"
median_combined <- rbind(keyhole_median, N4_median)
##Statistical test
stat.test <- median_combined %>%
  wilcox_test(DefectP ~ Correction , paired = TRUE) %>%
  add_significance()
stat.test

# Box plots with p-values VDP
p_bxp_median <-  ggpaired(median_combined, x = "Correction", y = "DefectP", fill = "Correction",
                          palette = c("#095859", "#DB9C60"), width = 0.5, point.size = 2, point.color="Subject",
                          ylim = c(0, 33), line.color = "gray", line.size = 0.5,
                          legend = "none", ylab = "VDP (%)", xlab = "Correction Method") + #
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "black", face = "bold"),
        axis.title = element_text(size = 22, color = "black", face = "bold"))
stat.test <- stat.test %>% add_xy_position(x = "Correction")
p_bxp_median <- p_bxp_median + stat_pvalue_manual(stat.test, label = "p.signif", y.position = 31.5,
                                                  label.size = 8, bracket.size = 0.8, tip.length = 0.02)
##axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)
print(p_bxp_median)
# Save the plot as a png file in the current directory
ggsave("Rplot_N4vsFA_vdpmedian_bxp_connected.png", plot = p_bxp_median, width = 4.25, height = 3.35, dpi = 300)



