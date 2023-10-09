#If needed, install library/package
#install.packages("ggpubr")

#Load library
library(ggpubr)
library(tidyverse)
library(rstatix)


cincy_col = c( "#cc64a4", "#8fc541", "#6ac3e2", "#e49fc4", "#e4487e", "#8fc541", "#d69cb7", "#9bd2da", "#cc64a4")

#Load data file (totNorm_lung_origBoxPlot.csv): GRE vs Spiral
df_4bxp_SNR <- read.csv(file.choose())
##Mean Signal
compare_means(Mean_Signal ~ Sequence, data = df_4bxp_SNR, method = "wilcox.test", paired = TRUE)
p <- ggboxplot(df_4bxp_SNR, x = "Sequence", y = "Mean_Signal",
               shape = 19,size=1, ylim = c(0, 1),
               ylab = "Mean Signal (arb. units)", xlab = "Sequence",
               fill = "Sequence", palette = "cincy_col", add = "jitter",
               add.params = list(color = "black", size=3),
               bxp.errorbar = TRUE,bxp.errorbar.width = 0.5, legend = "none",
               font.x = c(22, "bold", "black"),
               font.y = c(22, "bold", "black"),
               font.tickslab = c(22, "bold", "black"),
               caption="Wilcoxon signed rank test") +
               stat_compare_means(method = "wilcox.test", paired = TRUE,label.y = 0.95,
                         aes(label = paste0("P = ", ..p.format..)), size=8) +
               font("caption", size = 12, color = "gray", face = "bold.italic")

p + theme(axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none") +
  theme()
###Background std
compare_means(Background_Std ~ Sequence, data = df_4bxp_SNR, method = "wilcox.test", paired = TRUE)
p <- ggboxplot(df_4bxp_SNR, x = "Sequence", y = "Background_Std",
               shape = 19,size=1, ylim = c(0, 0.02),
               ylab = "Background SD (arb. units)", xlab = "Sequence",
               fill = "Sequence", palette = "cincy_col", add = "jitter",
               add.params = list(color = "black", size=3),
               bxp.errorbar = TRUE,bxp.errorbar.width = 0.5, legend = "none",
               font.x = c(22, "bold", "black"),
               font.y = c(22, "bold", "black"),
               font.tickslab = c(22, "bold", "black"),
               caption="Wilcoxon signed rank test") +
  stat_compare_means(method = "wilcox.test", paired = TRUE,label.y = 0.019,
                     aes(label = paste0("P = ", ..p.format..)), size=8) +
  font("caption", size = 12, color = "gray", face = "bold.italic")

p + theme(axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none") +
  theme()
###SNR
compare_means(SNR ~ Sequence, data = df_4bxp_SNR, method = "wilcox.test", paired = TRUE)
p <- ggboxplot(df_4bxp_SNR, x = "Sequence", y = "SNR",
               shape = 19,size=1, ylim = c(0, 100),
               ylab = "SNR", xlab = "Sequence",
               fill = "Sequence", palette = "cincy_col", add = "jitter",
               add.params = list(color = "black", size=3),
               bxp.errorbar = TRUE,bxp.errorbar.width = 0.5, legend = "none",
               font.x = c(22, "bold", "black"),
               font.y = c(22, "bold", "black"),
               font.tickslab = c(22, "bold", "black"),
               caption="Wilcoxon signed rank test") +
  stat_compare_means(method = "wilcox.test", paired = TRUE,label.y = 95,
                     aes(label = paste0("P = ", ..p.format..)), size=8) +
  font("caption", size = 12, color = "gray", face = "bold.italic")

p + theme(axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none") +
  theme()
### 
# #Load data file (CFBronc_vdp_boxplot_pval.csv)
# vdp_bxp <- read.csv(file.choose())
# compare_means(vdp_percent ~ sequence, data = vdp_bxp, group.by = "Correction", method = "wilcox.test", paired = TRUE)
# p <- ggboxplot(vdp_bxp, x = "sequence", y = "vdp_percent",
#                shape = 19,size=1, ylim = c(0, 35),
#                ylab = "VDP (%)", xlab = "Sequence",
#                fill = "sequence", palette = "cincy_col", add = "jitter",bxp.errorbar = TRUE,
#                bxp.errorbar.width = 0.5, legend = "none", caption="Wilcoxon signed rank test",
#                facet.by = "Correction", short.panel.labs = FALSE) +
#   stat_compare_means(method = "wilcox.test", paired = TRUE,aes(label = paste0("P = ", ..p.format..)), size=6)
# 
# p + theme(legend.position="none") +
#   theme(text = element_text(size = 18))

# # Choose data file (CFBronc_vdp_boxplot_pval.csv)
# vdp_data <- read.csv(file.choose())
# 
# ##Plotting for the original vdp: GRE vs Spiral
# bxp <- ggboxplot(
#   vdp_data, x = "sequence", y = "orig", 
#   ylab = "VDP (%)", xlab = "Sequence", color = "sequence", palette = "jco",
#   add = "jitter"
# )
# stat.test <- vdp_data  %>%
#   wilcox_test(orig ~ sequence, paired = TRUE) %>%
#   add_significance()
# stat.test
# 
# stat.test <- stat.test %>% add_xy_position(x = "sequence")
# bxp + 
#   stat_pvalue_manual(stat.test, tip.length = 0) +
#   labs(subtitle = get_test_label(stat.test, detailed= TRUE))
# ##Plotting for the N4 corrected vdp: GRE vs Spiral
# bxp <- ggboxplot(
#   vdp_data, x = "sequence", y = "N4_corr", 
#   ylab = "N4 corr VDP (%)", xlab = "Sequence", color = "sequence", palette = "jco",
#   add = "jitter"
# )
# stat.test <- vdp_data  %>%
#   wilcox_test(orig ~ sequence, paired = TRUE) %>%
#   add_significance()
# stat.test <- stat.test %>% add_xy_position(x = "sequence")
# bxp + 
#   stat_pvalue_manual(stat.test, tip.length = 0) +
#   labs(subtitle = get_test_label(stat.test, detailed= TRUE))

