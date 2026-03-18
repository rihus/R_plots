#load libraries
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)
library(PMCMRplus)

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/gex_analysis")

##Colorblind friendly palette:
cbPalette <- c("#888888", "#CC6677", "#882255", "#332288", "#6699CC", "#DDCC77",
               "#999933", "#AA4499", "#661100", "#44AA99", "#117733", "#88CCEE")

##Define functions
boxplt <- function(df_in, x_in, y_in, ylabel=NULL, xlabel="", ylimit=NULL,
                   bxfill = "white", bxpallet=NULL, pt_clrs=c("#000000", "#009e73"),
                   stat_tst="wilcox", paired=FALSE, py_pos=NULL, addp_eq=FALSE){
  require(ggpubr)
  # Check if ylabel, ylimit and py_pos are provided
  if (is.null(ylabel) || is.null(ylimit) || is.null(py_pos)){
    ylabel <- y_in
    ylimit <- c(0, 1.5 * max(df_in[[y_in]], na.rm = TRUE))
    py_pos <- 1.35 * max(df_in[[y_in]], na.rm = TRUE) }
  ##Paste x, y inputs for statistical tests
  yx_formula <- as.formula(paste(y_in, "~", x_in))
  mean_val <- aggregate(yx_formula, df_in, mean)
  print("Mean:")
  print(mean_val)
  sd_val <- aggregate(yx_formula, df_in, sd)
  print("Standard-deviation:")
  print(sd_val)
  range_val <- aggregate(yx_formula, df_in, function(x) {
    paste0("[", min(x, na.rm = TRUE), ", ", max(x, na.rm = TRUE), "]")
  })
  print("Range:")
  print(range_val)
  
  ## Statistical test
  pval <- NULL
  if (stat_tst == "wilcox") {
    pval <- df_in %>%
      wilcox_test(yx_formula, paired = paired) %>%
      adjust_pvalue(method = 'bonferroni') %>%
      add_significance()
    print(pval)
  } else if (stat_tst == "ttest") {
    pval <- df_in %>%
      t_test(yx_formula, paired = paired) %>%
      adjust_pvalue(method = 'bonferroni') %>%
      add_significance()
    print(pval)
  } else {
    stop("For stat_tst, only Wilcoxon (wilcox) or T-test (ttest) are accepted")
  }
  ##Print adjusted p-value if test is paired, otherwise p
  #if (paired) {plabel= "P = {p.adj}"} else {plabel= "P = {p}"}
  ##Unconnected box-plot 
  bxp_ <- ggboxplot(df_in, x = x_in, y = y_in,
                    ylim = ylimit, fill = bxfill, pallete = bxpallet,
                    outlier.shape = NA,
                    font.x = c(22, "bold", "#000000"),
                    font.y = c(24, "bold", "#000000"), 
                    font.tickslab = c(22, "bold", "#000000")) +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_jitter(aes(color = as.factor(df_in[[x_in]])), width=0.2, size=2, alpha=1) +
    scale_color_manual(values = pt_clrs) +
    theme(legend.position = "none")
  bxp_ <- bxp_ + geom_vline(xintercept = Inf, linetype = "solid")
  bxp_ <- bxp_ + geom_hline(yintercept = Inf, linetype = "solid")
  ##Add p-value on the plot
  pval <- pval %>% add_xy_position(x = x_in)
  if (addp_eq == TRUE) {plabel = "P={scales::pvalue(p, accuracy = 0.001)}"}
  else {plabel = "P{scales::pvalue(p, accuracy = 0.001)}"}
  bxp_p <-  bxp_ + stat_pvalue_manual(pval, label = plabel, #"P = {p.adj}"
                                      y.position=py_pos, label.size = 8,
                                      bracket.size = 0.8, tip.length = 0.025,
                                      vjust=-0.25)
  print(bxp_p)
  return(list(bxp_, bxp_p))
}

################################################################################
# ##Load the data from both CSV files and arrange it to plot
healthy_kernel_11 <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_kernel-11.csv")
healthy_kernel_2 <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_kernel-2.csv")
healthy_kernel_divMean <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_divMEAN.csv")


healthy_k11 <- boxplt(healthy_kernel_11, "subject_sex", "osc_mean", ylabel="Mean RBC Osc (%)",
                  xlabel="", ylimit=c(0, 10), bxfill= "white", bxpallet=NULL,
                  pt_clrs= c("pink", "skyblue"), stat_tst="wilcox", paired=FALSE,
                  py_pos=9, addp_eq=TRUE)

healthy_k2 <- boxplt(healthy_kernel_2, "subject_sex", "osc_mean", ylabel="Mean RBC Osc (%)",
                      xlabel="", ylimit=c(0, 10), bxfill= "white", bxpallet=NULL,
                      pt_clrs= c("pink", "skyblue"), stat_tst="wilcox", paired=FALSE,
                      py_pos=9, addp_eq=TRUE)

healthy_divM <- boxplt(healthy_kernel_divMean, "subject_sex", "osc_mean", ylabel="Mean RBC Osc (%)",
                      xlabel="", ylimit=c(0, 10), bxfill= "white", bxpallet=NULL,
                      pt_clrs= c("pink", "skyblue"), stat_tst="wilcox", paired=FALSE,
                      py_pos=9, addp_eq=TRUE)

# #Save the plot as a png file in the specified directory
ggsave("./healthy_all/zR_plots/rbc_osc_mean_kernel-11_plain.png", plot = bxp_osc, width = 4.5, height = 3.7, dpi = 300)
ggsave("./healthy_all/zR_plots/rbc_osc_mean_kernel-11_p.png", plot = bxp_osc_p, width = 4.5, height = 3.7, dpi = 300)
