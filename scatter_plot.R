#load libraries
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)
library(rlang)
library(PMCMRplus)

# Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/gex_analysis")

##Colorblind friendly palette:
cbPalette <- c("#888888", "#CC6677", "#882255", "#332288", "#6699CC", "#DDCC77",
               "#999933", "#AA4499", "#661100", "#44AA99", "#117733", "#88CCEE")

## Define functions
scatter_plt <- function(data, x_data, y_data, group,
                        pt_size = 6,
                        pt_clr = c('#000000', '#666666'),
                        xtitle = NULL, ytitle = NULL,
                        x_limits = c(0, 50), y_limits = c(0, 10),
                        lgd_pos = "right",              # use "none" to hide legend
                        group_labels = c("Female", "Male"), # 1=female, 2=male
                        stats_pos = NULL,
                        digits = 2) 
{
      ## Compute and print mean/sd (overall y_data)
      y_quo <- rlang::enquo(y_data)
      x_quo <- rlang::enquo(x_data)
      g_quo <- rlang::enquo(group)
      y_vals <- data[[rlang::as_name(y_quo)]]
      overall_mean <- mean(y_vals, na.rm = TRUE)
      overall_sd   <- stats::sd(y_vals, na.rm = TRUE)
      cat("Overall Mean ± SD of", rlang::as_name(y_quo), ":\n")
      cat(sprintf("  Mean = %.*f, SD = %.*f\n\n", digits, overall_mean, digits, overall_sd))

  scatter_ <- ggplot2::ggplot(
    data = data, aes(x = {{ x_data }}, y = {{ y_data }}, color = factor({{ group }}))
    ) +
    geom_point(size = pt_size) + labs(x = xtitle, y = ytitle, color = NULL) +
    theme_bw() +
    theme(axis.text  = element_text(size = 22, color = "#000000", face = "bold"),
      axis.title = element_text(size = 22, color = "#000000", face = "bold"),
      legend.title = element_blank(),
      legend.position = lgd_pos,
      legend.background = element_blank(),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      legend.margin = margin(t=-5, r=2, b=0, l=2),
      legend.text = element_text(size = 12, face = "bold")
    ) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE)
  
  # If the group has exactly 2 levels, apply colors and (optional) custom labels
  group_vals <- unique(stats::na.omit(scatter_$data[[rlang::as_name(rlang::enquo(group))]]))
  if (length(group_vals) == 2) {
    scatter_ <- scatter_ + ggplot2::scale_color_manual(values = pt_clr, labels = group_labels)
  } else {
    scatter_ <- scatter_ + ggplot2::scale_color_manual(values = pt_clr)
  }
  
  ##Add "Mean ± SD" annotation on the plot with default position: top-left
  if (is.null(stats_pos)) {
    xr <- diff(range(x_limits))
    yr <- diff(range(y_limits))
    ann_x <- x_limits[1] + 0.05 * xr
    ann_y <- y_limits[2] - 0.05 * yr
  } else {
    ann_x <- stats_pos[1]
    ann_y <- stats_pos[2]
  }
  ann_text <- sprintf("Mean ± SD: %.*f ± %.*f", digits, overall_mean, digits, overall_sd)
  scatter_ <- scatter_ +
    ggplot2::annotate("text", x = ann_x, y = ann_y,
      label = ann_text, hjust = 0, vjust = 1,
      size = 4, fontface = "bold", color = "black"
    )

  print(scatter_)
  return(scatter_)
}


################################################################################
# ##Load the data from both CSV files and arrange it to plot
healthy_kernel_11 <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_kernel-11.csv")
healthy_kernel_2 <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_kernel-2.csv")
healthy_kernel_divMean <- read.csv("./healthy_all/results15Feb2026/healthy_gx_osc_stats_divMEAN.csv")



healthy_k11 <- scatter_plt(healthy_kernel_11, subject_age, osc_mean, subject_sex,
                           pt_size = 3, pt_clr = c('#cc79a7', '#0072b2'),
                           xtitle = expression(bold("Age (years)")),
                           ytitle = expression(bold("Mean RBC Osc (%)")),
                           x_limits = c(0, 85), y_limits = c(0, 11),
                           lgd_pos = c(.85,.9), group_labels = c("Female", "Male")
)

healthy_k2 <- scatter_plt(healthy_kernel_2, subject_age, osc_mean, subject_sex,
                           pt_size = 3, pt_clr = c('#cc79a7', '#0072b2'),
                           xtitle = expression(bold("Age (years)")),
                           ytitle = expression(bold("Mean RBC Osc (%)")),
                           x_limits = c(0, 85), y_limits = c(0, 11),
                           lgd_pos = c(.85,.9), group_labels = c("Female", "Male")
)

healthy_divM <- scatter_plt(healthy_kernel_divMean, subject_age, osc_mean, subject_sex,
                          pt_size = 3, pt_clr = c('#cc79a7', '#0072b2'),
                          xtitle = expression(bold("Age (years)")),
                          ytitle = expression(bold("Mean RBC Osc (%)")),
                          x_limits = c(0, 85), y_limits = c(0, 11),
                          lgd_pos = c(.85,.9), group_labels = c("Female", "Male")
)

ggsave("./healthy_all/zR_plots/age_vs_osc_mean_k11.png", plot = healthy_k11, width = 4.9, height = 3.5, dpi = 300)

ggsave("./healthy_all/zR_plots/age_vs_osc_mean_k2.png", plot = healthy_k2, width = 4.9, height = 3.5, dpi = 300)

ggsave("./healthy_all/zR_plots/age_vs_osc_mean_divMean.png", plot = healthy_divM, width = 4.9, height = 3.5, dpi = 300)

