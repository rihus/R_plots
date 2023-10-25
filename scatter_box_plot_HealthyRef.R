# Load the ggplot2 library
library(ggplot2)
library(ggpubr)

# Define the path to the directory containing data file
data_directory <- "C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/human_data/VDP_analysis/healthy_subjects"
# Set the working directory to the data directory
setwd(data_directory)

################################################################################
# Read the CSV file for VDP 60% data
vdp_file_name <- "N4_vdp60_all_data.csv"
vdp_data <- read.csv(vdp_file_name)

if (!("Age" %in% colnames(vdp_data)) || !("VDP" %in% colnames(vdp_data))) {
  print("The CSV file must have 'Age' and 'VDP' columns.")
} else {
  p60 <- ggplot(vdp_data, aes(x = Age, y = VDP)) +
    geom_point(aes(shape = Sex, color = Sex), size = 4) +
    theme_bw(base_size = 16) +
    xlim(5, 82) +
    labs(
      x = "Age (years)",
      y = "VDP (%)") +
    theme(
      text = element_text(size = 20, face = 'bold'),
      axis.text.x = element_text(face = "bold", color = "black", size = 20),
      axis.text.y = element_text(face = "bold", color = "black", size = 20)
    )
  
  # Add a linear regression line (all data - ignore color i.e. Sex here)
  p60 <- p60 + geom_smooth(data = subset(vdp_data, !is.na(VDP)), aes(group = 1), method = "lm",
                           linewidth = 2, show.legend = FALSE, se = TRUE)
  #to add separate line based on color/sex
  #p60 + geom_smooth(method = "lm", se = TRUE, linewidth=1, aes(group = Sex,color = Sex))
  
  # Calculate the coefficients for the linear regression model
  lm_model <- lm(VDP ~ Age, data = vdp_data)
  intercept <- coef(lm_model)[1]
  slope <- coef(lm_model)[2]
  
  # Add the equation to the plot
  p60 <- p60 + geom_text(aes(x = 30, y = 14, label = paste("y=",round(slope, 2),"x","+ (", round(intercept, 2),")")),
                     size = 6, color = "black")
  
  # Print the plot
  print(p60)
}

# Calculate the coefficients for the linear regression model
lm_model <- lm(VDP ~ Age, data = vdp_data)
print(lm_model)

# Create a jittered boxplot comparing VDP between Males (M) and Females (F): VDP 60%
p_jitter <- ggboxplot(vdp_data, x = "Sex", y = "VDP",
                      shape = 19, size = 1, 
                      ylab = "VDP (%)", xlab = "Sex",
                      fill = "Sex", palette = c("#FF6700", "#0D98BA"),
                      add = "jitter",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.5,
                      legend = "none",
                      font.x = c(22, "bold", "black"),
                      font.y = c(22, "bold", "black"),
                      font.tickslab = c(22, "bold", "black")) +
  scale_fill_manual(values = c("#E69F00", "#63AAC0")) +
  stat_compare_means(method = "wilcox.test", paired = FALSE, label.y = 14.1,
                     aes(label = paste0("P = ", ..p.format..)), size = 8)

# # Add p-value annotation using geom_signif()
# p_jitter <- p_jitter +
#   geom_signif(comparisons = list(c("M", "F")), annotations = "p.format", 
#               y_position = 14, textsize = 8)

p_jitter <- p_jitter + theme(axis.line = element_line(colour = "black"),
                             panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
                             legend.position = "none") +
  theme()

# Print the plot
print(p_jitter)

################################################################################
# Read the CSV file for 99% percentile data
percentile_file_name <- "N4_percentile_all_data.csv"
percentile_data <- read.csv(percentile_file_name)
# Check if the 'Age' and 'DefectP' columns exist in the data
if (!("Age" %in% colnames(percentile_data)) || !("DefectP" %in% colnames(percentile_data))) {
  print("The CSV file must have 'Age' and 'DefectP' columns.")
} else {
  # Create a scatter plot with 'Age' and 'DefectP', using 'Sex' for colors
  p <- ggplot(percentile_data, aes(x = Age, y = DefectP)) +
    geom_point(aes(shape = Sex, color = Sex), size=4) +  # control marker size
    theme_bw(base_size = 16) + xlim(5, 82) + # ylim(-1, 15) +
    labs(
      x = "Age (years)",
      y = "Defect (%)") + # title = "Age vs Defects Plot",
    theme(text = element_text(size = 20, face = 'bold'),
          axis.text.x = element_text(face="bold", color="black", size=20),
          axis.text.y = element_text(face="bold", color="black", size=20))
}
# Add a linear regression line (all data - ignore color i.e. Sex here)
p + geom_smooth(data = subset(percentile_data, !is.na(DefectP)), aes(group = 1), method = "lm",
                size=2, show.legend = FALSE, se = TRUE)
#to add separate line colored based on sex
#p + geom_smooth(method = "lm", se = TRUE, size=1, aes(group = Sex,color = Sex))

# Calculate the coefficients for the linear regression model
lm_model <- lm(DefectP ~ Age, data = percentile_data)
print(lm_model)

# Create a jittered boxplot comparing VDP between Males (M) and Females (F): 99th percentile
p_jitter <- ggboxplot(percentile_data, x = "Sex", y = "DefectP",
                      shape = 19, size = 1, 
                      ylab = "Defects (%)", xlab = "Sex",
                      fill = "Sex", palette = c("#FF6700", "#0D98BA"),
                      add = "jitter",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.5,
                      legend = "none",
                      font.x = c(22, "bold", "black"),
                      font.y = c(22, "bold", "black"),
                      font.tickslab = c(22, "bold", "black")) +
  scale_fill_manual(values = c("#E69F00", "#63AAC0")) +
  stat_compare_means(method = "wilcox.test", paired = FALSE, label.y = 32.1,
                     aes(label = paste0("P = ", ..p.format..)), size = 8)

# # Add p-value annotation using geom_signif()
# p_jitter <- p_jitter +
#   geom_signif(comparisons = list(c("M", "F")), annotations = "p.format", 
#               y_position = 14, textsize = 8)

p_jitter <- p_jitter + theme(axis.line = element_line(colour = "black"),
                             panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
                             legend.position = "none") +
  theme()

# Print the plot
print(p_jitter)

################################################################################
# Read the CSV file for median normalized data
median_file_name <- "N4_median_all_data.csv"
median_data <- read.csv(median_file_name)

# Check if the 'Age' and 'DefectP' columns exist in the data
if (!("Age" %in% colnames(median_data)) || !("DefectP" %in% colnames(median_data))) {
  print("The CSV file must have 'Age' and 'DefectP' columns.")
} else {
  # Create a scatter plot with 'Age' and 'DefectP', using 'Sex' for colors
  p <- ggplot(median_data, aes(x = Age, y = DefectP)) + # , color = Sex
    geom_point(aes(shape = Sex, color = Sex), size=4) +
    theme_bw(base_size = 16) + xlim(5, 82) +
    labs(
      x = "Age (years)",
      y = "Defect (%)") + # title = "Age vs Defects Plot",
    theme(text = element_text(size = 20, face = 'bold'),
          axis.text.x = element_text(face="bold", color="black", size=20),
          axis.text.y = element_text(face="bold", color="black", size=20))
}
# Add a linear regression line (all data - ignore color i.e. Sex here)
p + geom_smooth(data = subset(median_data, !is.na(DefectP)), aes(group = 1), method = "lm",
                size=2, show.legend = FALSE, se = TRUE)
#to add separate line based on color
p + geom_smooth(method = "lm", se = TRUE, size=1, aes(group = Sex,color = Sex))

# Calculate the coefficients for the linear regression model
lm_model <- lm(DefectP ~ Age, data = median_data)
print(lm_model)

# Create a jittered boxplot comparing VDP between Males (M) and Females (F): median normalized
p_jitter <- ggboxplot(median_data, x = "Sex", y = "DefectP",
                      shape = 19, size = 1, 
                      ylab = "Defects (%)", xlab = "Sex",
                      fill = "Sex", palette = c("#FF6700", "#0D98BA"),
                      add = "jitter",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.5,
                      legend = "none",
                      font.x = c(22, "bold", "black"),
                      font.y = c(22, "bold", "black"),
                      font.tickslab = c(22, "bold", "black")) +
  scale_fill_manual(values = c("#E69F00", "#63AAC0")) +
  stat_compare_means(method = "t.test", paired = FALSE, label.y = 16.1,
                     aes(label = paste0("P = ", ..p.format..)), size = 8)

# # Add p-value annotation using geom_signif()
# p_jitter <- p_jitter +
#   geom_signif(comparisons = list(c("M", "F")), annotations = "p.format", 
#               y_position = 14, textsize = 8)

p_jitter <- p_jitter + theme(axis.line = element_line(colour = "black"),
                             panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
                             legend.position = "none") +
  theme()

# Print the plot
print(p_jitter)

################################################################################
# Read the CSV file for GAMLSS data (distribution generated)
gamlss_file_name <- "N4_gamlss_all_data.csv"
gamlss_data <- read.csv(gamlss_file_name)

# Check if the 'Age' and 'DefectP' columns exist in the data
if (!("Age" %in% colnames(gamlss_data)) || !("DefectP" %in% colnames(gamlss_data))) {
  print("The CSV file must have 'Age' and 'DefectP' columns.")
} else {
  # Create a scatter plot with 'Age' and 'DefectP', using 'Sex' for colors
  p <- ggplot(gamlss_data, aes(x = Age, y = DefectP)) + # , color = Sex
    geom_point(aes(shape = Sex, color = Sex), size=4) +
    theme_bw(base_size = 16) + xlim(5, 82) + ylim(-5, 21) +
    labs(
      x = "Age (years)",
      y = "Defect (%)") + # title = "Age vs Defects Plot",
    theme(text = element_text(size = 20, face = 'bold'),
          axis.text.x = element_text(face="bold", color="black", size=20),
          axis.text.y = element_text(face="bold", color="black", size=20))
}
# Add a linear regression line (all data - ignore color i.e. Sex here)
p + geom_smooth(data = subset(gamlss_data, !is.na(DefectP)), aes(group = 1), method = "lm",
                size=2, show.legend = FALSE, se = TRUE)
#to add separate line based on color
#p + geom_smooth(method = "lm", se = TRUE, size=1, aes(group = Sex,color = Sex))

# Calculate the coefficients for the linear regression model
lm_model <- lm(DefectP ~ Age, data = gamlss_data)
print(lm_model)

# Create a jittered boxplot comparing VDP between Males (M) and Females (F): median normalized
p_jitter <- ggboxplot(gamlss_data, x = "Sex", y = "DefectP",
                      shape = 19, size = 1, 
                      ylab = "Defects (%)", xlab = "Sex",
                      fill = "Sex", palette = c("#FF6700", "#0D98BA"),
                      add = "jitter",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.5,
                      legend = "none",
                      font.x = c(22, "bold", "black"),
                      font.y = c(22, "bold", "black"),
                      font.tickslab = c(22, "bold", "black")) +
  scale_fill_manual(values = c("#E69F00", "#63AAC0")) +
  stat_compare_means(method = "t.test", paired = FALSE, label.y = 7.1,
                     aes(label = paste0("P = ", ..p.format..)), size = 8)

# # Add p-value annotation using geom_signif()
# p_jitter <- p_jitter +
#   geom_signif(comparisons = list(c("M", "F")), annotations = "p.format", 
#               y_position = 14, textsize = 8)

p_jitter <- p_jitter + theme(axis.line = element_line(colour = "black"),
                             panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
                             legend.position = "none") +
  theme()

# Print the plot
print(p_jitter)

################################################################################
# Read the CSV file for GAMLSS data (distribution generated)
glb_file_name <- "N4_glb_all_data.csv"
glb_data <- read.csv(glb_file_name)

# Check if the 'Age' and 'DefectP' columns exist in the data
if (!("Age" %in% colnames(glb_data)) || !("DefectP" %in% colnames(glb_data))) {
  print("The CSV file must have 'Age' and 'DefectP' columns.")
} else {
  # Create a scatter plot with 'Age' and 'DefectP', using 'Sex' for colors
  p <- ggplot(glb_data, aes(x = Age, y = DefectP)) + # , color = Sex
    geom_point(aes(shape = Sex, color = Sex), size=4) +
    theme_bw(base_size = 16) + xlim(5, 82) + ylim(-5, 21) +
    labs(
      x = "Age (years)",
      y = "Defect (%)") + # title = "Age vs Defects Plot",
    theme(text = element_text(size = 20, face = 'bold'),
          axis.text.x = element_text(face="bold", color="black", size=20),
          axis.text.y = element_text(face="bold", color="black", size=20))
}
# Add a linear regression line (all data - ignore color i.e. Sex here)
p + geom_smooth(data = subset(glb_data, !is.na(DefectP)), aes(group = 1), method = "lm",
                size=2, show.legend = FALSE, se = TRUE)
#to add separate line based on color
#p + geom_smooth(method = "lm", se = TRUE, size=1, aes(group = Sex,color = Sex))

# Calculate the coefficients for the linear regression model
lm_model <- lm(DefectP ~ Age, data = glb_data)
print(lm_model)

# Create a jittered boxplot comparing VDP between Males (M) and Females (F): median normalized
p_jitter <- ggboxplot(glb_data, x = "Sex", y = "DefectP",
                      shape = 19, size = 1, 
                      ylab = "Defects (%)", xlab = "Sex",
                      fill = "Sex", palette = c("#FF6700", "#0D98BA"),
                      add = "jitter",
                      bxp.errorbar = TRUE, bxp.errorbar.width = 0.5,
                      legend = "none",
                      font.x = c(22, "bold", "black"),
                      font.y = c(22, "bold", "black"),
                      font.tickslab = c(22, "bold", "black")) +
  scale_fill_manual(values = c("#E69F00", "#63AAC0")) +
  stat_compare_means(method = "t.test", paired = FALSE, label.y = 16.1,
                     aes(label = paste0("P = ", ..p.format..)), size = 8)

# # Add p-value annotation using geom_signif()
# p_jitter <- p_jitter +
#   geom_signif(comparisons = list(c("M", "F")), annotations = "p.format", 
#               y_position = 14, textsize = 8)

p_jitter <- p_jitter + theme(axis.line = element_line(colour = "black"),
                             panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
                             legend.position = "none") +
  theme()

# Print the plot
print(p_jitter)
