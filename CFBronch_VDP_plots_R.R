install.packages("dplyr")
install.packages("ggpubr")
install.packages("PairedData")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("tidyverse")

# Define the path to the directory containing data file
data_directory <- "C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/human_data/VDP_analysis/CFNonCF_Bronch/IRC740H_2Dspiral_CF"
# Set the working directory to the data directory
setwd(data_directory)

################################################################################
# Read the CSV file for VDP 60% data
vdp_file_name <- "" #(insert name)
vdp_data <- read.csv(vdp_file_name)

library("ggpubr")
ggboxplot(my_data, x = "vdp", y = "N4_vdp", 
          color = "Sequence", palette = c("#00AFBB", "#E7B800"),
          order = c("vdp", "N4_vdp"),
          ylab = "Measurement", xlab = "VDP (%)")

# Subset GRE sequence VDP percentage data
GRE <- subset(my_data,  Sequence == "GRE_VDP", VDP_percent,
                 drop = TRUE)
# subset Spiral sequence VDP percentage data
Spiral <- subset(my_data,  Sequence == "Spiral_VDP", VDP_percent,
                drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(GRE, Spiral)
plot(pd, type = "profile") + theme_bw()

res <- wilcox.test(VDP_percent ~ Sequence, data = my_data, paired = TRUE, alternative = "less")
res
# to print only p-value
res$p.value

# to test GRE < Spiral
res <- wilcox.test(VDP_percent ~ Sequence, data = my_data, paired = TRUE, alternative = "less")
res
# to test GRE > Spiral
res <- wilcox.test(VDP_percent ~ Sequence, data = my_data, paired = TRUE, alternative = "greater")
res

#res <- wilcox.test(GRE, Spiral, paired = TRUE)
#res

# We can conclude that the median VDP percentage of the patients with
# GRE sequence is NOT significantly different from the median VDP percentage with Spiral sequence
# with a p-value = 0.1513672

library(tidyverse)
library(hrbrthemes)
library(viridis)

# Plot
my_data %>%
  ggplot( aes(x=vdp, y=N4_vdp, color=Sequence)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("GRE vs Spiral VDP") +
  xlab("")



