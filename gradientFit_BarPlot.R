###To make bar plots from gradient across MR mean signal per slice data
###
library(ggplot2)
library(ggpubr)
library(dplyr)


cincy_col = c("#6ac3e2", "#e49fc4", "#e4487e", "#8fc541", "#d69cb7", "#9bd2da", "#cc64a4") # "#cc64a4", "#8fc541",

#Load data file (gradientFit_BarPlot_data.csv): GRE vs Spiral
df_4bar_grad <- read.csv(file.choose())

pbar<-ggbarplot(df_4bar_grad, x = "Correction", y = "Slope",add = c("mean_sd", "dotplot"),
          xlab = "Correction",ylab = "Norm. M-Slope (S/cm)", ylim = c(-0.04, 0.01),
          fill = "Correction",
          add.params = list(color = "black", fill="black"),
          font.x = c(22, "bold", "black"),yticks.by=0.01,
          font.y = c(22, "bold", "black"), 
          font.tickslab = c(22, "bold", "black"),
          ggtheme = theme_pubr()
)+    
  
    scale_fill_manual(values = c("#4DBEEE", "#D95319","#77AC30")) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed", size = 1.5)
pbar + theme(axis.line = element_line(colour = "black", size=0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none") +
  theme()

# 

# wilcox.test(df_4bar_grad$Slope, mu = 0)


matlab_colors2=c("#D95319", "#4DBEEE")

#Load data file (gradientFit_boxplot_percent.csv)
df_4bxp_gradp <- read.csv(file.choose())

compare_means(absPChange ~ Correc, data = df_4bxp_gradp, method = "wilcox.test", paired = TRUE)
p <- ggboxplot(df_4bxp_gradp, x = "Correc", y = "absPChange",
               shape = 19,size=1, ylim = c(0, 105),
               ylab = "Slope Change (%)", xlab = "Correction",
               fill = "Correc", add = "jitter",
               add.params = list(color = "black", size=3),
               bxp.errorbar = TRUE,bxp.errorbar.width = 0.5, legend = "none",
               font.x = c(22, "bold", "black"),
               font.y = c(22, "bold", "black"),
               font.tickslab = c(22, "bold", "black"),
               caption="Wilcoxon signed rank test") +
  font("caption", size = 12, color = "gray", face = "bold.italic")+
  scale_fill_manual(values = c( "#D95319","#77AC30"))

p + theme(axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none") +
  theme()

#  "#4DBEEE",

#stat_compare_means(method = "wilcox.test", paired = TRUE,label.y = 15,
#                   aes(label = paste0("P = ", ..p.format..)), size=8)

##    palette = "matlab_colors2",







df_grad <- data_summary(df_4bxp_grad, varname="Slope", 
                        groupnames=c("Correction"))
p_grad<- ggplot(df_grad, aes(x=Correction, y=Slope)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Slope-sd, ymax=Slope+sd), width=.2,
                position=position_dodge(.9))
print(p_grad)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
