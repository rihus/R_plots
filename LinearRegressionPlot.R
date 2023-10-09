##To plot linear regression between two variables
library(ggplot2)
library(ggpubr)

# Choose the file (CFBronc_vdp_hyper_allData.csv)
df_4Spearman <- read.csv(file.choose())

#Get the spearman correlation for N4 corrected VDP
x_vdp <- df_4Spearman[,c('CTC_N4_vdp')]
y_vdp <- df_4Spearman[,c('SOS_N4_vdp')]
# Apply the lm() function.
relation_vdp <- lm(x_vdp~y_vdp)
print(summary(relation_vdp))
#Get the spearman correlation for N4 corrected Hyper
x_hyper <- df_4Spearman[,c('CTC_N4_hyper')]
y_hyper <- df_4Spearman[,c('SOS_N4_hyper')]
# Apply the lm() function.
relation_hyper <- lm(x_hyper~y_hyper)
print(summary(relation_hyper))

# Scatter plot of Spearman's - VDP
sp1 <- ggscatter(df_4Spearman, x = "CTC_N4_vdp", y = "SOS_N4_vdp",
          color = "black",shape = 19, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          xlim = c(0, 25), xlab="GRE VDP (%)",
          ylim = c(0, 25), ylab="Spiral VDP (%)",
          add.params = list(color = "blue", size=2, fill = "red",alpha = 0.2), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          ggtheme = theme_bw(),
          font.x = c(22, "bold", "black"),
          font.y = c(22, "bold", "black"),
          font.tickslab = c(22, "bold", "black"),
          caption="Spearman's rank correlation coefficient"
#          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
#          cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) +
  font("caption", size = 12, color = "gray", face = "bold.italic")

sp1 + stat_cor(method = "spearman", cor.coef.name = c("rho"),aes(label = paste(..rr.label..)),
              label.x = 0,label.y = 23,label.sep = "\n", size = 9
          
)
# , gsub("p", "P", ..p.label..), sep = "~`,`~"
# Scatter plot of Spearman's - Hyper
sp2 <- ggscatter(df_4Spearman, x = "CTC_N4_hyper", y = "SOS_N4_hyper",
                color = "black",shape = 19, size = 5, # Points color, shape and size
                add = "reg.line",  # Add regressin line
                xlim = c(0, 0.7), xlab="GRE HVP (%)",
                ylim = c(0, 0.7), ylab="Spiral HVP (%)",
                add.params = list(color = "blue", size=2, fill = "red",alpha = 0.2), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                ggtheme = theme_bw(),
                font.x = c(22, "bold", "black"),
                font.y = c(22, "bold", "black"),
                font.tickslab = c(22, "bold", "black"),
                caption="Spearman's rank correlation coefficient"
                #          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                #          cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) +
  font("caption", size = 12, color = "gray", face = "bold.italic")

sp2 + stat_cor(method = "spearman", cor.coef.name = c("rho"),aes(label = paste(..rr.label..)),
              label.x = 0,label.y = 0.65,label.sep = "\n", size = 9
              
)
# , gsub("p", "P", ..p.label..), sep = "~`,`~"

# plot(y,x,xlim = c(0, 25),ylim = c(-0, 25),cex.axis = 1.5,
#      col = "blue",main = "GRE vs Spiral VDP regression",abline(lm(x~y)),
#      cex = 1.4,pch = 17,cex.lab = 1.5,xlab = "GRE VDP (%)",ylab = "Spiral VDP (%)")


# # Linear Regression plot
# ggplotRegression <- function (fit) {
#   
#   require(ggplot2)
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point(size = 3) +
#     xlab("GRE VDP (%)") +
#     ylab("Spiral VDP (%)") +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R^2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 ),
#                        " Slope =",signif(fit$coef[[2]], 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5)))+
#     theme(axis.text=element_text(size=14),
#           axis.title=element_text(size=14,face="bold"))
# }
# 
# ggplotRegression(lm(x~y))



