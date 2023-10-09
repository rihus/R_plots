#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)


# Select and Load CFBronc_vdp_hyper_allData.csv file
df_4BA <- read.csv(file.choose())
# average of CTC, SOS value columns - VDP
df_4BA$mean_vdp <- rowMeans(df_4BA[,c("CTC_vdp","SOS_vdp")])
# average of CTC, SOS value columns - Hyper
df_4BA$mean_hyper <- rowMeans(df_4BA[,c("CTC_hyper","SOS_hyper")])
# average of CTC, SOS values after N4 correction - VDP
df_4BA$mean_N4_vdp <- rowMeans(df_4BA[,c("CTC_N4_vdp","SOS_N4_vdp")])
# average of CTC, SOS values after N4 correction - Hyper
df_4BA$mean_N4_hyper <- rowMeans(df_4BA[,c("CTC_N4_hyper","SOS_N4_hyper")])

# difference (SOS - CTC) - VDP
df_4BA$diff_vdp <- df_4BA$SOS_vdp - df_4BA$CTC_vdp
# difference (SOS - CTC) - Hyper
df_4BA$diff_hyper <- df_4BA$SOS_hyper - df_4BA$CTC_hyper
# difference (SOS - CTC) after N4 correction - VDP
df_4BA$diff_N4_vdp <- df_4BA$SOS_N4_vdp - df_4BA$CTC_N4_vdp
# difference (SOS - CTC) after N4 correction - Hyper
df_4BA$diff_N4_hyper <- df_4BA$SOS_N4_hyper - df_4BA$CTC_N4_hyper

#Average of difference - VDP
mean_diff_vdp <- mean(df_4BA$diff_vdp)
#Average of difference - Hyper
mean_diff_hyper <- mean(df_4BA$diff_hyper)
#Average of difference after N4 correction - VDP
mean_diff_N4_vdp <- mean(df_4BA$diff_N4_vdp)
#Average of difference after N4 correction - Hyper
mean_diff_N4_hyper <- mean(df_4BA$diff_N4_hyper)

#To find lower 95% confidence interval limits - VDP
lower_lim_vdp <- mean_diff_vdp - 1.96*sd(df_4BA$diff_vdp)
#To find lower 95% confidence interval limits with N4 correction - VDP
lower_lim_N4_vdp <- mean_diff_N4_vdp - 1.96*sd(df_4BA$diff_N4_vdp)
#To find lower 95% confidence interval limits - Hyper
lower_lim_hyper <- mean_diff_hyper - 1.96*sd(df_4BA$diff_hyper)
#To find lower 95% confidence interval limits with N4 correction - Hyper
lower_lim_N4_hyper <- mean_diff_N4_hyper - 1.96*sd(df_4BA$diff_N4_hyper)

#To find upper 95% confidence interval limits - VDP
upper_lim_vdp <- mean_diff_vdp + 1.96*sd(df_4BA$diff_vdp)
#To find upper 95% confidence interval limits with N4 correction - VDP
upper_lim_N4_vdp <- mean_diff_N4_vdp + 1.96*sd(df_4BA$diff_N4_vdp)
#To find upper 95% confidence interval limits - Hyper
upper_lim_hyper <- mean_diff_hyper + 1.96*sd(df_4BA$diff_hyper)
#To find upper 95% confidence interval limits with N4 correction - Hyper
upper_lim_N4_hyper <- mean_diff_N4_hyper + 1.96*sd(df_4BA$diff_N4_hyper)


#create Bland-Altman plot - VDP after N4
theme_set(theme_bw())
#N4 corrected VDP Bland-Altman plot - VDP
ggplot(df_4BA, aes(x = mean_N4_vdp, y = diff_N4_vdp)) +
  geom_point(size=5) +
  xlim(0, 25) + ylim(-6, 6) +
  annotate("rect", xmin= -Inf, xmax= Inf, ymin=lower_lim_N4_vdp,ymax=upper_lim_N4_vdp,
           fill="red",alpha = .2) +
  geom_hline(yintercept = mean_diff_N4_vdp, size = 1.2) +
  geom_hline(yintercept = lower_lim_N4_vdp, color = "black", linetype="dashed", size = 1.5) +
  geom_hline(yintercept = upper_lim_N4_vdp, color = "black", linetype="dashed", size = 1.5) +
  ylab("Spiral - GRE VDP (%)") +
  xlab("Mean of GRE & Spiral VDP (%)") +
  labs(caption = "Bland-Altman plot of VDP") +
  # annotate("text", x=19, y=3.5,label="Mean + 1.96*STD",color="black",size=6,fontface="bold") +
  # annotate("text", x=19, y=-2.7,label="Mean - 1.96*STD",color="black",size=6,fontface="bold") +
  theme(axis.title.x = element_text(vjust = 0, size = 22, face = "bold"),
        axis.title.y = element_text(vjust = 0, size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold", color = "black")) +
  font("caption", size = 12, color = "gray", face = "bold.italic")
 
#plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5))

#create Bland-Altman plot - Hyper after N4
theme_set(theme_bw())
#N4 corrected VDP Bland-Altman plot - Hyper
ggplot(df_4BA, aes(x = mean_N4_hyper, y = diff_N4_hyper)) +
  geom_point(size=5) +
  xlim(0, 0.6) + ylim(-0.6, 0.6) +
  annotate("rect", xmin= -Inf, xmax= Inf, ymin=lower_lim_N4_hyper,ymax=upper_lim_N4_hyper,
           fill="red",alpha = .2) +
  geom_hline(yintercept = mean_diff_N4_hyper, size = 1.2) +
  geom_hline(yintercept = lower_lim_N4_hyper, color = "black", linetype="dashed", size = 1.5) +
  geom_hline(yintercept = upper_lim_N4_hyper, color = "black", linetype="dashed", size = 1.5) +
  ylab("Spiral - GRE HVP (%)") +
  xlab("Mean of GRE & Spiral HVP (%)") +
  labs(caption = "Bland-Altman plot of HVP")+
  theme(axis.title.x = element_text(vjust = 0, color = "black", size = 22, face = "bold"),
        axis.title.y = element_text(vjust = 0, size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold", color = "black"))+
  font("caption", size = 12, color = "gray", face = "bold.italic")



# annotate("text", x=15, y=8, label= "Mean = ")+
#   annotate("text", x=16, y=8, label= )+


# # Original VDP Bland-Altman Plot
# ggplot(df_4BA, aes(x = mean, y = diff)) +
#   geom_point(size=5) +
#   xlim(0, 35) + ylim(-10, 10) +
#   annotate("rect", xmin = -Inf, xmax = Inf,ymin=lower_lim,ymax=upper_lim,fill="red",alpha = 0.2) +
#   geom_hline(yintercept = mean_diff, size = 1.2) +
#   geom_hline(yintercept = lower_lim, color = "black", linetype="dashed", size = 1.5) +
#   geom_hline(yintercept = upper_lim, color = "black", linetype="dashed", size = 1.5) +
#   #ggtitle("Bland-Altman of VDP: Spiral vs GRE sequences") +
#   ylab("Difference of VDP (%)") +
#   xlab("Average of GRE and Spiral VDP (%)") +
#   theme(axis.title.x = element_text(vjust = 0, size = 22),
#         axis.title.y = element_text(vjust = 2, size = 22), axis.text = element_text(size = 22))#,
# #plot.title = element_text(size = 14, face = "bold.italic", hjust = 0.5))
# #,fill = "none"
