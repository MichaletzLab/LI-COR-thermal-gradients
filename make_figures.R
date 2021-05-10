# Code to generate figures for LI-COR gradients manuscript
#
#
# 4 June 2020 Josef Garen

library(ggplot2)
library(gridExtra)
library(tidyverse)

library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

source("curve_fitting.R")
source("error_prop_6400.R")
source("error_prop_6800.R")

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_a = c("#E69F00", "#56B4E9")
palette_b = c("#0072B2", "#D55E00")
palette_c = c("#CC79A7","#009E73")

my_theme = theme_bw() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


### Figure 1 ###
# Relationship between T_leaf and T_air for both 6400 and 6800
{
licor_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/licor_trials_all.csv")

licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6400"] = "LI-6400XT"
licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6800"] = "LI-6800"

subdata = subset(licor_trials_all, Species == "Empty")


pdf("figures/fig1.pdf", width = 7.5, height = 3.5)

p1 = ggplot(data = subdata, aes(x = T_air, y = T_leaf-T_air, color = Licor_Type)) + 
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_smooth(data=subdata, method=lm) +
  geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
  geom_point(aes(pch = Licor_name, fill = Licor_Type), size=2.4, color = "black") +
  scale_shape_manual(values=c(21,22,23,24)) +
  my_theme +
  ylab(expression(atop(paste("Difference between measured"), paste("and reported air temperatures (", degree, "C)")))) +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) +
  guides(pch = F) +
  annotate("text", x = 13, y = 3.2, label = "(b)")

p2 = ggplot(data = subdata, aes(x = T_air, y = T_leaf, color = Licor_Type)) + 
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_smooth(method=lm) +
  geom_abline(slope=1, linetype = "dashed") +
  geom_point(aes(pch = Licor_name, fill=Licor_Type), size=2.4, color = "black") +
  scale_shape_manual(values=c(21,22,23,24)) +
  my_theme +
  guides(color = guide_legend(override.aes = list(pch = 21))) +
  theme(legend.position=c(0.8,0.15)) +
  theme(legend.title = element_blank()) +
  ylab(expression(paste("Measured in-cuvette air temperature (", degree, "C)"))) +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) +
  guides(pch = F) +
  annotate("text", x = 13, y = 41, label = "(a)")

grid.arrange(p2,p1, widths = c(1,1.075), ncol = 2)

dev.off()
}


### Figure 2 ###
# Comparison of above and below leaf temps with T_air
{
# Top, delta 6400
licor_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/licor_trials_all.csv")

licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6400"] = "LI-6400XT"
licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6800"] = "LI-6800"

fig2a = subset(licor_trials_all, Licor_Type == "LI-6400XT")
fig2a = subset(fig2a, Licor_name == "Angert")
fig2b = subset(licor_trials_all, Licor_Type == "LI-6800")
fig2b = subset(fig2b, Licor_name == "Pennell")
fig2a = subset(fig2a, Species != "Paper")
fig2b = subset(fig2b, Species != "Paper")

a = fig2a
a$above_below = "above"
a$T_air_meas = a$T_above
b = fig2a
b$above_below = "below"
b$T_air_meas = b$T_below
fig2a_1 = rbind(a,b)

a = fig2b
a$above_below = "above"
a$T_air_meas = a$T_above
b = fig2b
b$above_below = "below"
b$T_air_meas = b$T_below
fig2b_1 = rbind(a,b)


pdf("figures/fig2.pdf", width = 7.5, height = 7)

p1 = ggplot(data = fig2a_1, aes(x=T_air,y=T_air_meas,color=above_below)) + 
  scale_fill_manual(values = palette_b, labels = c(expression(paste("T"["air,upper"])),expression(paste("T"["air,lower"])))) +
  scale_colour_manual(values = palette_b, labels = c(expression(paste("T"["air,upper"])),expression(paste("T"["air,lower"])))) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method=lm) +
  geom_point(aes(fill=above_below, pch=Species),size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25), labels = c("Empty cuvette", expression(italic("G. shallon")), expression(italic("T. plicata")))) +
  my_theme +
  guides(color = guide_legend(override.aes = list(pch = 21))) +
  theme(legend.position = c(0.63,0.15)) +
  theme(legend.title = element_blank()) +
  theme(legend.box = "horizontal") +
  theme(legend.background = element_rect(fill="transparent")) +
  xlab(NULL) + 
  ylab(expression(atop(paste("Measured"), paste("air temperature (", degree, "C)")))) +
  ylim(c(10,42)) + 
  xlim(c(10,42)) +
  annotate("text", x = -Inf, y = 42, hjust = -0.1, label = "(a)  LI-6400XT")

  
p2 = ggplot(data = fig2b_1, aes(x=T_air,y=T_air_meas,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_abline(slope=1, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(NULL) + 
  ylab(NULL) +
  ylim(c(10,42)) +
  xlim(c(10,42)) +
  annotate("text", x = -Inf, y = 42, hjust = -0.1, label = "(b)  LI-6800")
 
p3 = ggplot(data = fig2a_1, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
  ylab(expression(atop(paste("Difference between measured and"), paste("reported air temperature (", degree, "C)")))) +
  ylim(c(-8,8)) + 
  xlim(c(10,42)) +
  annotate("text", x = -Inf, y = 8, hjust = -0.1, label = "(c)  LI-6400XT")

# Bottom, delta 6800
p4 = ggplot(data = fig2b_1, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
  ylab(NULL) +
  ylim(c(-8,8)) +
  xlim(c(10,42)) +
  annotate("text", x = -Inf, y = 8, hjust = -0.1, label = "(d)  LI-6800")

grid.arrange(p1,p2,p3,p4, widths = c(1.075,1), ncol = 2)

dev.off()
}


### Figure 3 ###
# Growth chamber trials
{
growth_chamber_trials_all <- read_csv("~/Documents/School/UBC - Botany/LICOR gradients/data/growth_chamber_trials_all.csv")

fig3a = subset(growth_chamber_trials_all, Licor == 6400)
fig3b = subset(growth_chamber_trials_all, Licor == 6800)

a = fig3a
a$above_below = "above"
a$T_air_meas = a$T_above
b = fig3a
b$above_below = "below"
b$T_air_meas = b$T_below
fig3a_1 = rbind(a,b)

a = fig3b
a$above_below = "above"
a$T_air_meas = a$T_above
b = fig3b
b$above_below = "below"
b$T_air_meas = b$T_below
fig3b_1 = rbind(a,b)

pdf("figures/fig3.pdf", width = 7, height = 7)

p1 = ggplot(data = fig3a_1, aes(x=T_air,y=T_air_meas,color=above_below)) + 
  scale_fill_manual(values = palette_b, labels = c(expression(paste("T"["air,upper"])),expression(paste("T"["air,lower"])))) +
  scale_colour_manual(values = palette_b, labels = c(expression(paste("T"["air,upper"])),expression(paste("T"["air,lower"])))) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below, shape=Species),size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25), labels = c("Empty cuvette", expression(italic("G. shallon")), expression(italic("T. plicata")))) +
  my_theme +
  guides(color = guide_legend(override.aes = list(pch = 21))) +
  theme(legend.position = c(0.62,0.15)) +
  theme(legend.title = element_blank()) +
  theme(legend.box = "horizontal") +
  theme(legend.background = element_rect(fill="transparent")) +
  xlab(NULL) + 
  ylab(expression(atop(paste("Measured"), paste("air temperature (", degree, "C)")))) +
  ylim(c(12,43)) +
  xlim(c(12,43)) +
  annotate("text", x = -Inf, y = 43, hjust = -0.1, label = "(a)  LI-6400XT")

p2 = ggplot(data = fig3b_1, aes(x=T_air,y=T_air_meas,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_abline(slope=1, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below, shape=Species),size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(NULL) + 
  ylab(NULL) +
  ylim(c(12,43)) +
  xlim(c(12,43)) +
  annotate("text", x = -Inf, y = 43, hjust = -0.1, label = "(b)  LI-6800")

p3 = ggplot(data = fig3a_1, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below, shape=Species),size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
  ylab(expression(atop(paste("Difference in measured"), paste("and reported air temperature (", degree, "C)")))) +
  ylim(c(-3,3)) +
  xlim(c(12,43)) +
  annotate("text", x = -Inf, y = 3, hjust = -0.1, label = "(c)  LI-6400XT")

# Bottom, delta 6800
p4 = ggplot(data = fig3b_1, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
  scale_fill_manual(values = palette_b) +
  scale_colour_manual(values = palette_b) +
  geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
  geom_smooth(aes(color=above_below),method="lm") +
  geom_point(aes(fill=above_below, shape=Species),size=2.4, color="black") +
  scale_shape_manual(values=c(21,22,25)) +
  my_theme +
  xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
  ylab(NULL) +
  ylim(c(-3,3)) +
  xlim(c(12,43)) +
  annotate("text", x = -Inf, y = 3, hjust = -0.1, label = "(d)  LI-6800")

grid.arrange(p1,p2,p3,p4, widths = c(1.1,1), ncol = 2)

dev.off()
}


### Figure 4 ###
# Tleaf error estimates
{
Tleaf_error_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/Tleaf_error_trials_all.csv")
Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"

#Tleaf_error_trials_all$Licor_Type = as.factor(Tleaf_error_trials_all$Licor_Type)
Tleaf_error_trials_all = subset(Tleaf_error_trials_all, !is.na(T_air_below))

pdf("figures/fig4.pdf", width = 7, height = 3.5)

# Tleaf error as a function of Tair and Tleaf (conditions reported by the machine)
p1 = ggplot(data=Tleaf_error_trials_all, aes(x=T_air-T_leaf, y=T_leaf-T_below, color=Licor_Type)) +
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_abline(slope= 0, lty=2) + 
  geom_smooth(method=lm) +
  geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape = 21) +
  my_theme +
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.title = element_blank()) +
  ylim(c(-3.1,4.4)) +
  xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)"))) + 
  ylab(expression(paste("Error in reported leaf temperature (", degree, "C)"))) +
  annotate("text", x = -3.3, y = 4.4, label = "(a)")

# Influence of in-chamber air temperature on Tleaf error
p2 = ggplot(data=Tleaf_error_trials_all, aes(x=T_air_below-T_below, y=T_leaf-T_below, color=Licor_Type)) +
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_abline(slope=0, lty=2)+ 
  geom_smooth(method=lm) +
  geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape=21) +
  my_theme +
  ylim(c(-3.1,4.4)) +
  xlab(expression(paste("Measured air-leaf temperature difference (", degree, "C)"))) + 
  ylab(NULL) +
  annotate("text", x = -2.8, y = 4.4, label = "(b)")
  

grid.arrange(p1, p2, ncol = 2)

dev.off()
}


### Figure 5 ###
# Errors in derived quantities
{
#source("error_prop_6400.R", echo=TRUE)
error_6400_data = read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6400_subsampled_broadleaf.csv")
#error_est_reg_6400 = est_error_6400_reg(error_6400_data, m = 0.7581, b = 0.518)
error_est_reg_6400 = est_error_6400_reg(error_6400_data, m = 0.7838, b = 0.8092)

source("error_prop_6800.R", echo=TRUE)
error_6800_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6800_subsampled_new.csv")
#error_est_reg_6800 = est_error_6800_reg(error_6800_data, m = 0.3818, b = -0.9969)
error_est_reg_6800 = est_error_6800_reg(error_6800_data, m = 0.4941, b = -1.1436)

# Bind together
error_est_reg_6400$Licor_Type = "LI-6400XT"
error_est_reg_6800$Licor_Type = "LI-6800"

error_all = rbind(error_est_reg_6400, error_est_reg_6800)
error_all = subset(error_all, error < 1)


pdf("figures/fig5.pdf", width = 7, height = 3.5)

dat_text <- data.frame(
  labels = c("(b)", "(c)", "(d)", "(e)"),
  variable = c("g_sw","g_tw","g_tc","C_i"),
  Tair = 0,
  Tleaf = 0,
  error = 0,
  Licor_Type = "LI-6400XT"
)

p1 = ggplot(data = error_all, aes(x = variable, y=100*error, fill=Licor_Type)) + 
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_boxplot() +
  ylab("Relative error (%)") + xlab(NULL) + #ylim(-0.75, 0.75) + 
  scale_x_discrete(labels=c("g_sw" = expression(italic("g"[sw])), 
                            "g_tw" = expression(italic("g"[tw])), 
                            "g_tc" = expression(italic("g"[tc])),
                            "C_i" = expression(italic("C"[i])))) +
  my_theme +
  theme(legend.position = c(0.8, 0.12)) +
  theme(legend.title = element_blank()) +
  theme(legend.background=element_blank()) +
  ylim(c(-50,75)) +
  geom_abline(slope = 0, lty = 2) +
  annotate("text", x = -Inf, y = 0.75*100, hjust = -0.1, label = "(a)") +
  theme(axis.text.x = element_text(face="bold",size=12))

p2 = ggplot(data = error_all, aes(x = Tair-Tleaf, y=100*error, color=Licor_Type)) +
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(size = 0.8) +
  facet_wrap(~ variable, ncol = 2) +
  my_theme + 
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)  "))) + 
  ylab("Relative error (%)") +
  guides(colour = "none") +
  ylim(c(-0.55*100, 0.75*100)) +
  geom_abline(slope = 0, lty = 2) +
  geom_text(data = dat_text, aes(x = -Inf, y = Inf, label = labels), color = "black", hjust = -0.1, vjust = 1.3)
#  annotate("text", x = -6, y = 0.75*100, label = "(b)") #+
#  annotate("text", x = 10, 0.75*100, label = "italic(C[i])", parse=T)

grid.arrange(p1,p2, ncol = 2)
dev.off()
}


### Figure 6 ###
# Revisiting the leaf thermoregulation hypothesis / Remastering Fig 1a in Blonder and Michaletz
{
# only_6400 = subset(licor_trials_all, Licor_Type == "6400")
# 
# plot(only_6400$T_air, only_6400$T_leaf)
# empty = subset(only_6400, Species == "Empty")
# 
# lm(T_below ~ T_air, data = only_6400) # 1. Below leaf T, all data; T_in_chamber = 5.6002 + 0.7947*T_air
# lm(T_above ~ T_air, data = only_6400) # 2. Above leaf T, all data; T_in_chamber = 10.7378 + 0.6006*T_air
# lm(T_leaf ~ T_air, data = only_6400)  # 3. T_leaf, empty chamber; T_in_chamber = 7.59 + 0.7388*T_air
# 
# above_below = rbind(data.frame(T_in_chamber = only_6400$T_below, T_air = only_6400$T_air),
#                     data.frame(T_in_chamber = only_6400$T_above, T_air = only_6400$T_air))
# 
# lm(T_in_chamber ~ T_air, data = above_below) # 4. All above and below data; T_in_chamber = 8.1690 + 0.6977*T_air

# All above and below data using only Gaultheria: T_in_chamber = 8.1591 + 0.7032*T_air (almost exactly the same as for all data)

# Let's bring in the Blonder Michaletz data and start making some corrections
bm_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/blonder_michaletz_fig_1a_data_cut.csv")

# We can apply our correction to Tleaf based on the difference between Tair and reported Tleaf
# T_leaf-T_internal = (T_air-T_leaf)*0.7581 + 0.518
# So T_internal = -[ (T_air-T_leaf)*0.7581 + 0.518 - T_leaf ]
# Correcting T_leaf,LI based on Fig 4 regression for Tleaf error as a fn of (Tair,LI-Tleaf,LI)
bm_data$T_internal = -( (bm_data$Tair_C-bm_data$Tleaf_C)*0.7838 + 0.8092 - bm_data$Tleaf_C )

# Correcting Tair,LI based on regression of Tair,meas vs. Tair,LI (same data as Fig 4?)
# Unfortunately I forgot where these regressions came from
#bm_data$T_cham_1 = 5.6002 + 0.7947*bm_data$Tair_C
#bm_data$T_cham_2 = 10.7378 + 0.6006*bm_data$Tair_C
#bm_data$T_cham_3 = 7.59 + 0.7388*bm_data$Tair_C
#bm_data$T_cham_4 = 8.1690 + 0.6977*bm_data$Tair_C
#bm_data$T_cham_5 = 8.1591 + 0.7032*bm_data$Tair_C
bm_data$T_cham = 8.3243 + 0.6705*bm_data$Tair_C

bm_broadleaf = subset(bm_data, Site != "SUMO")
bm_broadleaf = subset(bm_broadleaf, !is.na(Tleaf_C))
bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes

bm_broadleaf %>% 
  group_by(curveID) %>% 
  do(model = lm(T_internal ~ T_cham, data = .)) -> tbl_models

model_coef = c()
for (i in 1:dim(tbl_models)[1]) {
  model_coef$curveID[i] = tbl_models$curveID[i]
  model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
  model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
}
model_coef = data.frame(model_coef)
model_coef$high_slope = model_coef$slope > 1
bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")

pdf("fig6.pdf", width = 7, height = 3.5)

main_plot = ggplot(data = bm_broadleaf, aes(x = T_cham, y = T_internal)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c) +
  scale_color_manual(values=c("blue","red")) +
  geom_abline(slope = 1, lty=2) +
  my_theme +
  xlab("Corrected in-chamber air temperature (C)") +
  ylab("Corrected leaf temperature (C)") +
  xlim(c(9,43)) + ylim(c(9,43)) +
  annotate("text", x = 9, y = 43, label = "(a)")

inset_plot = ggplotGrob(
  ggplot(data = model_coef, aes(x = slope)) + 
    geom_density() + 
    theme_classic() +
    theme(axis.title = element_text(size=8),
          rect = element_rect(fill = "transparent"),
          plot.background = element_rect(colour = "transparent") )
  )

p1 = main_plot + annotation_custom(grob = inset_plot, xmin = 7.5, xmax = 27, ymin = 29, ymax = 45)

p2 = ggplot(data = bm_broadleaf, aes(x = T_cham, y = T_internal-T_cham)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c) +
  geom_abline(slope = 0, intercept = 0, lty=2) +
  scale_color_manual(values=c("blue","red")) +
  my_theme +
  xlab("Corrected in-chamber air temperature (C)") +
  ylab("Corrected leaf temperature excess (C)") +
  xlim(c(9,43)) +# ylim(c(8,46)) 
  annotate("text", x = 9, y = 11, label = "(b)")


grid.arrange(p1, p2, ncol = 2)
dev.off()
}
### Figure 6 version 2
{
  # only_6400 = subset(licor_trials_all, Licor_Type == "6400")
  # 
  # plot(only_6400$T_air, only_6400$T_leaf)
  # empty = subset(only_6400, Species == "Empty")
  # 
  # lm(T_below ~ T_air, data = only_6400) # 1. Below leaf T, all data; T_in_chamber = 5.6002 + 0.7947*T_air
  # lm(T_above ~ T_air, data = only_6400) # 2. Above leaf T, all data; T_in_chamber = 10.7378 + 0.6006*T_air
  # lm(T_leaf ~ T_air, data = only_6400)  # 3. T_leaf, empty chamber; T_in_chamber = 7.59 + 0.7388*T_air
  # 
  # above_below = rbind(data.frame(T_in_chamber = only_6400$T_below, T_air = only_6400$T_air),
  #                     data.frame(T_in_chamber = only_6400$T_above, T_air = only_6400$T_air))
  # 
  # lm(T_in_chamber ~ T_air, data = above_below) # 4. All above and below data; T_in_chamber = 8.1690 + 0.6977*T_air
  
  # All above and below data using only Gaultheria: T_in_chamber = 8.1591 + 0.7032*T_air (almost exactly the same as for all data)
  
  # Let's bring in the Blonder Michaletz data and start making some corrections
  bm_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/blonder_michaletz_fig_1a_data_cut.csv")
  
  # We can apply our correction to Tleaf based on the difference between Tair and reported Tleaf
  # T_leaf-T_internal = (T_air-T_leaf)*0.7581 + 0.518
  # So T_internal = -[ (T_air-T_leaf)*0.7581 + 0.518 - T_leaf ]
  # Correcting T_leaf,LI based on Fig 4 regression for Tleaf error as a fn of (Tair,LI-Tleaf,LI)
  bm_data$T_internal = -( (bm_data$Tair_C-bm_data$Tleaf_C)*0.7838 + 0.8092 - bm_data$Tleaf_C )
  
  # Correcting Tair,LI based on regression of Tair,meas vs. Tair,LI (same data as Fig 4?)
  # Unfortunately I forgot where these regressions came from
  #bm_data$T_cham_1 = 5.6002 + 0.7947*bm_data$Tair_C
  #bm_data$T_cham_2 = 10.7378 + 0.6006*bm_data$Tair_C
  #bm_data$T_cham_3 = 7.59 + 0.7388*bm_data$Tair_C
  #bm_data$T_cham_4 = 8.1690 + 0.6977*bm_data$Tair_C
  #bm_data$T_cham_5 = 8.1591 + 0.7032*bm_data$Tair_C
  bm_data$T_cham = 8.3243 + 0.6705*bm_data$Tair_C
  
  bm_broadleaf = subset(bm_data, Site != "SUMO")
  bm_broadleaf = subset(bm_broadleaf, !is.na(Tleaf_C))
  bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes
  
  bm_broadleaf %>% 
    group_by(curveID) %>% 
    do(model = lm(Tleaf_C ~ Tair_C, data = .)) -> tbl_models
  
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$curveID[i] = tbl_models$curveID[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  model_coef$high_slope = model_coef$slope > 1
#  bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")
  
  pdf("figures/fig6.pdf", width = 7, height = 3.5)
  
  main_plot = ggplot(data = bm_broadleaf, aes(x = Tair_C, y = Tleaf_C)) + 
    geom_point(size = 0.8) + 
    geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c[1]) +
    scale_color_manual(values=c("blue","red")) +
    geom_abline(slope = 1, lty=2) +
    my_theme +
    xlab("Uncorrected in-chamber air temperature (ºC)") +
    ylab("Uncorrected leaf temperature (ºC)") +
    xlim(c(9,43)) + ylim(c(9,43)) +
    annotate("text", x = 9, y = 43, label = "(a)")
  
  inset_plot = ggplotGrob(
    ggplot(data = model_coef, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0.25,1)) + ylim(c(0,7.5)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p1 = main_plot + annotation_custom(grob = inset_plot, xmin = 7.5, xmax = 27, ymin = 29, ymax = 45)

  
  
  bm_broadleaf %>% 
    group_by(curveID) %>% 
    do(model = lm(T_internal ~ T_cham, data = .)) -> tbl_models
  
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$curveID[i] = tbl_models$curveID[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  model_coef$high_slope = model_coef$slope > 1
#  bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")
  
  main_plot = ggplot(data = bm_broadleaf, aes(x = T_cham, y = T_internal)) + 
    geom_point(size = 0.8) + 
    geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c[2]) +
    scale_color_manual(values=c("blue","red")) +
    geom_abline(slope = 1, lty=2) +
    my_theme +
    xlab("Corrected in-chamber air temperature (ºC)") +
    ylab("Corrected leaf temperature (ºC)") +
    xlim(c(9,43)) + ylim(c(9,43)) +
    annotate("text", x = 9, y = 43, label = "(b)")
  
  inset_plot = ggplotGrob(
    ggplot(data = model_coef, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0.25,1)) + ylim(c(0,7.5)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p2 = main_plot + annotation_custom(grob = inset_plot, xmin = 7.5, xmax = 27, ymin = 29, ymax = 45)
  
  grid.arrange(p1, p2, ncol = 2)
  dev.off()
}


### Figure 7 ###
# Correcting an AT curve
{
AT_uncorrected = read.csv("data/AT_uncorrected.csv")
AT_uncorrected$Tleaf_corr = -( (AT_uncorrected$Tair-AT_uncorrected$Tleaf)*0.7838 + 0.8092 - AT_uncorrected$Tleaf )


uncorr_df = data.frame(
  curveID = 1,
  Taxon = AT_uncorrected$Taxon,
  Condition = "Uncorrected",
  Tleaf = AT_uncorrected$Tleaf,
  Photo = AT_uncorrected$Photo)

corr_df = data.frame(
  curveID = 2,
  Taxon = AT_uncorrected$Taxon,
  Condition = "Corrected",
  Tleaf = AT_uncorrected$Tleaf_corr,
  Photo = AT_uncorrected$Photo)

models_to_fit = c("johnsonlewin_1946", "briere2_1999", "gaussian_1987")
mod = models_to_fit[1]


mod = "johnsonlewin_1946"
start_vals = get_start_vals(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
low_lims = get_lower_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
upper_lims = get_upper_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)

fit = nls_multstart(Photo~johnsonlewin_1946(temp = Tleaf, r0,e,eh,topt),
                     data = uncorr_df,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')


mod = "pawar_2018"
start_vals = get_start_vals(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
low_lims = get_lower_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
upper_lims = get_upper_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)

fit = nls_multstart(Photo~pawar_2018(temp = Tleaf, r_tref,e,eh,topt, tref=25),
                    data = uncorr_df,
                    iter = 500,
                    start_lower = start_vals - 10,
                    start_upper = start_vals + 10,
                    lower = low_lims,
                    upper = upper_lims,
                    supp_errors = 'Y')


mod = "sharpeschoolhigh_1981"
start_vals = get_start_vals(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
low_lims = get_lower_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)
upper_lims = get_upper_lims(uncorr_df$Tleaf, uncorr_df$Photo, model_name = mod)

fit = nls_multstart(Photo~sharpeschoolhigh_1981(temp = Tleaf, r_tref,e,eh,th,tref=25),
                    data = uncorr_df,
                    iter = 500,
                    start_lower = start_vals - 10,
                    start_upper = start_vals + 10,
                    lower = low_lims,
                    upper = upper_lims,
                    supp_errors = 'Y')

fit <- nls_multstart(Photo ~ (Schoolfield(lnB0, E, E_D, T_h, temp = Tleaf)),
                     data = uncorr_df,
                     iter = 1000,
                     start_lower = c(lnB0 = 0, E = 0, E_D = 0.2, T_h = 285),
                     start_upper = c(lnB0 = 100, E = 4, E_D = 8, T_h = 330),
                     supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(lnB0 = -10, E = 0, E_D = 0, T_h = 0))





Schoolfield <- function(lnB0, E, E_D, T_h, temp, SchoolTpk=TRUE) { 
  # temp   : Temperature values to evaluate at (C)
  # lnB0   : Normalisation constant (units depend on rate; log transformed)
  # E      : Activation energy (eV; E > 0)
  # E_D    : High temperature de-activation energy (eV, E_D > 0) 
  # T_h    : Temperature at which trait reaches peak value (K, Tpk)   
  
  k <- 8.62e-5        # Boltzmann's constant (eV K-1)
  temp = temp+273.15  # Convert T to Kelvin
  
  return(lnB0*exp((-E/k) * ((1/temp)))/(1 + (E/(E_D - E)) * exp(E_D/k * (1/T_h - 1/temp)))))
}




#all_data = rbind(uncorr_df, corr_df)

#fits = fit_curves(all_data)

ModelToPlotS = c()

for (i in 1:2) {
  d_1 = subset(all_data, curveID == i)
  # Make predicted model lines
  # Pick a set of temperature values to compute model over
  tmp_temps <- seq(min(floor(d_1$Tleaf)), ceiling(max(d_1$Tleaf)), length = 200)
  
  # Compute model predictions depending on model type and select curve color
  tmp_model <- exp(Schoolfield(fits$lnB0[i], fits$E[i], fits$E_D[i], fits$T_h[i], tmp_temps))
  color_model = "red"
  
  # Assemble model predicitons and raw data to plot
  ModelToPlotS <- rbind(ModelToPlotS,data.frame(Temperature = tmp_temps, TraitValue = tmp_model, curveID = i, Condition = fits$Condition[i]))
}

pdf("figures/fig7.pdf", width = 4, height = 3.5)

ggplot(all_data, aes(x = Tleaf, y = Photo, color = Condition)) + 
  scale_fill_manual(values = palette_c) +
  scale_colour_manual(values = palette_c) +
  geom_point(aes(fill = Condition), size=2.4, color = "black", pch = 21) +
#  scale_shape_manual(values=c(21)) +
#  geom_point() +
  geom_line(data = ModelToPlotS, aes(x = Temperature, y = TraitValue)) +
  my_theme +
  theme(legend.position = c(0.165,0.88)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(fill="transparent")) +
  xlab("Leaf temperature (ºC)") +
  ylab("Assimilation rate (µmol/m²s)") #+
#geom_segment(aes(x = 35, y = 9, xend = 28, yend = 9), color = "black",
#             arrow = arrow(length = unit(0.5, "cm")))

dev.off()
}


### Figure 8 ###
# Correcting ACi curves
{
recalc_Ci <- function(test_params, m = 1, b = 0) {
  
  # Read out the variables we need
  E_cur = test_params$Trmmol/1000 #test_params$Trans       # Transpiration rate
  TleafC_cur = test_params$Tleaf  # Leaf Temperature C
  TairC_cur = test_params$Tair    # Air Temperature C
  P_cur = test_params$Press       # Air pressure
  W_s_cur = test_params$H2OS      # H20 in sample
  g_bw_cur = test_params$BLC_1    # Boundary layer conductance
  K_cur = test_params$StmRat      # Stomatal ratio
  A_cur = test_params$Photo       # Photosynthetic rate
  C_s_cur = test_params$CO2S      # CO2 sample
  
  # Compute current T_leaf error under current conditions
  T_error = m*(TairC_cur-TleafC_cur)+b
  
  # Recompute conductance and Ci
  g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
  g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur)
  g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
  C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
  
  return(C_i_a)
}


aci_all = read.csv("data/aci_by_temp_all.csv")
Ci_corr = recalc_Ci(aci_all, m = 0.7838, b = 0.8092)
aci_all$Ci_corr = Ci_corr

# Vcmax and Jmax values
plot_model_14 = c()

a = subset(aci_all, T_setpoint == 14)
b = fitaci(a, Tcorrect = F)
c = b$Photosyn(Ca = 1*(60:2500))
c$Type = "Uncorrected"
plot_model_14 = c

b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
c = b$Photosyn(Ca = 1*(60:2500))
c$Type = "Corrected"
plot_model_14 = rbind(plot_model_14,c)

plot_model_38 = c()

a = subset(aci_all, T_setpoint == 38)
b = fitaci(a, Tcorrect = F)
c = b$Photosyn(Ca = 1*(52:1921))
c$Type = "Uncorrected"
plot_model_38 = c

b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
c = b$Photosyn(Ca = 1*(52:2022))
c$Type = "Corrected"
plot_model_38 = rbind(plot_model_38,c)

aci_plot = rbind(
  data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci, Type = "Uncorrected", T_setpoint = aci_all$T_setpoint),
  data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci_corr, Type = "Corrected", T_setpoint = aci_all$T_setpoint))

#aci_plot = subset(aci_plot, T_setpoint == 14 | T_setpoint == 38)

aci_plot$T_setpoint = as.factor(aci_plot$T_setpoint)

pdf("figures/fig8.pdf", width = 7.5, height = 3.5)

aci_cut = subset(aci_plot, T_setpoint == 14)

p1 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
  scale_fill_manual(values = palette_c) +
  scale_colour_manual(values = palette_c) +  
  geom_line(data = plot_model_14, aes(x=Ci, y=ALEAF,color=Type),lwd=0.4) +
  geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
  my_theme +
  xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
  ylab("Net assimilation (µmol/m²s)") +
  xlim(c(0,1700)) + ylim(c(-2,27)) +
  annotate("text", x = 300, y = 27, label = expression(paste("(a) ", italic("T"[paste("block,LI")]), " = 14ºC")))

aci_cut = subset(aci_plot, T_setpoint == 38)
#fitaci(data = aci_cut)
#fitaci(data = aci_cut, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",
#                                       PPFD = "PARi", Rd = "Rd"))

p2 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
  scale_fill_manual(values = palette_c) +
  scale_colour_manual(values = palette_c) +
  geom_line(data = plot_model_38, aes(x=Ci, y=ALEAF), lwd=0.4) +
  geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
  my_theme +
  xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
  ylab("Net assimilation (µmol/m²s)") +
  theme(legend.position = c(0.8,0.15)) +
  theme(legend.title = element_blank()) +
  xlim(c(0,1700)) + ylim(c(-2,27)) +
  annotate("text", x = 300, y = 27, label = expression(paste("(b) ", italic("T"[paste("block,LI")]), " = 38ºC")))

grid.arrange(p1, p2, ncol = 2)
dev.off()
}





###
### Supplementary figures
###

### Figure S2 ###
# Supplemental figure showing agreement of leaf and threaded thermocouple
{
Tleaf_error_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/Tleaf_error_trials_all.csv")
Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"

subsub = subset(Tleaf_error_trials_all, !is.na(T_thread))

pdf("figS2.pdf", width = 4, height = 4)

ggplot(data=Tleaf_error_trials_all, aes(x=T_thread, y=T_below)) +
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_abline(slope= 1, lty=2) + 
  geom_smooth(method=lm, color="black") +
  geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape = 21) +
  my_theme +
  theme(legend.position = c(0.2, 0.85)) +
  theme(legend.title = element_blank()) +
  xlim(c(15,43)) +
  ylim(c(15,43)) +
  xlab(expression(paste("Leaf temperature, threaded thermocouple (", degree, "C)"))) + 
  ylab(expression(paste("Leaf temperature, abaxial thermocouple (", degree, "C)"))) #+
#  annotate("text", x = -3.3, y = 4.4, label = "(a)")

dev.off()
}


### Figure S3
# New supplementary figure - revisiting Blonder and Michaletz, but w/o extrapolation
{
bm_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/blonder_michaletz_fig_1a_data_cut.csv")

# First trim the data s.t. Tair-Tleaf is w/in the bounds of our data (-3.33,2.18)
# Then trim s.t. Tair is w/in the bounds of our data (15.67,37.56)
bm_data = subset(bm_data, Tair_C >= 15.67 & Tair_C <= 37.56)
bm_data$diff = bm_data$Tair_C-bm_data$Tleaf_C
bm_data = subset(bm_data, diff >= -3.33 & diff <= 2.18)

# We can apply our correction to Tleaf based on the difference between Tair and reported Tleaf
# T_leaf-T_internal = (T_air-T_leaf)*0.7581 + 0.518
# So T_internal = -[ (T_air-T_leaf)*0.7581 + 0.518 - T_leaf ]
# Correcting T_leaf,LI based on Fig 4 regression for Tleaf error as a fn of (Tair,LI-Tleaf,LI)
bm_data$T_internal = -( (bm_data$Tair_C-bm_data$Tleaf_C)*0.7838 + 0.8092 - bm_data$Tleaf_C )

# Correcting Tair,LI based on regression of Tair,meas vs. Tair,LI (same data as Fig 4?)
# Unfortunately I forgot where these regressions came from
#bm_data$T_cham_1 = 5.6002 + 0.7947*bm_data$Tair_C
#bm_data$T_cham_2 = 10.7378 + 0.6006*bm_data$Tair_C
#bm_data$T_cham_3 = 7.59 + 0.7388*bm_data$Tair_C
#bm_data$T_cham_4 = 8.1690 + 0.6977*bm_data$Tair_C
#bm_data$T_cham_5 = 8.1591 + 0.7032*bm_data$Tair_C
bm_data$T_cham = 8.3243 + 0.6705*bm_data$Tair_C

bm_broadleaf = subset(bm_data, Site != "SUMO")
bm_broadleaf = subset(bm_broadleaf, !is.na(Tleaf_C))
bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes

bm_broadleaf %>% 
  group_by(curveID) %>% 
  do(model = lm(T_internal ~ T_cham, data = .)) -> tbl_models

model_coef = c()
for (i in 1:dim(tbl_models)[1]) {
  model_coef$curveID[i] = tbl_models$curveID[i]
  model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
  model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
}
model_coef = data.frame(model_coef)
model_coef$high_slope = model_coef$slope > 1
bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")

pdf("figS3.pdf", width = 7, height = 3.5)

main_plot = ggplot(data = bm_broadleaf, aes(x = T_cham, y = T_internal)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c) +
  scale_color_manual(values=c("blue","red")) +
  geom_abline(slope = 1, lty=2) +
  my_theme +
  xlab("Corrected in-chamber air temperature (C)") +
  ylab("Corrected leaf temperature (C)") +
  xlim(c(14,38)) + ylim(c(14,38)) +
  annotate("text", x = 14, y = 38, label = "(a)")

inset_plot = ggplotGrob(
  ggplot(data = model_coef, aes(x = slope)) + 
    geom_density() + 
    theme_classic() +
    theme(axis.title = element_text(size=8),
          rect = element_rect(fill = "transparent"),
          plot.background = element_rect(colour = "transparent") )
)

p1 = main_plot + annotation_custom(grob = inset_plot, xmin = 13, xmax = 27, ymin = 28, ymax = 38)

p2 = ggplot(data = bm_broadleaf, aes(x = T_cham, y = T_internal-T_cham)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method=lm, fill=NA, aes(group=curveID), lwd=0.3, color = palette_c) +
  geom_abline(slope = 0, intercept = 0, lty=2) +
  scale_color_manual(values=c("blue","red")) +
  my_theme +
  xlab("Corrected in-chamber air temperature (C)") +
  ylab("Corrected leaf temperature excess (C)") +
  xlim(c(14,38)) + ylim(c(-8,8)) +
  annotate("text", x = 14, y = 8, label = "(b)")


grid.arrange(p1, p2, ncol = 2)
dev.off()
}


### Figure S4 ###
# Figure showing possible thermoregulation in our T_leaf error dataset
{
#Tleaf_error_trials_all$Licor_Type = as.factor(Tleaf_error_trials_all$Licor_Type)
Tleaf_error_trials_all$T_air_avg = (Tleaf_error_trials_all$T_air_below+Tleaf_error_trials_all$T_air_above)/2

pdf("figS4.pdf", width = 7, height = 3.5)

p1 = ggplot(data = Tleaf_error_trials_all, aes(x = T_air_avg, y = T_below, color=Licor_Type)) +
  scale_fill_manual(values = palette_a) +
  scale_colour_manual(values = palette_a) +
  geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape = 21) + 
  geom_smooth(method = lm) +
  geom_abline(slope = 1, intercept = 0, lty =2) +
  my_theme +
  xlab("Measured air temperature (C)") +
  ylab("Measured leaf temperature (C)") +
  theme(legend.position = c(0.8,0.15)) +
  theme(legend.title = element_blank()) +
  xlim(c(16,41)) +
  ylim(c(16,41)) +
  annotate("text", x = 14, y = 42, label = "(a)")

p2 = ggplot(data = Tleaf_error_trials_all, aes(x = T_air_avg, y = T_below-T_air_avg, color=Licor_Type)) + 
  scale_fill_manual(values = cbPalette) +
  scale_colour_manual(values = cbPalette) +
  geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape = 21) + 
  geom_smooth(method = lm) +
  geom_abline(slope = 0, lty =2) +
  my_theme +
  xlab("Measured air temperature (C)") +
  ylab("Measured leaf-air temperature difference (C)   ") +
  xlim(c(16,41)) +
  annotate("text", x = 14, y = 3, label = "(b)")

grid.arrange(p1,p2, ncol=2)
dev.off()
}


### Figure S5 ###
# Showing thermocouple calibration
{
tc_calib_all <- read.csv("data/tc_calib_all.csv")

pdf("figures/figS5.pdf", width = 5, height = 5)

ggplot(data = tc_calib_all, aes(x = Sous_vide, y = Value, fill = Type)) +
  geom_point(pch = 21, size = 2.4) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  my_theme +
  xlab("Water temperature setpoint (ºC)") +
  ylab("Thermocouple reported temperature (ºC)") +
  xlim(c(4,47)) + ylim(c(4,47)) +
  theme(legend.position = c(0.15,0.6)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(fill="transparent"))

dev.off()
}
  
  
  