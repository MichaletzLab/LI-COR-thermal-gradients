# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 3: Comparison of above and below leaf temps with T_air
#
# Last edited 13 July 2022, Josef Garen

make_fig3 = function() {

  # Load data
  licor_trials_all <- read.csv("data/licor_trials_all.csv")
  
  # Clean up label names
  licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6800"] = "LI-6800"
  
  # Select datasets of interest
  data6400 = subset(licor_trials_all, Licor_Type == "LI-6400XT")
  data6400 = subset(data6400, Licor_name == "Angert")
  data6400 = subset(data6400, Species != "Paper")
  data6800 = subset(licor_trials_all, Licor_Type == "LI-6800")
  data6800 = subset(data6800, Licor_name == "Pennell")
  data6800 = subset(data6800, Species != "Paper")
  
  # Convert data to long format
  data6400 = data6400 %>% gather(above_below, T_air_meas, T_air_below:T_air_above, factor_key = T)
  data6800 = data6800 %>% gather(above_below, T_air_meas, T_air_below:T_air_above, factor_key = T)
  
  # Open file
  pdf("figures/fig3.pdf", width = 7.5, height = 7)
  
  # Build plots
  p1 = ggplot(data = data6400, aes(x=T_air,y=T_air_meas,color=above_below)) + 
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
    ylim(c(3,47)) + 
    xlim(c(3,47)) +
    annotate("text", x = -Inf, y = 47, hjust = -0.1, label = "(a)  LI-6400XT")
  
  p2 = ggplot(data = data6800, aes(x=T_air,y=T_air_meas,color=above_below)) + 
    scale_fill_manual(values = palette_b) +
    scale_colour_manual(values = palette_b) +
    geom_abline(slope=1, linetype = "dashed") +
    geom_smooth(aes(color=above_below),method="lm") +
    geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
    scale_shape_manual(values=c(21,22,25)) +
    my_theme +
    xlab(NULL) + 
    ylab(NULL) +
    ylim(c(3,47)) +
    xlim(c(3,47)) +
    annotate("text", x = -Inf, y = 47, hjust = -0.1, label = "(b)  LI-6800")
  
  p3 = ggplot(data = data6400, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
    scale_fill_manual(values = palette_b) +
    scale_colour_manual(values = palette_b) +
    geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
    geom_smooth(aes(color=above_below),method="lm") +
    geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
    scale_shape_manual(values=c(21,22,25)) +
    my_theme +
    xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
    ylab(expression(atop(paste("Difference between measured and"), paste("reported air temperature (", degree, "C)")))) +
    ylim(c(-10,10)) + 
    xlim(c(3,47)) +
    annotate("text", x = -Inf, y = 10, hjust = -0.1, label = "(c)  LI-6400XT")
  
  p4 = ggplot(data = data6800, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
    scale_fill_manual(values = palette_b) +
    scale_colour_manual(values = palette_b) +
    geom_hline(data = NULL, yintercept=0, linetype = "dashed") +
    geom_smooth(aes(color=above_below),method="lm") +
    geom_point(aes(fill=above_below,shape = Species), size=2.4, color="black") +
    scale_shape_manual(values=c(21,22,25)) +
    my_theme +
    xlab(expression(paste("Reported air temperature (", degree, "C)"))) + 
    ylab(NULL) +
    ylim(c(-10,10)) +
    xlim(c(3,47)) +
    annotate("text", x = -Inf, y = 10, hjust = -0.1, label = "(d)  LI-6800")
  
  grid.arrange(p1,p2,p3,p4, widths = c(1.075,1), ncol = 2)
  
  # Close file
  dev.off()
  
  
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 3:\n")
  cat("===================================\n\n")

  z = lm(T_air_above ~ T_air, data = subset(licor_trials_all, Licor_Type == "LI-6400XT" & Species != "Raphanus sativus"))
  cat("Linear model summary (LI-6400XT, T_air_upper):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6400XT, T_air_upper):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(T_air_below ~ T_air, data = subset(licor_trials_all, Licor_Type == "LI-6400XT" & Species != "Raphanus sativus"))
  cat("Linear model summary (LI-6400XT, T_air_lower):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6400XT, T_air_lower):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(T_air_above ~ T_air, data = subset(licor_trials_all, Licor_Type == "LI-6800"))
  cat("Linear model summary (LI-6800, T_air_upper):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6800, T_air_upper):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(T_air_below ~ T_air, data = subset(licor_trials_all, Licor_Type == "LI-6800"))
  cat("Linear model summary (LI-6800, T_air_lower):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6800, T_air_lower):\n")
  print(confint(z))
  
  # Close file
  cat("\n\n\n")
  sink()
}
