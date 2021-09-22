### Figure 5 ###
# Tleaf error estimates
make_fig5 = function() {
  
  # Read data
  Tleaf_error_trials_all <- read.csv("data/Tleaf_error_trials_all.csv")
  
  # Clean up label names
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"
  
  # Remove lines with missing data
  Tleaf_error_trials_all = subset(Tleaf_error_trials_all, !is.na(T_air_below))
  
  # Open file
  pdf("figures/fig5.pdf", width = 7, height = 3.5)

  # Build plots  
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
  
  # Close file
  dev.off()
  
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 5:\n")
  cat("===================================\n\n")
  
  Tleaf_error_trials_all$a = Tleaf_error_trials_all$T_leaf-Tleaf_error_trials_all$T_below
  Tleaf_error_trials_all$b = Tleaf_error_trials_all$T_air-Tleaf_error_trials_all$T_leaf
  Tleaf_error_trials_all$c = Tleaf_error_trials_all$T_air_below-Tleaf_error_trials_all$T_below
  
      
  z = lm(a ~ b, data = subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT"))
  cat("Linear model summary (LI-6400XT, Tleaf error vs. Tair-Tleaf):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6400XT, Tleaf error vs. Tair-Tleaf):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(a ~ b, data = subset(Tleaf_error_trials_all, Licor_Type == "LI-6800"))
  cat("Linear model summary (LI-6800, Tleaf error vs. Tair-Tleaf):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6800, Tleaf error vs. Tair-Tleaf):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(a ~ c, data = subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT"))
  cat("Linear model summary (LI-6400XT, Tleaf error vs. Tair,lower-Tleaf,lower):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6400XT, Tleaf error vs. Tair,lower-Tleaf,lower):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(a ~ c, data = subset(Tleaf_error_trials_all, Licor_Type == "LI-6800"))
  cat("Linear model summary (LI-6800, Tleaf error vs. Tair,lower-Tleaf,lower):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6800, Tleaf error vs. Tair,lower-Tleaf,lower):\n")
  print(confint(z))
  
  
  # Close file
  cat("\n\n\n")
  sink()
}
