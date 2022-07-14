# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 2: Relationship between T_leaf and T_air for both 6400 and 6800
#
# Last edited 13 July 2022, Josef Garen

make_fig2 = function() {
  
  # Read in data
  licor_trials_all <- read.csv("data/licor_trials_all.csv")
  
  # Clean up label names
  licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  licor_trials_all$Licor_Type[licor_trials_all$Licor_Type == "6800"] = "LI-6800"
  
  # Empty chambers only
  subdata = subset(licor_trials_all, Species == "Empty")
  
  # Open file
  pdf("figures/fig2.pdf", width = 7.5, height = 3.5)
  
  # Build plots
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
    guides(pch = "none") +
    xlim(3,47) +
    annotate("text", x = 3, y = 7, label = "(b)")
  
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
    guides(pch = "none") +
    xlim(3,47) +
    ylim(3,47) +
    annotate("text", x = 3, y = 47, label = "(a)")
  
  grid.arrange(p2,p1, widths = c(1,1.075), ncol = 2)
  
  # Close file
  dev.off()
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 2:\n")
  cat("===================================\n\n")
  
  z = lm(T_leaf ~ T_air, data = subset(subdata, Licor_Type == "LI-6400XT"))
  cat("Linear model summary (LI-6400XT):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6400XT):\n")
  print(confint(z))
  
  cat("\n\n")
  
  z = lm(T_leaf ~ T_air, data = subset(subdata, Licor_Type == "LI-6800"))
  cat("Linear model summary (LI-6800):")
  print(summary(z))
  cat("\n95% confidence intervals (LI-6800):\n")
  print(confint(z))
  
  
  # Close file
  cat("\n\n\n")
  sink()
  
}
