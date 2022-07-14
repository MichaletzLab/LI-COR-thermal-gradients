# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure S1: Supplemental figure showing agreement of leaf and threaded thermocouple
#
# Last edited 13 July 2022, Josef Garen

make_figS1 = function() {
  
  # Read in data
  Tleaf_error_trials_all = read.csv("data/Tleaf_error_thread.csv")
  
  # Clean up label names
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"
  
  data_thread = Tleaf_error_trials_all
  
  # Open file
  pdf("figures/figS1.pdf", width = 4, height = 4)
  
  # Build plot
  p1 = ggplot(data=data_thread, aes(x=T_thread, y=T_below)) +
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

  grid.arrange(p1, ncol = 1)
  
  # Close file
  dev.off()
  
  # Output associated statistics
  
  # Open file
  sink("stats_supplement.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. S1:\n")
  cat("===================================\n\n")
  
  z = lm(T_below ~ T_thread, data = data_thread)
  cat("Linear model summary:")
  print(summary(z))
  cat("\n95% confidence intervals:\n")
  print(confint(z))
  
  # Close file
  cat("\n\n\n")
  sink()
  
}
