### Figure S1 ###
# Supplemental figure showing agreement of leaf and threaded thermocouple

make_figS1 = function() {
  
  # Read in data
  Tleaf_error_trials_all <- read.csv("data/Tleaf_error_trials_all.csv")
  
  # Clean up label names
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"
  
  
  data_thread = subset(Tleaf_error_trials_all, !is.na(T_thread))
  
  pdf("figures/figS1.pdf", width = 4, height = 4)
  
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
  dev.off()
  
}
