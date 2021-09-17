### Figure 4 ###
# Growth chamber trials
make_fig4 = function() {
  
  # Read data
  growth_chamber_trials_all <- read.csv("data/growth_chamber_trials_all.csv")
  
  # Select datasets of interest 
  data6400 = subset(growth_chamber_trials_all, Licor == 6400)
  data6800 = subset(growth_chamber_trials_all, Licor == 6800)
  
  # Convert data to long format
  data6400 = data6400 %>% gather(above_below, T_air_meas, T_below:T_above, factor_key = T)
  data6800 = data6800 %>% gather(above_below, T_air_meas, T_below:T_above, factor_key = T)

  # Open file
  pdf("figures/fig4.pdf", width = 7, height = 7)
  
  # Build plots
  p1 = ggplot(data = data6400, aes(x=T_air,y=T_air_meas,color=above_below)) + 
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
  
  p2 = ggplot(data = data6800, aes(x=T_air,y=T_air_meas,color=above_below)) + 
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
  
  p3 = ggplot(data = data6400, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
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
  
  p4 = ggplot(data = data6800, aes(x=T_air,y=T_air_meas-T_air,color=above_below)) + 
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
  
  # Close file
  dev.off()
}
