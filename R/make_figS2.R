# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure S2: Showing thermocouple "calibration" - agreement between thermocouples
#
# Last edited 13 July 2022, Josef Garen

make_figS2 = function () {
  
  # Read data
  tc_calib_all <- read.csv("data/tc_calib_all.csv")
  
  # Open file
  pdf("figures/figS2.pdf", width = 5, height = 5)
  
  # Build plot
  p1 = ggplot(data = tc_calib_all, aes(x = Sous_vide, y = Value, fill = Type)) +
    geom_point(pch = 21, size = 2.4) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    my_theme +
    xlab("Water temperature setpoint (ºC)") +
    ylab("Thermocouple reported temperature (ºC)") +
    xlim(c(4,47)) + ylim(c(4,47)) +
    theme(legend.position = c(0.15,0.6)) +
    theme(legend.title = element_blank()) +
    theme(legend.background = element_rect(fill="transparent"))
  
  grid.arrange(p1, ncol = 1)
  
  # Close file
  dev.off()
  
}


