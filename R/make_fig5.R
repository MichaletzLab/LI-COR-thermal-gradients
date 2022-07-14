# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 5: Tleaf error estimates
#
# Last edited 13 July 2022, Josef Garen

make_fig5 = function() {
  
  # Read data
  Tleaf_error_trials_all <- read.csv("data/Tleaf_error_trials_all.csv")

  # Clean up label names
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6400"] = "LI-6400XT"
  Tleaf_error_trials_all$Licor_Type[Tleaf_error_trials_all$Licor_Type == "6800"] = "LI-6800"

  Tleaf_error_trials_all$UniqueID = as.factor(Tleaf_error_trials_all$UniqueID)
  
  # Compute average above and below leaf temperatures
  Tleaf_error_trials_all$T_leaf_mean = (Tleaf_error_trials_all$T_leaf_above + Tleaf_error_trials_all$T_leaf_below)/2
  Tleaf_error_trials_all$T_air_mean = (Tleaf_error_trials_all$T_air_above + Tleaf_error_trials_all$T_air_below)/2

  # Remove any missing data
  Tleaf_error_trials_all = subset(Tleaf_error_trials_all, !is.na(T_leaf_mean))
  
  # Compute differences used in subsequent models
  Tleaf_error_trials_all$a = Tleaf_error_trials_all$T_leaf-Tleaf_error_trials_all$T_leaf_below
  Tleaf_error_trials_all$b = Tleaf_error_trials_all$T_air-Tleaf_error_trials_all$T_leaf
  Tleaf_error_trials_all$c = Tleaf_error_trials_all$T_air_below-Tleaf_error_trials_all$T_leaf_below
  Tleaf_error_trials_all$d = Tleaf_error_trials_all$T_air-Tleaf_error_trials_all$T_air_mean
  Tleaf_error_trials_all$e = Tleaf_error_trials_all$T_block-Tleaf_error_trials_all$T_air

  # Make model predictions for plot a
  li6400.lme.a = lme(a ~ b, random = ~ b|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT"))
  li6800.lme.a = lme(a ~ b, random = ~ b|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6800"))
  
  li6400 = subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT")
  b = seq(min(li6400$b), max(li6400$b), 0.1)
  a = fixef(li6400.lme.a)[1] + b*fixef(li6400.lme.a)[2]
  model_plot_a = data.frame(a,b,Licor_Type = "LI-6400XT")
  
  li6800 = subset(Tleaf_error_trials_all, Licor_Type == "LI-6800")
  b = seq(min(li6800$b), max(li6800$b), 0.1)
  a = fixef(li6800.lme.a)[1] + b*fixef(li6800.lme.a)[2]
  model_plot_a = rbind(model_plot_a,data.frame(a,b,Licor_Type="LI-6800"))
  
  
  # Make model predictions for plot b
  li6400.lme.b = lme(a ~ c, random = ~ c|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT"))
  li6800.lme.b = lme(a ~ c, random = ~ c|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6800" & !is.na(c)), control = lmeControl(msMaxIter = 1000, msMaxEval = 1000))
  
  li6400 = subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT")
  c = seq(min(li6400$c), max(li6400$c), 0.1)
  a = fixef(li6400.lme.b)[1] + c*fixef(li6400.lme.b)[2]
  model_plot_b = data.frame(a,c,Licor_Type = "LI-6400XT")
  
  li6800 = subset(Tleaf_error_trials_all, Licor_Type == "LI-6800")
  c = seq(min(li6800$c, na.rm = T), max(li6800$c, na.rm = T), 0.1)
  a = fixef(li6800.lme.b)[1] + c*fixef(li6800.lme.b)[2]
  model_plot_b = rbind(model_plot_b,data.frame(a,c,Licor_Type="LI-6800"))
  
  
  # Build models for Tair correction
  li6400.lme.c = lme(d ~ e, random = ~ e|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6400XT"))
  li6800.lme.c = lme(d ~ e, random = ~ e|UniqueID, subset(Tleaf_error_trials_all, Licor_Type == "LI-6800" & !is.na(d)))
  
  pdf("figures/fig5.pdf", width = 7, height = 3.5)

  # Build plots  
  p1 = ggplot(data=Tleaf_error_trials_all, aes(x=b, y=a, color=Licor_Type)) +
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_abline(slope= 0, lty=2) + 
    geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape = 21) +
    geom_line(data = model_plot_a, lwd=1.9, aes(group = Licor_Type),color = "black") +
    geom_line(data = model_plot_a, lwd=1.1) +
    my_theme +
    theme(legend.position = c(0.2, 0.8)) +
    theme(legend.title = element_blank()) +
    xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)"))) + 
    ylab(expression(paste("Error in reported leaf temperature (", degree, "C)"))) +
    annotate("text", x = -6.5, y = 2.4, label = "(a)")
  
  p2 = ggplot(data=subset(Tleaf_error_trials_all, !is.na(c)), aes(x=c, y=a, color=Licor_Type)) +
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_abline(slope=0, lty=2)+ 
    geom_point(aes(fill = Licor_Type), size=2.4, color = "black", shape=21) +
    geom_line(data = model_plot_b, lwd=1.9, aes(group = Licor_Type),color = "black") +
    geom_line(data = model_plot_b, lwd=1.2) +
    my_theme +
    xlab(expression(paste("Measured air-leaf temperature difference (", degree, "C)"))) + 
    ylab(NULL) +
    annotate("text", x = -6.5, y = 2.2, label = "(b)")
  
  grid.arrange(p1, p2, ncol = 2)
  
  # Close file
  dev.off()
  
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 5:\n")
  cat("===================================\n\n")
 
  
  cat("Linear model summary (LI-6400XT, Tleaf error vs. Tair-Tleaf):")
  print(summary(li6400.lme.a))
  cat("\n95% confidence intervals (LI-6400XT, Tleaf error vs. Tair-Tleaf):\n")
  print(intervals(li6400.lme.a, which = "fixed"))
  
  cat("\n\n")
  
  cat("Linear model summary (LI-6800, Tleaf error vs. Tair-Tleaf):")
  print(summary(li6800.lme.a))
  cat("\n95% confidence intervals (LI-6800, Tleaf error vs. Tair-Tleaf):\n")
  print(intervals(li6800.lme.a, which = "fixed"))
  
  cat("\n\n")
  
  cat("Linear model summary (LI-6400XT, Tleaf error vs. Tair,lower-Tleaf,lower):")
  print(summary(li6400.lme.b))
  cat("\n95% confidence intervals (LI-6400XT, Tleaf error vs. Tair,lower-Tleaf,lower):\n")
  print(intervals(li6400.lme.b, which = "fixed"))
  
  cat("\n\n")
  
  cat("Linear model summary (LI-6800, Tleaf error vs. Tair,lower-Tleaf,lower):")
  print(summary(li6800.lme.b))
  cat("\n95% confidence intervals (LI-6800, Tleaf error vs. Tair,lower-Tleaf,lower):\n")
  print(intervals(li6800.lme.b, which = "fixed"))
  
  cat("\n\n")
  
  cat("Linear model summary (LI-6400XT, Tair error vs. Tblock-Tair):")
  print(summary(li6400.lme.c))
  cat("\n95% confidence intervals (LI-6400XT, Tair error vs. Tblock-Tair):\n")
  print(intervals(li6400.lme.c, which = "fixed"))
  
  cat("Linear model summary (LI-6800, Tair error vs. Tblock-Tair):")
  print(summary(li6800.lme.c))
  cat("\n95% confidence intervals (LI-6800, Tair error vs. Tblock-Tair):\n")
  print(intervals(li6800.lme.c, which = "fixed"))
  
  # Close file
  cat("\n\n\n")
  sink()
}
