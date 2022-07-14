# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 6: Error in derived quantities
#
# Last edited 13 July 2022, Josef Garen

make_fig6 = function() {
  
  # Read in datasets
  data6400 = read.csv("data/error_propagation_6400.csv")
  data6800 = read.csv("data/error_propagation_6800.csv")

  # Correct Tleaf and derived values
  data6400$total.leaf.area = data6400$chamber_leaf_area
  data6400_corr = correct_licor6400(data6400)
  data6800_corr = correct_licor6800(data6800)
  
  # Compute relative error and assemble dataframes for plots
  error6400 = data.frame(
    error = c((data6400$Cond - data6400_corr$Cond) / data6400_corr$Cond,
              (data6400$CndTotal - data6400_corr$CndTotal) / data6400_corr$CndTotal,
              (data6400$CndCO2 - data6400_corr$CndCO2) / data6400_corr$CndCO2,
              (data6400$Ci - data6400_corr$Ci) / data6400_corr$Ci),
    Tair = data6400$Tair,
    Tleaf = data6400$Tleaf,
    variable = c(rep("g_sw", 200), rep("g_tw", 200), rep("g_tc", 200), rep("C_i", 200)),
    Licor_Type = "LI-6400XT"
  )
  
  error6800 = data.frame(
    error = c((data6800$gsw - data6800_corr$gsw) / data6800_corr$gsw,
              (data6800$gtw - data6800_corr$gtw) / data6800_corr$gtw,
              (data6800$gtc - data6800_corr$gtc) / data6800_corr$gtc,
              (data6800$Ci - data6800_corr$Ci) / data6800_corr$Ci),
    Tair = data6800$Tair,
    Tleaf = data6800$Tleaf,
    variable = c(rep("g_sw", 200), rep("g_tw", 200), rep("g_tc", 200), rep("C_i", 200)),
    Licor_Type = "LI-6800"
  )
  
  error_all = bind_rows(error6400, error6800)
  
  # Trim a couple of outliers for plotting purposes
  error_all = subset(error_all, error < 0.5)
  
  # Open file
  pdf("figures/fig6.pdf", width = 7, height = 3.5)
  
  # Make label text
  dat_text <- data.frame(
    labels = c("(c) ~~ italic(g[sw])", "(e) ~~ italic(g[tw])", "(d) ~~ italic(g[tc])", "(b) ~~ italic(C[i])"),
    variable = c("g_sw","g_tw","g_tc","C_i"),
    Tair = 0,
    Tleaf = 0,
    error = 0,
    Licor_Type = "LI-6400XT"
  )
  
  # Build plots
  p1 = ggplot(data = error_all, aes(x = variable, y=100*error, fill=Licor_Type)) + 
    scale_fill_manual(values = palette_a) +
    geom_boxplot() +
    ylab("Relative error (%)") + xlab(NULL) + 
    scale_x_discrete(labels=c("g_sw" = expression(italic("g"[sw])), 
                              "g_tw" = expression(italic("g"[tw])), 
                              "g_tc" = expression(italic("g"[tc])),
                              "C_i" = expression(italic("C"[i])))) +
    my_theme +
    theme(legend.position = c(0.17, 0.12)) +
    theme(legend.title = element_blank()) +
    theme(legend.background=element_blank()) +
    ylim(c(-50,50)) +
    geom_abline(slope = 0, lty = 2) +
    annotate("text", x = -Inf, y = 0.50*100, hjust = -0.1, label = "(a)") +
    theme(axis.text.x = element_text(face="bold",size=12))
  
  p2 = ggplot(data = error_all, aes(x = Tair-Tleaf, y=100*error, color=Licor_Type)) +
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_point(size = 0.8) +
    facet_wrap(~ variable, ncol = 2) +
    my_theme + 
    theme(strip.background = element_blank(), strip.text = element_blank()) +
    xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)  "))) + 
    ylab("Relative error (%)") +
    guides(colour = "none") +
    ylim(c(-0.50*100, 0.50*100)) +
    geom_abline(slope = 0, lty = 2) +
    geom_text(data = dat_text, aes(x = -Inf, y = Inf, label = labels), color = "black", hjust = -0.1, vjust = 1.3, parse=T)
  
  grid.arrange(p1,p2, ncol = 2)
  
  # Close file
  dev.off()
  
  
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 6:\n")
  cat("===================================\n\n")
  

  cat("LI-6400XT, g_sw:\n")
  print(summary(subset(error6400, variable == "g_sw")$error))
  cat("SD:")
  print(sd(subset(error6400, variable == "g_sw")$error))
  
  cat("\nLI-6400XT, g_tw:\n")
  print(summary(subset(error6400, variable == "g_tw")$error))
  cat("SD:")
  print(sd(subset(error6400, variable == "g_tw")$error))
  
  cat("\nLI-6400XT, g_tc:\n")
  print(summary(subset(error6400, variable == "g_tc")$error))
  cat("SD:")
  print(sd(subset(error6400, variable == "g_tc")$error))
  
  cat("\nLI-6400XT, C_i:\n")
  print(summary(subset(error6400, variable == "C_i")$error))
  cat("SD:")
  print(sd(subset(error6400, variable == "C_i")$error))
  
  cat("\nLI-6800, g_sw:\n")
  print(summary(subset(error6800, variable == "g_sw")$error))
  cat("SD:")
  print(sd(subset(error6800, variable == "g_sw")$error))
  
  cat("\nLI-6800, g_tw:\n")
  print(summary(subset(error6800, variable == "g_tw")$error))
  cat("SD:")
  print(sd(subset(error6800, variable == "g_tw")$error))
  
  cat("\nLI-6800, g_tc:\n")
  print(summary(subset(error6800, variable == "g_tc")$error))
  cat("SD:")
  print(sd(subset(error6800, variable == "g_tc")$error))
  
  cat("\nLI-6800, C_i:\n")
  print(summary(subset(error6800, variable == "C_i")$error))
  cat("SD:")
  print(sd(subset(error6800, variable == "C_i")$error))
  
  # Close file
  cat("\n\n\n")
  sink()
}



