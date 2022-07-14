# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure S3: Energy balance calculations
#
# Last edited 13 July 2022, Josef Garen

make_figS3 = function() {

  Tmerged = read.csv("data/eb_6400.csv")
  
  # compute Ebal w/ tair lower
  Tmerged$total.leaf.area = Tmerged$Area
  
  Tmerged$T_air_licor = Tmerged$Tair
  Tmerged$T_leaf_licor = Tmerged$Tleaf
  
  Tmerged$Tleaf = Tmerged$T_air_below
  Tmerged$`EBal?` = 1
  
  Trecalc = calc_licor(Tmerged)
  
  # compare Tleaf, Tleaf,lower, and TleafEbal; plot it up
  Trecalc$UniqueID = as.factor(Trecalc$UniqueID)
  
  pdf("figures/figS3.pdf", width = 4, height = 3.5)
  
  p1 = ggplot(data=Trecalc, aes(x=T_leaf_below, y=CTleaf)) +
      scale_shape_manual(values = c(21,22,23,24)) +
      geom_abline(slope= 1, lty=2) + 
      geom_smooth(method=lm, color = palette_a[1]) +
      geom_point(aes(shape=UniqueID), size=2.4, fill = palette_a[1]) +
      my_theme +
      xlim(8,49) +
      ylim(8,49) +
      ylab(expression(paste("Computed leaf temperature (", degree, "C)"))) + 
      xlab(expression(paste("Abaxial leaf temperature (T"["leaf,lower"], "; ",degree, "C)"))) #+
  
  grid.arrange(p1, ncol = 1)
  
  # Close file
  dev.off()
  
  # Output associated statistics
  
  # Open file
  sink("stats_supplement.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. S1:\n")
  cat("===================================\n\n")
  
  z = lm(CTleaf ~ T_leaf_below, data = Trecalc)
  cat("Linear model summary:")
  print(summary(z))
  cat("\n95% confidence intervals:\n")
  print(confint(z))
  
  # Close file
  cat("\n\n\n")
  sink()
}
