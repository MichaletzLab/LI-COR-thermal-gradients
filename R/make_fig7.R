# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 7: Limited homeothermy
#
# Last edited 13 July 2022, Josef Garen

make_fig7 = function () {
  
  licor_means = read.csv("data/lh_6400.csv")
  
  # Correct the leaf and air temperatures using new function
  licor_means$total.leaf.area = licor_means$Area
  licor_corrected = correct_licor6400(licor_means)
  
  # Fit linear models to each curve in the dataset (uncorrected T values)
  tbl_models = licor_corrected %>% 
    group_by(uniqueid) %>% 
    do(model = lm(Tleaf_uncorrected ~ Tair_uncorrected, data = .))
  
  # Extract linear model coefficients
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$uniqueid[i] = tbl_models$uniqueid[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef_uncorrected = data.frame(model_coef)

  # Fit linear model to each curve in the dataset (corrected T values)
  tbl_models = licor_corrected %>% 
    group_by(uniqueid) %>% 
    do(model = lm(Tleaf ~ Tair, data = .))
  
  # Extract model coefficients
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$uniqueid[i] = tbl_models$uniqueid[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef_corrected = data.frame(model_coef)
  
  # Open file
  pdf("figures/fig7.pdf", width = 7, height = 3.5, onefile=F)
  
  # Make main plot
  main_plot = ggplot(data = licor_corrected, aes(x = Tair_uncorrected, y = Tleaf_uncorrected)) + 
    geom_point(size = 0.8) + 
    geom_smooth(method=lm, fill=NA, aes(group=uniqueid), lwd=0.3, color = palette_c[1]) +
    scale_color_manual(values=c("blue","red")) +
    geom_abline(slope = 1, lty=2) +
    my_theme +
    xlab("Uncorrected in-chamber air temperature (ºC)") +
    ylab("Uncorrected leaf temperature (ºC)") +
    xlim(c(2,51)) + ylim(c(2,51)) +
    annotate("text", x = 2, y = 51, label = "(a)")
  
  # Make inset - slope distribution
  inset_plot = ggplotGrob(
    ggplot(data = model_coef_uncorrected, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0.4,1.0)) +
      ylim(c(0,22)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p1 = main_plot + annotation_custom(grob = inset_plot, xmin = 0, xmax = 27, ymin = 29, ymax = 50)
  
  # Build main plot
  main_plot_2 = ggplot(data = licor_corrected, aes(x = Tair, y = Tleaf)) + 
    geom_point(size = 0.8) + 
    geom_smooth(method=lm, fill=NA, aes(group=uniqueid), lwd=0.3, color = palette_c[2]) +
    scale_color_manual(values=c("blue","red")) +
    geom_abline(slope = 1, lty=2) +
    my_theme +
    xlab("Corrected in-chamber air temperature (ºC)") +
    ylab("Corrected leaf temperature (ºC)") +
    xlim(c(2,51)) + ylim(c(2,51)) +
    annotate("text", x = 2, y = 51, label = "(b)")
  
  # Build inset plot
  inset_plot_2 = ggplotGrob(
    ggplot(data = model_coef_corrected, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0.4,1.0)) + ylim(c(0,22)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p2 = main_plot_2 + annotation_custom(grob = inset_plot_2, xmin = 0, xmax = 27, ymin = 29, ymax = 50)
  grid.arrange(p1, p2, ncol = 2)
  
  # Close file
  dev.off()
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 7:\n")
  cat("===================================\n\n")
  
  # Mean, stderr, and variance of slope values in uncorrected dataset
  print("Mean slope, uncorrected:")
  print(mean(model_coef_uncorrected$slope))
  print("Std. error, uncorrected:")
  print(sd(model_coef_uncorrected$slope)/sqrt(length(model_coef_uncorrected$slope)))
  print("Variance, uncorrected:")
  print(var(model_coef_uncorrected$slope))
  
  # Mean, stderr, and variance of slope values in corrected dataset
  print("Mean slope, corrected:")
  print(mean(model_coef_corrected$slope))
  print("Std. error, corrected:")
  print(sd(model_coef_corrected$slope)/sqrt(length(model_coef_corrected$slope)))
  print("Variance, corrected:")
  print(var(model_coef_corrected$slope))
  
  # Close file
  cat("\n\n\n")
  sink()
  
}
