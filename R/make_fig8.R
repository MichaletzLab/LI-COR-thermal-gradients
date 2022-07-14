# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Figure 8: Correcting an AT curve
#
# Last edited 13 July 2022, Josef Garen
make_fig8 = function () {

  # Read in data
  AT_uncorrected = read.csv("data/lh_6400.csv")
  AT_uncorrected$total.leaf.area = AT_uncorrected$Area
  
  # Select first curve
  AT_uncorrected = subset(AT_uncorrected, uniqueid == 1)

  # Correct raw LI-COR data using new function
  AT_corrected = correct_licor6400(AT_uncorrected)

  # Add labels
  AT_uncorrected$Condition = "Uncorrected"
  AT_corrected$Condition = "Corrected"
  
  # Fit Pawar model (equivalent to Sharpe-Schoolfield with high temperature deactivation) 
  # Grab starting limits
  mod = "pawar_2018"
  start_vals = get_start_vals(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  low_lims = get_lower_lims(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  upper_lims = get_upper_lims(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  
  # Fit model to uncorrected data
  fit_pawar_uncorr = nls_multstart(Photo~pawar_2018(temp = Tleaf, r_tref,e,eh,topt, tref=5),
                                   data = AT_uncorrected,
                                   iter = 1000,
                                   start_lower = start_vals - 10,
                                   start_upper = start_vals + 10,
                                   lower = low_lims,
                                   upper = upper_lims,
                                   supp_errors = 'Y')
  
  # Fit model to corrected data
  fit_pawar_corr = nls_multstart(Photo~pawar_2018(temp = Tleaf, r_tref,e,eh,topt, tref=5),
                                 data = AT_corrected,
                                 iter = 1000,
                                 start_lower = start_vals - 10,
                                 start_upper = start_vals + 10,
                                 lower = low_lims,
                                 upper = upper_lims,
                                 supp_errors = 'Y')
  
  # Make model predictions for plotting below from Pawar (SS) model
  # Grab parameters for uncorrected model fit
  tref = 5
  r_tref <- coef(fit_pawar_uncorr)["r_tref"]
  e <- coef(fit_pawar_uncorr)["e"]
  eh <- coef(fit_pawar_uncorr)["eh"]
  topt <- coef(fit_pawar_uncorr)["topt"]
  
  # Generate sequence of temperatures spanning range of data
  tmp_temps <- seq(min(AT_uncorrected$Tleaf), max(AT_uncorrected$Tleaf), length = 200)
  
  # Generate model predicted values
  pred_uncorr = data.frame(Temperature = tmp_temps,
                           TraitValue = pawar_2018(tmp_temps,r_tref,e,eh,topt,tref=5),
                           Condition = "Uncorrected")
  
  # Grab parameters for uncorrected model fit
  r_tref <- coef(fit_pawar_corr)["r_tref"]
  e <- coef(fit_pawar_corr)["e"]
  eh <- coef(fit_pawar_corr)["eh"]
  topt <- coef(fit_pawar_corr)["topt"]
  
  # Generate sequence of temperatures spanning range of data
  tmp_temps <- seq(min(AT_corrected$Tleaf), max(AT_corrected$Tleaf), length = 200)
  
  # Generate model predicted values
  pred_corr = data.frame(Temperature = tmp_temps,
                         TraitValue = pawar_2018(tmp_temps,r_tref,e,eh,topt,tref=5),
                         Condition = "Corrected")

  # Make dataframes for plotting
  ModelToPlotS = rbind(pred_uncorr, pred_corr)
  all_data = bind_rows( AT_corrected , AT_uncorrected)

  all_data$Condition = as.factor(all_data$Condition)
  ModelToPlotS$Condition = as.factor(ModelToPlotS$Condition)
  
  # Open file
  pdf("figures/fig8.pdf", width = 4, height = 3.5)
  
  # Build plot
  p1 = ggplot(data = all_data, aes(x = Tleaf, y = Photo, fill = Condition)) + 
    scale_fill_manual(values = palette_c[c(2,1)]) +
    scale_colour_manual(values = palette_c[c(2,1)]) +
    geom_line(data = ModelToPlotS, aes(x = Temperature, y = TraitValue, color = Condition)) +
    geom_point(aes(fill = Condition), size=2.4, color = "black", pch = 21) +
    my_theme +
    theme(legend.position = c(0.77,0.88)) +
    theme(legend.title = element_blank()) +
    theme(legend.background = element_rect(fill="transparent")) +
    xlab("Leaf temperature (ºC)") +
    ylab(expression(paste("Assimilation rate (µmol ", m^-2, s^-1, ")")))
  
  grid.arrange(p1, ncol=1)
  
  # Close file
  dev.off()
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 8:\n")
  cat("===================================\n\n")
  
  cat("Sharpe-Schoolfield best fit parameters, uncorrected data:")
  print(summary(fit_pawar_uncorr))
  print(confint2(fit_pawar_uncorr))
  
  cat("\n\nSharpe-Schoolfield best fit parameters, corrected data:")
  print(summary(fit_pawar_corr))
  print(confint2(fit_pawar_corr))
  
  # Fit modified Gaussian function
  mod = "modifiedgaussian_2006"
  start_vals = get_start_vals(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  low_lims = get_lower_lims(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  upper_lims = get_upper_lims(AT_uncorrected$Tleaf, AT_uncorrected$Photo, model_name = mod)
  
  fit_gaussian_uncorr = nls_multstart(Photo~modifiedgaussian_2006(temp = Tleaf, rmax, topt, a,b),
                                      data = AT_uncorrected,
                                      iter = 500,
                                      start_lower = start_vals - 10,
                                      start_upper = start_vals + 10,
                                      lower = low_lims,
                                      upper = upper_lims,
                                      supp_errors = 'Y')
  
  fit_gaussian_corr = nls_multstart(Photo~modifiedgaussian_2006(temp = Tleaf, rmax, topt, a,b),
                                    data = AT_corrected,
                                    iter = 500,
                                    start_lower = start_vals - 10,
                                    start_upper = start_vals + 10,
                                    lower = low_lims,
                                    upper = upper_lims,
                                    supp_errors = 'Y')
  
  cat("\n\nModified Gaussian best fit parameters, uncorrected data:")
  print(summary(fit_gaussian_uncorr))
  print(confint2(fit_gaussian_uncorr))
  
  cat("\n\nModified Gaussian best fit parameters, corrected data:")
  print(summary(fit_gaussian_corr))
  print(confint2(fit_gaussian_corr))
  
  # Join data for next tests
  all_data = bind_rows(AT_uncorrected, AT_corrected)
  all_data$Condition = as.factor(all_data$Condition)
  
  # Fit model allowing all parameters to vary by condition
  g1 = nls(Photo ~ gaussian_1987(temp = Tleaf, rmax[Condition], topt[Condition], a[Condition]),
           data = all_data,
           start = list(rmax = c(13,13),
                        topt = c(30,30),
                        a = c(14,14)))
  
  # Fit model, restricting rmax to be the same across conditions
  g2 = nls(Photo ~ gaussian_1987(temp = Tleaf, rmax, topt[Condition], a[Condition]),
           data = all_data,
           start = list(rmax = 13,
                        topt = c(30,30),
                        a = c(14,14)))
  
  # Fit model, restricting a (breadth) to be the same across conditions
  g3 = nls(Photo ~ gaussian_1987(temp = Tleaf, rmax[Condition], topt[Condition], a),
           data = all_data,
           start = list(rmax = c(13.5,13.5),
                        topt = c(29,29),
                        a = 18))
  
  cat("\n\nAnova - significant difference in rmax?\n")
  print(anova(g1,g2))
  cat("\n\nAnova - significant difference in a (breadth)?\n")
  print(anova(g1,g3))
  
  # Fit model allowing all parameters to vary by condition
  p1 = nls(Photo ~ pawar_2018(temp = Tleaf, r_tref[Condition], e[Condition], eh[Condition], topt[Condition], tref=5),
           data = all_data,
           start = list(r_tref = c(1,1),
                        e = c(0.5,0.5),
                        eh = c(1,1),
                        topt = c(23,23)))
  
  # Fit model with same e across conditions
  p2 = nls(Photo ~ pawar_2018(temp = Tleaf, r_tref[Condition], e, eh[Condition], topt[Condition], tref=5),
           data = all_data,
           start = list(r_tref = c(1,1),
                        e = 0.5,
                        eh = c(1,1),
                        topt = c(28,28)))
  
  # Fit model with same topt across conditions
  p3 = nls(Photo ~ pawar_2018(temp = Tleaf, r_tref[Condition], e[Condition], eh[Condition], topt, tref=5),
           data = all_data,
           start = list(r_tref = c(1,1),
                        e = c(0.5,0.5),
                        eh = c(1,1),
                        topt = 28))
  
  p4 = nls(Photo ~ pawar_2018(temp = Tleaf, r_tref[Condition], e[Condition], eh, topt[Condition], tref=5),
          data = all_data,
          start = list(r_tref = c(1,1),
                       e = c(0.5,0.5),
                       eh = 1,
                       topt = c(28,28)))
  
  cat("\n\nAnova - significant difference in e?\n")
  print(anova(p1,p2))
  cat("\n\nAnova - significant difference in topt?\n")
  print(anova(p1,p3))
  cat("\n\nAnova - significant difference in eh?\n")
  print(anova(p1,p4))
  
  # Close file
  cat("\n\n\n")
  sink()
  
}
