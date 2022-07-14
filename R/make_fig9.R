### Figure 9 ###
# Temperature dependence of Vcmax and Jmax
make_fig9 = function() {

  # Read in all A-Ci data
  aci_uncorr = read.csv("data/aci_by_temp_all.csv")
  aci_uncorr$total.leaf.area = aci_uncorr$Area
  
  # Correct data using new function
  aci_corr = correct_licor6400(aci_uncorr)
  
  # Fit FvCB model to corrected A-Ci curves and extract Vcmax Jmax
  aci_fits_corr = fitacis(data = aci_corr, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_corr = coef(aci_fits_corr)
  Tleaf_means_corr = aci_corr %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf))
  kinetics_extracted_corr = merge(kinetics_extracted_corr, Tleaf_means_corr, by = "curveID")
  kinetics_extracted_corr$type = "Corrected"
  
  # Fit FvCB model to uncorrected A-Ci curves and extract Vcmax Jmax
  aci_fits_uncorr = fitacis(data = aci_uncorr, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_uncorr = coef(aci_fits_uncorr)
  Tleaf_means_uncorr = aci_uncorr %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf))
  kinetics_extracted_uncorr = merge(kinetics_extracted_uncorr, Tleaf_means_uncorr, by = "curveID")
  kinetics_extracted_uncorr$type = "Uncorrected"
  
  # Bind all data together
  kinetics_all = rbind(kinetics_extracted_corr, kinetics_extracted_uncorr)
  
  # Define Arrhenius temperature response function
  arrhenius = function(k25, Ea, TC, R = 8.31446) { return(k25*exp( (Ea*(TC+273.15) - Ea*298.15) / (298.15*R*(TC+273.15)) )) }
  
  # Fit Arrhenius function to Vcmax and Jmax for uncorrected and corrected datasets
  vcmax_corr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), data = kinetics_extracted_corr,start = list(k25 = 66, Ea = 50000))
  jmax_corr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), data = kinetics_extracted_corr,start = list(k25 = 66, Ea = 50000))
  vcmax_uncorr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), data = kinetics_extracted_uncorr,start = list(k25 = 66, Ea = 50000))
  jmax_uncorr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), data = kinetics_extracted_uncorr, start = list(k25 = 66, Ea = 50000))
  
  # Make dataset for model predictions to plot
  temps_uncorr = seq(min(kinetics_extracted_uncorr$Tleaf),max(kinetics_extracted_uncorr$Tleaf), 0.1)
  temps_corr = seq(min(kinetics_extracted_corr$Tleaf),max(kinetics_extracted_corr$Tleaf), 0.1)
  
  # Fit SS models to the Vcmax and Jmax data
  mod = "pawar_2018"
  start_vals = get_start_vals(kinetics_extracted_corr$Tleaf, kinetics_extracted_corr$Vcmax, model_name = mod)
  low_lims = get_lower_lims(kinetics_extracted_corr$Tleaf, kinetics_extracted_corr$Vcmax, model_name = mod)
  upper_lims = get_upper_lims(kinetics_extracted_corr$Tleaf, kinetics_extracted_corr$Vcmax, model_name = mod)
  
  start_vals = c(r_tref=50, e = 0.5, topt = 30)
  
  v_corr_model = data.frame(Vcmax = predict(vcmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  v_uncorr_model = data.frame(Vcmax = predict(vcmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  vcmax_model = rbind(v_corr_model, v_uncorr_model)
  
  j_corr_model = data.frame(Jmax = predict(jmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  j_uncorr_model = data.frame(Jmax = predict(jmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  jmax_model = rbind(j_corr_model, j_uncorr_model)
  
  # Open file
  pdf("figures/fig9.pdf", width = 7.5, height = 3.5)
  
  # Build plots
  p1 = ggplot(data = kinetics_all, aes(x = Tleaf, y = Vcmax, fill = type, color = type)) +
    scale_fill_manual(values = rev(palette_c)) +
    scale_colour_manual(values = rev(palette_c)) +  
    geom_line(data = vcmax_model, aes(color=type)) +
    geom_point(aes(fill = type), size=2.4, color = "black", shape = 21) +
    my_theme +
    theme(legend.position = c(0.2,0.8)) +
    theme(legend.title = element_blank()) +
    xlab("Leaf temperature (ºC)") +
    ylab(expression(paste(V[cmax]~~(µmol~m^-2~s^-1 )))) +
    annotate("text", x = 17.5, y = 150, label = "(a)")
  
  p2 = ggplot(data = kinetics_all, aes(x = Tleaf, y = Jmax, fill = type, color = type)) +
    scale_fill_manual(values = rev(palette_c)) +
    scale_colour_manual(values = rev(palette_c)) +  
    geom_line(data = jmax_model, aes(color=type)) +
    geom_point(aes(fill = type), size=2.4, color = "black", shape = 21) +
    my_theme +
    xlab("Leaf temperature (ºC)") +
    ylab(expression(paste(J[max]~~(µmol~m^-2~s^-1 )))) +
    annotate("text", x = 17.5, y = 160, label = "(b)")
  
  grid.arrange(p1, p2, ncol = 2)
  
  # Close file
  dev.off()
  
  
  # Output associated statistics
  
  # Open file
  sink("stats.txt", append = T)
  cat("===================================\n")
  cat("Statistics associated with Fig. 9:\n")
  cat("===================================\n\n")

  # Significance tests
  kinetics_all$type = as.factor(kinetics_all$type)

  # Fit Arrhenius model to Vcmax data, allowing all parameters to vary
  a1 = nls(Vcmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))

  # Fit Arrhenius model to Vcmax, holding k25 constant
  a2 = nls(Vcmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))

  # Fit Arrhenius model to Vcmax, holding Ea constant
  a3 = nls(Vcmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))

  cat("\nVcmax best fit parameters:")
  print(summary(a1))

  cat("\n\nAnova - significant difference in k25 (Vcmax)?\n")
  print(anova(a1,a2))
  cat("\n\nAnova - significant difference in Ea (Vcmax)?\n")
  print(anova(a1,a3))
  
  # Fit Arrhenius model to Jmax, allowing all parameters to vary
  b1 = nls(Jmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))

  # Fit Arrhenius model to Jmax, holding k25 constant
  b2 = nls(Jmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))

  # Fit Arrhenius model to Jmax, holding Ea constant
  b3 = nls(Jmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))
  
  cat("\nJmax best fit parameters:")
  print(summary(b1))

  cat("\n\nAnova - significant difference in k25 (Jmax)?\n")
  print(anova(b1,b2))
  cat("\n\nAnova - significant difference in Ea (Jmax)?\n")
  print(anova(b1,b3))

  # Close file
  cat("\n\n\n")
  sink()
  
}
