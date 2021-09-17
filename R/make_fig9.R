
### Figure 9 ###
# Correcting ACi curves

make_fig9 = function() {
  
      recalc_Ci <- function(test_params, m = 1, b = 0) {
        
        # Read out the variables we need
        E_cur = test_params$Trmmol/1000 #test_params$Trans       # Transpiration rate
        TleafC_cur = test_params$Tleaf  # Leaf Temperature C
        TairC_cur = test_params$Tair    # Air Temperature C
        P_cur = test_params$Press       # Air pressure
        W_s_cur = test_params$H2OS      # H20 in sample
        g_bw_cur = test_params$BLC_1    # Boundary layer conductance
        K_cur = test_params$StmRat      # Stomatal ratio
        A_cur = test_params$Photo       # Photosynthetic rate
        C_s_cur = test_params$CO2S      # CO2 sample
        
        # Compute current T_leaf error under current conditions
        T_error = m*(TairC_cur-TleafC_cur)+b
        
        # Recompute conductance and Ci
        g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
        g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur)
        g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
        C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
        
        return(C_i_a)
      }
      
      
      aci_all = read.csv("data/aci_by_temp_all.csv")
      Ci_corr = recalc_Ci(aci_all, m = 0.7838, b = 0.8092)
      aci_all$Ci_corr = Ci_corr
      
      m = 0.7838
      b = 0.8092
      T_error = m*(aci_all$Tair-aci_all$Tleaf)+b
      aci_all$Tleaf_corr = aci_all$Tleaf-T_error
      
      # Vcmax and Jmax values
      plot_model_14 = c()
      
      a = subset(aci_all, T_setpoint == 14)
      b = fitaci(a, Tcorrect = F)
      confint(b$nlsfit)
      c = b$Photosyn(Ca = 1*(60:2500))
      c$Type = "Uncorrected"
      plot_model_14 = c
      
      b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
      confint(b$nlsfit)
      c = b$Photosyn(Ca = 1*(60:2500))
      c$Type = "Corrected"
      plot_model_14 = rbind(plot_model_14,c)
      
      plot_model_38 = c()
      
      a = subset(aci_all, T_setpoint == 38)
      b = fitaci(a, Tcorrect = F)
      confint(b$nlsfit)
      c = b$Photosyn(Ca = 1*(52:1921))
      c$Type = "Uncorrected"
      plot_model_38 = c
      
      b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
      confint(b$nlsfit)
      c = b$Photosyn(Ca = 1*(52:2022))
      c$Type = "Corrected"
      plot_model_38 = rbind(plot_model_38,c)
      
      aci_plot = rbind(
        data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci, Type = "Uncorrected", T_setpoint = aci_all$T_setpoint),
        data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci_corr, Type = "Corrected", T_setpoint = aci_all$T_setpoint))
      
      #aci_plot = subset(aci_plot, T_setpoint == 14 | T_setpoint == 38)
      
      aci_plot$T_setpoint = as.factor(aci_plot$T_setpoint)
      
      pdf("figures/fig9.pdf", width = 7.5, height = 3.5)
      
      aci_cut = subset(aci_plot, T_setpoint == 14)
      
      p1 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
        scale_fill_manual(values = rev(palette_c)) +
        scale_colour_manual(values = rev(palette_c)) +  
        geom_line(data = plot_model_14, aes(x=Ci, y=ALEAF,color=Type),lwd=0.4) +
        geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
        my_theme +
        xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
        ylab("Net assimilation (µmol/m²s)") +
        xlim(c(0,1700)) + ylim(c(-2,27)) +
        annotate("text", x = 300, y = 27, label = expression(paste("(a) ", italic("T"[paste("block,LI")]), " = 14ºC")))
      
      aci_cut = subset(aci_plot, T_setpoint == 38)
      #fitaci(data = aci_cut)
      #fitaci(data = aci_cut, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",
      #                                       PPFD = "PARi", Rd = "Rd"))
      
      p2 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
        scale_fill_manual(values = rev(palette_c)) +
        scale_colour_manual(values = rev(palette_c)) +
        geom_line(data = plot_model_38, aes(x=Ci, y=ALEAF), lwd=0.4) +
        geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
        my_theme +
        xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
        ylab("Net assimilation (µmol/m²s)") +
        theme(legend.position = c(0.8,0.15)) +
        theme(legend.title = element_blank()) +
        xlim(c(0,1700)) + ylim(c(-2,27)) +
        annotate("text", x = 300, y = 27, label = expression(paste("(b) ", italic("T"[paste("block,LI")]), " = 38ºC")))
      
      grid.arrange(p1, p2, ncol = 2)
      dev.off()
  
  
  # Let's look at Vcmax and Jmax temperature response
  aci_fits_corr = fitacis(data = aci_all, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf_corr", Ci = "Ci_corr", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_corr = coef(aci_fits_corr)
  Tleaf_means_corr = aci_all %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf_corr))
  kinetics_extracted_corr = merge(kinetics_extracted_corr, Tleaf_means_corr, by = "curveID")
  kinetics_extracted_corr$type = "Corrected"
  
  aci_fits_uncorr = fitacis(data = aci_all, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_uncorr = coef(aci_fits_uncorr)
  Tleaf_means_uncorr = aci_all %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf))
  kinetics_extracted_uncorr = merge(kinetics_extracted_uncorr, Tleaf_means_uncorr, by = "curveID")
  kinetics_extracted_uncorr$type = "Uncorrected"
  
  kinetics_all = rbind(kinetics_extracted_corr, kinetics_extracted_uncorr)
  
  # Now we need to fit an arrhenius model
  arrhenius = function(k25, Ea, TC, R = 8.31446) {return(k25*exp( (Ea*(TC+273.15) - Ea*298.15) / (298.15*R*(TC+273.15)) ))}
  
  vcmax_corr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), 
                   data = kinetics_extracted_corr,
                   start = list(k25 = 66, Ea = 50000))
  
  jmax_corr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), 
                  data = kinetics_extracted_corr,
                  start = list(k25 = 66, Ea = 50000))
  
  vcmax_uncorr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), 
                     data = kinetics_extracted_uncorr,
                     start = list(k25 = 66, Ea = 50000))
  
  jmax_uncorr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), 
                    data = kinetics_extracted_uncorr,
                    start = list(k25 = 66, Ea = 50000))
  
  # Estimate Q10
  kc = kinetics_extracted_corr
  ku = kinetics_extracted_uncorr
  q10 = function(r1, r2, t1, t2) {
    return( (r2/r1)^(10/(t2-t1)) )
  }
  jmax_q10_corr = q10(kc$Jmax[1], kc$Jmax[5], kc$Tleaf[1], kc$Tleaf[5])
  jmax_q10_uncorr = q10(ku$Jmax[1], ku$Jmax[5], ku$Tleaf[1], ku$Tleaf[5])
  vcmax_q10_corr = q10(kc$Vcmax[1], kc$Vcmax[5], kc$Tleaf[1], kc$Tleaf[5])
  vcmax_q10_uncorr = q10(ku$Vcmax[1], ku$Vcmax[5], ku$Tleaf[1], ku$Tleaf[5])
  
  
  
  
  # Test of a new significance thingie
  kinetics_all$type = as.factor(kinetics_all$type)
  
  a1 = nls(Vcmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))
  
  a2 = nls(Vcmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))
  
  a3 = nls(Vcmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))
  
  anova(a1,a2)
  anova(a1,a3)
  
  b1 = nls(Jmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))
  
  b2 = nls(Jmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))
  
  b3 = nls(Jmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))
  
  anova(b1,b2)
  anova(b1,b3)
  
  
  
  
  temps_uncorr = seq(17.8, 32.4, 0.2)
  temps_corr = seq(18.6,29, 0.2)
  v_corr_model = data.frame(Vcmax = predict(vcmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  v_uncorr_model = data.frame(Vcmax = predict(vcmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  vcmax_model = rbind(v_corr_model, v_uncorr_model)
  
  j_corr_model = data.frame(Jmax = predict(jmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  j_uncorr_model = data.frame(Jmax = predict(jmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  jmax_model = rbind(j_corr_model, j_uncorr_model)
  
  
  pdf("figures/fig9a.pdf", width = 7.5, height = 3.5)
  
  # Plot 1 vcmax
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
  
  #ylab(expression(paste("Assimilation rate (µmol ", m^-2, s^-1, ")")))
  
  # Plot 2 Jmax
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
  dev.off()

}

#
#
#
#
#
#
#
#
#






make_fig9a = function() {
  
  recalc_Ci <- function(test_params, m = 1, b = 0) {
    
    # Read out the variables we need
    E_cur = test_params$Trmmol/1000 #test_params$Trans       # Transpiration rate
    TleafC_cur = test_params$Tleaf  # Leaf Temperature C
    TairC_cur = test_params$Tair    # Air Temperature C
    P_cur = test_params$Press       # Air pressure
    W_s_cur = test_params$H2OS      # H20 in sample
    g_bw_cur = test_params$BLC_1    # Boundary layer conductance
    K_cur = test_params$StmRat      # Stomatal ratio
    A_cur = test_params$Photo       # Photosynthetic rate
    C_s_cur = test_params$CO2S      # CO2 sample
    
    # Compute current T_leaf error under current conditions
    T_error = m*(TairC_cur-TleafC_cur)+b
    
    # Recompute conductance and Ci
    g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
    g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur)
    g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
    
    return(C_i_a)
  }
  
  
  aci_all = read.csv("data/aci_by_temp_all.csv")
  Ci_corr = recalc_Ci(aci_all, m = 0.7838, b = 0.8092)
  aci_all$Ci_corr = Ci_corr
  
  m = 0.7838
  b = 0.8092
  T_error = m*(aci_all$Tair-aci_all$Tleaf)+b
  aci_all$Tleaf_corr = aci_all$Tleaf-T_error
  
  # Vcmax and Jmax values
  plot_model_14 = c()
  
  a = subset(aci_all, T_setpoint == 14)
  b = fitaci(a, Tcorrect = F)
  confint(b$nlsfit)
  c = b$Photosyn(Ca = 1*(60:2500))
  c$Type = "Uncorrected"
  plot_model_14 = c
  
  b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  confint(b$nlsfit)
  c = b$Photosyn(Ca = 1*(60:2500))
  c$Type = "Corrected"
  plot_model_14 = rbind(plot_model_14,c)
  
  plot_model_38 = c()
  
  a = subset(aci_all, T_setpoint == 38)
  b = fitaci(a, Tcorrect = F)
  confint(b$nlsfit)
  c = b$Photosyn(Ca = 1*(52:1921))
  c$Type = "Uncorrected"
  plot_model_38 = c
  
  b = fitaci(a, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  confint(b$nlsfit)
  c = b$Photosyn(Ca = 1*(52:2022))
  c$Type = "Corrected"
  plot_model_38 = rbind(plot_model_38,c)
  
  aci_plot = rbind(
    data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci, Type = "Uncorrected", T_setpoint = aci_all$T_setpoint),
    data.frame(Photo = aci_all$Photo, Ci = aci_all$Ci_corr, Type = "Corrected", T_setpoint = aci_all$T_setpoint))
  
  #aci_plot = subset(aci_plot, T_setpoint == 14 | T_setpoint == 38)
  
  aci_plot$T_setpoint = as.factor(aci_plot$T_setpoint)
  
  pdf("figures/fig9.pdf", width = 7.5, height = 3.5)
  
  aci_cut = subset(aci_plot, T_setpoint == 14)
  
  p1 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
    scale_fill_manual(values = rev(palette_c)) +
    scale_colour_manual(values = rev(palette_c)) +  
    geom_line(data = plot_model_14, aes(x=Ci, y=ALEAF,color=Type),lwd=0.4) +
    geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
    my_theme +
    xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
    ylab("Net assimilation (µmol/m²s)") +
    xlim(c(0,1700)) + ylim(c(-2,27)) +
    annotate("text", x = 300, y = 27, label = expression(paste("(a) ", italic("T"[paste("block,LI")]), " = 14ºC")))
  
  aci_cut = subset(aci_plot, T_setpoint == 38)
  #fitaci(data = aci_cut)
  #fitaci(data = aci_cut, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci_corr",
  #                                       PPFD = "PARi", Rd = "Rd"))
  
  p2 = ggplot(data = aci_cut, aes(x = Ci, y = Photo, fill = Type, color = Type)) +
    scale_fill_manual(values = rev(palette_c)) +
    scale_colour_manual(values = rev(palette_c)) +
    geom_line(data = plot_model_38, aes(x=Ci, y=ALEAF), lwd=0.4) +
    geom_point(aes(fill = Type), size=2.4, color = "black", shape = 21) +
    my_theme +
    xlab("Leaf interceullular CO2 concentration (µmol/mol)") +
    ylab("Net assimilation (µmol/m²s)") +
    theme(legend.position = c(0.8,0.15)) +
    theme(legend.title = element_blank()) +
    xlim(c(0,1700)) + ylim(c(-2,27)) +
    annotate("text", x = 300, y = 27, label = expression(paste("(b) ", italic("T"[paste("block,LI")]), " = 38ºC")))
  
  grid.arrange(p1, p2, ncol = 2)
  dev.off()
  
  
  # Let's look at Vcmax and Jmax temperature response
  aci_fits_corr = fitacis(data = aci_all, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf_corr", Ci = "Ci_corr", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_corr = coef(aci_fits_corr)
  Tleaf_means_corr = aci_all %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf_corr))
  kinetics_extracted_corr = merge(kinetics_extracted_corr, Tleaf_means_corr, by = "curveID")
  kinetics_extracted_corr$type = "Corrected"
  
  aci_fits_uncorr = fitacis(data = aci_all, group = "curveID", fitmethod = "default", varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd = "Rd"), Tcorrect = F)
  kinetics_extracted_uncorr = coef(aci_fits_uncorr)
  Tleaf_means_uncorr = aci_all %>% group_by(curveID) %>% summarize(Tleaf = mean(Tleaf))
  kinetics_extracted_uncorr = merge(kinetics_extracted_uncorr, Tleaf_means_uncorr, by = "curveID")
  kinetics_extracted_uncorr$type = "Uncorrected"
  
  kinetics_all = rbind(kinetics_extracted_corr, kinetics_extracted_uncorr)
  
  # Now we need to fit an arrhenius model
  arrhenius = function(k25, Ea, TC, R = 8.31446) {return(k25*exp( (Ea*(TC+273.15) - Ea*298.15) / (298.15*R*(TC+273.15)) ))}
  
  vcmax_corr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), 
                   data = kinetics_extracted_corr,
                   start = list(k25 = 66, Ea = 50000))
  
  jmax_corr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), 
                  data = kinetics_extracted_corr,
                  start = list(k25 = 66, Ea = 50000))
  
  vcmax_uncorr = nls(Vcmax ~ arrhenius(k25, Ea, Tleaf), 
                     data = kinetics_extracted_uncorr,
                     start = list(k25 = 66, Ea = 50000))
  
  jmax_uncorr = nls(Jmax ~ arrhenius(k25, Ea, Tleaf), 
                    data = kinetics_extracted_uncorr,
                    start = list(k25 = 66, Ea = 50000))
  
  # Estimate Q10
  kc = kinetics_extracted_corr
  ku = kinetics_extracted_uncorr
  q10 = function(r1, r2, t1, t2) {
    return( (r2/r1)^(10/(t2-t1)) )
  }
  jmax_q10_corr = q10(kc$Jmax[1], kc$Jmax[5], kc$Tleaf[1], kc$Tleaf[5])
  jmax_q10_uncorr = q10(ku$Jmax[1], ku$Jmax[5], ku$Tleaf[1], ku$Tleaf[5])
  vcmax_q10_corr = q10(kc$Vcmax[1], kc$Vcmax[5], kc$Tleaf[1], kc$Tleaf[5])
  vcmax_q10_uncorr = q10(ku$Vcmax[1], ku$Vcmax[5], ku$Tleaf[1], ku$Tleaf[5])
  
  
  
  
  # Test of a new significance thingie
  kinetics_all$type = as.factor(kinetics_all$type)
  
  a1 = nls(Vcmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))
  
  a2 = nls(Vcmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))
  
  a3 = nls(Vcmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))
  
  anova(a1,a2)
  anova(a1,a3)
  
  b1 = nls(Jmax ~ arrhenius(k25[type], Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = c(50000,50000)))
  
  b2 = nls(Jmax ~ arrhenius(k25, Ea[type], Tleaf),
           data = kinetics_all,
           start = list(k25 = 66, Ea = c(50000,50000)))
  
  b3 = nls(Jmax ~ arrhenius(k25[type], Ea, Tleaf),
           data = kinetics_all,
           start = list(k25 = c(66,66), Ea = 50000))
  
  anova(b1,b2)
  anova(b1,b3)
  
  
  
  
  temps_uncorr = seq(17.8, 32.4, 0.2)
  temps_corr = seq(18.6,29, 0.2)
  v_corr_model = data.frame(Vcmax = predict(vcmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  v_uncorr_model = data.frame(Vcmax = predict(vcmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  vcmax_model = rbind(v_corr_model, v_uncorr_model)
  
  j_corr_model = data.frame(Jmax = predict(jmax_corr, newdata = list(Tleaf = temps_corr)), Tleaf = temps_corr, type = "Corrected")
  j_uncorr_model = data.frame(Jmax = predict(jmax_uncorr, newdata = list(Tleaf = temps_uncorr)), Tleaf = temps_uncorr, type = "Uncorrected")
  jmax_model = rbind(j_corr_model, j_uncorr_model)
  
  
  pdf("figures/fig9a.pdf", width = 7.5, height = 3.5)
  
  # Plot 1 vcmax
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
  
  #ylab(expression(paste("Assimilation rate (µmol ", m^-2, s^-1, ")")))
  
  # Plot 2 Jmax
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
  dev.off()
  
}