# Analysis of LI-COR 6800 internal calculation error
# Equations refer to LI-COR 6800 operators manual
# 15 April 2020
# JCG

# Functions

# Saturation vapour pressure (kPa)
# tempC is leaf temperature in C
# Eqn C-43 in manual
# OK
svp = function(tempC) {
  return(0.6135*exp( (17.502*tempC) / (240.97 + tempC) ))
}

# Molar concentration of water vapor within the leaf (mmol H20 / mol air)
# TleafC = leaf temperature in C
# P = Atm pressure in kPa
# Eqn C-41 in manual
# OK
W_l = function(TleafC, P) {
  return(1000*svp(TleafC)/(P))
}

# Total leaf conductance to water (mol H2O m-2 s-1)
# E = transpiration rate (mol m-2 s-1)
# W_l = Molar concentration of water vapor within the leaf (mmol H20 / mol air)
# W_s = sample mole fraction of water (mmol H20 / mol air)
# Equivalent to eqn C-6 in manual
# OK
g_tw = function(E, W_l, W_s) {
  return( E*(1000 - ((W_l + W_s)/2)) / (W_l - W_s))
}

# Stomatal conductance to water (mol H2O m-2 s-1)
# g_tw = total conductance to water (mol H2O m-2 s-1)
# g_bw = boundary layer conductance to water(mol H2O m-2 s-1)
# K = stomatal ratio (fraction of stomatal conductances of one side of the leaf to the other, unitless)
# Eqn C-14 in manual
# OK
g_sw = function(g_tw, g_bw, K) {
  sub_a = ((1/g_tw) - (1/g_bw))
  sub_b = sqrt( sub_a^2 + (4*K/((K+1)^2))*((2/g_tw) - (1/g_bw))*(1/g_bw) )
  return( 2 / (sub_a + sub_b))
}

# Leaf total conductance to CO2 (mol CO2 m-2 s-1)
# g_sw = Stomatal conductance to water (mol H2O m-2 s-1)
# g_bw = leaf boundary layer conductance to water(mol H2O m-2 s-1)
# K = stomatal ratio (fraction of stomatal conductances of one side of the leaf to the other, unitless)
# Eqn C-15 in manual
# OK
g_tc = function(g_sw, g_bw, K) {
  return( (1 / ( (1+K)*(1.6/g_sw) + (1.37/g_bw) )) + (K / ( (1+K)*(1.6/g_sw) + K*(1.37/g_bw) )) )
}

# Intercellular CO2 concentration (umol CO2 mol air-1)
# g_tc = Leaf total conductance to CO2 (mol CO2 m-2 s-1)
# C_s = mole fraction of CO2 in the sample IRGA, (umol CO2 mol-1 air)
# A = net assimilation rate of CO2 by the leaf (umol CO2 m-2 s-1)
# E = transpiration (mol H2O m-2 s-1)
# Eqn C-16 in manual
# OK
C_i = function(g_tc, C_s, A, E) {
  return(  ((g_tc - E/2)*C_s - A) / (g_tc + E/2)  )
}







#### Estimates ####


# (Make set of read-in parameters)
# Read in set of parameters
# Step through each set of params
# At each param set, estimate each measurement of interest 3 times (g_sw, g_tw?, g_tc?, Ci)
#   One at measured T_leaf
#   One at +T_leaf_error
#   One at -T_leaf_error
# Compile estimates in dataframe 

#test_params = read.csv("error_test_data_6800.csv")


est_error_6800_max <- function(test_params, T_leaf_error = 1.25) {
  
  n = dim(test_params)[1]
  T_error = T_leaf_error
  results = c()
  
  for (i in 1:n){
    # Read out things we're interested in
    E_cur = test_params[i,]$E
    TleafC_cur = test_params[i,]$Tleaf
    P_cur = test_params[i,]$"ΔPcham" + test_params[i,]$Pa
    W_s_cur = test_params[i,]$H2O_s
    g_bw_cur = test_params[i,]$gbw
    K_cur = test_params[i,]$K
    A_cur = test_params[i,]$A
    C_s_cur = test_params[i,]$Ca
    
    # Compute stomatal conductance to water, +/- error
    g_sw_0 = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur),
                  g_bw = g_bw_cur, K = K_cur)
    g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur+T_error, P = P_cur), W_s = W_s_cur),
                  g_bw = g_bw_cur, K = K_cur)
    g_sw_b = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur),
                  g_bw = g_bw_cur, K = K_cur)
    
    # Compute total conductance to water, +/- error 
    g_tw_0 = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur)
    g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur+T_error, P = P_cur), W_s = W_s_cur)
    g_tw_b = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur-T_error, P = P_cur), W_s = W_s_cur)
    
    # Compute total conductance to CO2, +/- error
    g_tc_0 = g_tc(g_sw = g_sw(g_tw = g_tw_0, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    g_tc_b = g_tc(g_sw = g_sw(g_tw = g_tw_b, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    # Compute intercellular CO2 concentration, +/- error
    C_i_0 = C_i(g_tc = g_tc_0, C_s = C_s_cur, A = A_cur, E = E_cur)
    C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
    C_i_b = C_i(g_tc = g_tc_b, C_s = C_s_cur, A = A_cur, E = E_cur)
    
    
    results = rbind(results, data.frame(g_sw_0, g_sw_a, g_sw_b,g_tw_0,g_tw_a,g_tw_b,
                                        g_tc_0,g_tc_a,g_tc_b,C_i_0,C_i_a,C_i_b))
  }
  
  # Works a treat, friend
  
  
  
  # Compute relative error
  error_df = data.frame( g_sw_high = (results$g_sw_0-results$g_sw_a)/results$g_sw_0,
                         g_sw_low =  (results$g_sw_0-results$g_sw_b)/results$g_sw_0,
                         g_tw_high = (results$g_tw_0-results$g_tw_a)/results$g_tw_0,
                         g_tw_low =  (results$g_tw_0-results$g_tw_b)/results$g_tw_0,
                         g_tc_high = (results$g_tc_0-results$g_tc_a)/results$g_tc_0,
                         g_tc_low =  (results$g_tc_0-results$g_tc_b)/results$g_tc_0,
                         C_i_high = (results$C_i_0-results$C_i_a)/results$C_i_0,
                         C_i_low =  (results$C_i_0-results$C_i_b)/results$C_i_0 )
  
  # Combine positive and negative errors
  error_df_combined = data.frame( g_sw = abs(c(error_df$g_sw_high, error_df$g_sw_low)),
                                  g_tw = abs(c(error_df$g_tw_high, error_df$g_tw_low)),
                                  g_tc = abs(c(error_df$g_tc_high, error_df$g_tc_low)),
                                  C_i = abs(c(error_df$C_i_high, error_df$C_i_low)))
  
  err_long = rbind(data.frame(error = error_df_combined$g_sw, variable = "g_sw"),
                   data.frame(error = error_df_combined$g_tw, variable = "g_tw"),
                   data.frame(error = error_df_combined$g_tc, variable = "g_tc"),
                   data.frame(error = error_df_combined$C_i, variable = "C_i"))
  
  return(err_long)
}






est_error_6800_reg <- function(test_params, m = 1, b = 0) {
  
  n = dim(test_params)[1]
#  T_error = T_leaf_error
  results = c()
  
  for (i in 1:n){
    # Read out things we're interested in
    E_cur = test_params[i,]$E
    TleafC_cur = test_params[i,]$Tleaf
    TairC_cur = test_params[i,]$Tair
    P_cur = test_params[i,]$"ΔPcham" + test_params[i,]$Pa
    W_s_cur = test_params[i,]$H2O_s
    g_bw_cur = test_params[i,]$gbw
    K_cur = test_params[i,]$K
    A_cur = test_params[i,]$A
    C_s_cur = test_params[i,]$Ca
    
    # Compute current T_leaf error under current conditions
    T_error = m*(TairC_cur-TleafC_cur)+b
    print(T_error)
    
    # Compute stomatal conductance to water, +/- error
    g_sw_0 = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
    g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur+T_error, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
    
    # Compute total conductance to water, +/- error 
    g_tw_0 = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur)
    g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur+T_error, P = P_cur), W_s = W_s_cur)

    # Compute total conductance to CO2, +/- error
    g_tc_0 = g_tc(g_sw = g_sw(g_tw = g_tw_0, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)

    # Compute intercellular CO2 concentration, +/- error
    C_i_0 = C_i(g_tc = g_tc_0, C_s = C_s_cur, A = A_cur, E = E_cur)
    C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
    
    results = rbind(results, data.frame(g_sw_0,g_sw_a,g_tw_0,g_tw_a,g_tc_0,g_tc_a,C_i_0,C_i_a,TleafC_cur,TairC_cur))
  }
  
  # Works a treat, friend
  
  
  
  # Compute relative error
  error_df = data.frame( g_sw_high = (results$g_sw_0-results$g_sw_a)/results$g_sw_0,
                         g_tw_high = (results$g_tw_0-results$g_tw_a)/results$g_tw_0,
                         g_tc_high = (results$g_tc_0-results$g_tc_a)/results$g_tc_0,
                         C_i_high = (results$C_i_0-results$C_i_a)/results$C_i_0,
                         Tleaf = results$TleafC_cur,
                         Tair = results$TairC_cur)
  
  # Combine positive and negative errors (not now - add back in abs)
  error_df_combined = data.frame( g_sw = error_df$g_sw_high,g_tw = error_df$g_tw_high,
                                  g_tc = error_df$g_tc_high,C_i = error_df$C_i_high,
                                  Tleaf = error_df$Tleaf, Tair = error_df$Tair)
  
  err_long = rbind(data.frame(error = error_df_combined$g_sw, variable = "g_sw", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$g_tw, variable = "g_tw", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$g_tc, variable = "g_tc", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$C_i, variable = "C_i", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair))
  
  return(err_long)
}



est_error_6800_reg_2 <- function(test_params, m = 1, b = 0) {
  
  n = dim(test_params)[1]
  #  T_error = T_leaf_error
  results = c()
  
  for (i in 1:n){
    # Read out things we're interested in
    E_cur = test_params[i,]$E
    TleafC_cur = test_params[i,]$Tleaf
    TairC_cur = test_params[i,]$Tair
    P_cur = test_params[i,]$"ΔPcham" + test_params[i,]$Pa
    W_s_cur = test_params[i,]$H2O_s
    g_bw_cur = test_params[i,]$gbw
    K_cur = test_params[i,]$K
    A_cur = test_params[i,]$A
    C_s_cur = test_params[i,]$Ca
    
    # Compute current T_leaf error under current conditions
    #T_error = m*(TairC_cur-TleafC_cur)+b
    Tleaf_corr = -( (TairC_cur-TleafC_cur)*0.4941 - 1.1436 - TleafC_cur )
    #print(T_error)
    
    # Compute stomatal conductance to water, +/- error
    g_sw_0 = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
    g_sw_a = g_sw(g_tw(E = E_cur, W_l = W_l(TleafC = Tleaf_corr, P = P_cur), W_s = W_s_cur), g_bw = g_bw_cur, K = K_cur)
    
    # Compute total conductance to water, +/- error 
    g_tw_0 = g_tw(E = E_cur, W_l = W_l(TleafC = TleafC_cur, P = P_cur), W_s = W_s_cur)
    g_tw_a = g_tw(E = E_cur, W_l = W_l(TleafC = Tleaf_corr, P = P_cur), W_s = W_s_cur)
    
    # Compute total conductance to CO2, +/- error
    g_tc_0 = g_tc(g_sw = g_sw(g_tw = g_tw_0, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    g_tc_a = g_tc(g_sw = g_sw(g_tw = g_tw_a, g_bw = g_bw_cur, K = K_cur), g_bw = g_bw_cur, K = K_cur)
    
    # Compute intercellular CO2 concentration, +/- error
    C_i_0 = C_i(g_tc = g_tc_0, C_s = C_s_cur, A = A_cur, E = E_cur)
    C_i_a = C_i(g_tc = g_tc_a, C_s = C_s_cur, A = A_cur, E = E_cur)
    
    results = rbind(results, data.frame(g_sw_0,g_sw_a,g_tw_0,g_tw_a,g_tc_0,g_tc_a,C_i_0,C_i_a,TleafC_cur,TairC_cur))
  }
  
  # Works a treat, friend
  
  
  
  # Compute relative error
  error_df = data.frame( g_sw_high = (results$g_sw_0-results$g_sw_a)/results$g_sw_0,
                         g_tw_high = (results$g_tw_0-results$g_tw_a)/results$g_tw_0,
                         g_tc_high = (results$g_tc_0-results$g_tc_a)/results$g_tc_0,
                         C_i_high = (results$C_i_0-results$C_i_a)/results$C_i_0,
                         Tleaf = results$TleafC_cur,
                         Tair = results$TairC_cur)
  
  # Combine positive and negative errors (not now - add back in abs)
  error_df_combined = data.frame( g_sw = error_df$g_sw_high,g_tw = error_df$g_tw_high,
                                  g_tc = error_df$g_tc_high,C_i = error_df$C_i_high,
                                  Tleaf = error_df$Tleaf, Tair = error_df$Tair)
  
  err_long = rbind(data.frame(error = error_df_combined$g_sw, variable = "g_sw", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$g_tw, variable = "g_tw", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$g_tc, variable = "g_tc", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair),
                   data.frame(error = error_df_combined$C_i, variable = "C_i", Tleaf = error_df_combined$Tleaf, Tair = error_df_combined$Tair))
  
  return(err_long)
}
