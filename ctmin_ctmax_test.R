tpc = function (Tleaf, b, c, Tmin, Tmax) {
  
  return( ( b*(Tleaf - Tmin) * (1-exp(c*(Tleaf - Tmax))))^2 )
  
}




fit_tpc_uncorr = nls_multstart(Photo~tpc(Tleaf = Tleaf, b,c,Tmin,Tmax),
                                 data = uncorr_df,
                                 iter = 1000,
                                 start_lower = c(b = -100, c = -100, Tmin = -20, Tmax = 20),
                                 start_upper = c(b = 100, c = 100, Tmin = 20, Tmax = 70),
                                 supp_errors = 'Y')

fit_tpc_corr = nls_multstart(Photo~tpc(Tleaf = Tleaf, b,c,Tmin,Tmax),
                               data = corr_df,
                               iter = 1000,
                               start_lower = c(b = -100, c = -100, Tmin = -20, Tmax = 20),
                               start_upper = c(b = 100, c = 100, Tmin = 20, Tmax = 70),
                               supp_errors = 'Y')
