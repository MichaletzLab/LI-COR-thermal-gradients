# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Functions for correcting leaf temperatures in the LI-6800
#
# Last edited 13 July 2022, Josef Garen

# Correct leaf temperatures using regression
correct_Tleaf6800 = function(Tleaf, Tair) {
  return( -( (Tair-Tleaf)*0.45322 - 1.02172 - Tleaf ) )
}


# Recalculate reported values from LI-6800. This only recalculates values relevant
# to the analyses performed in this paper, not all values
correct_licor6800 = function(licor_uncorrected) {
  licor_uncorrected %>% 
    mutate(
      Tleaf_uncorrected = Tleaf,
      Tleaf = correct_Tleaf6800(Tleaf_uncorrected, Tair),
      TleafCnd = Tleaf
    ) %>% 
    
    # Correct other quantities
    mutate(
      gtw = E*(1000-(1000*0.61365*exp(17.502*TleafCnd/(240.97+TleafCnd))/(Pa+`ΔPcham`)+H2O_s)/2)/(1000*0.61365*exp(17.502*TleafCnd/(240.97+TleafCnd))/(Pa+`ΔPcham`)-H2O_s),
      gsw = 2 / ((1/gtw - 1/gbw) + sign(gtw)*sqrt((1/gtw - 1/gbw)^2 + 4*K/((K+1)^2)*(2/(gtw*gbw) - 1/(gbw^2)))),
      gtc = 1 / ((K+1)/(gsw/1.6)+1/(gbw/1.37)) + K/((K+1)/(gsw/1.6) + K/(gbw/1.37)),
      Ci = ((gtc-E/2)*Ca-A)/(gtc+E/2),
    )
}
