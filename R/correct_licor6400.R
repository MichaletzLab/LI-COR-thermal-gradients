# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Functions for correcting leaf and air temperatures in the LI-6400XT
#
# Last edited 13 July 2022, Josef Garen

# Function to correct Tleaf based on the difference between Tair and Tleaf
correct_Tleaf6400 = function(Tleaf, Tair, slope = 0.52959, intercept = -0.87710) {
  return(Tleaf - (slope*(Tair-Tleaf)+intercept))
}

# Function to correct Tair based on the difference between Tblock and Tair
correct_Tair6400 = function(Tblock, Tair, slope = 2.28354, intercept = -0.34569) {
    return(Tair - (slope*(Tblock-Tair)+intercept))
}

# Correct leaf and air temperatures and recalculate derived values
correct_licor6400 = function(licor_uncorrected, slope = 0.52959, intercept= -0.87710) {
  
  # Correct air and leaf temperatures
  licor_uncorrected %>% 
    
    # Preserve uncorrected values
    mutate(
      CTleaf_uncorrected = CTleaf,
      Tleaf_uncorrected = Tleaf,
      Tair_uncorrected = Tair,
      #total.leaf.area = Area
    ) %>% 
    
    # Correct air and leaf temperatures 
    mutate(
      CTleaf = correct_Tleaf6400(CTleaf_uncorrected, Tair_uncorrected, slope, intercept),
      Tleaf = correct_Tleaf6400(Tleaf_uncorrected, Tair_uncorrected, slope, intercept),
      Tair = correct_Tair6400(TBlk, Tair_uncorrected)
    ) %>% 
    
    # Correct other quantities based on revised leaf and air temperatures
    calc_licor()
  
}

