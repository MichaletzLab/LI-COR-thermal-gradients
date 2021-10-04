
# Function to correct Tleaf based on the difference between Tair and Tleaf
correct_Tleaf6400 = function(Tleaf, Tair, slope = 0.7838, intercept = 0.8092) {
  return(Tleaf - (slope*(Tair-Tleaf)+intercept))
}

# Function to correct Tair based on the difference between Tblock and Tair
correct_Tair6400 = function(Tblock, Tair, slope = 1.7790, intercept = -0.2557) {
  return(Tair - (slope*(Tblock-Tair)+intercept))
}

correct_licor6400 = function(licor_uncorrected) {
  
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
      CTleaf = correct_Tleaf6400(CTleaf_uncorrected, Tair_uncorrected),
      Tleaf = correct_Tleaf6400(Tleaf_uncorrected, Tair_uncorrected),
      Tair = correct_Tair6400(TBlk, Tair_uncorrected)
    ) %>% 
    
    # Correct other quantities based on revised leaf and air temperatures
    calc_licor()
  
}

