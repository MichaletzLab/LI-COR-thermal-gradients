


#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/LICOR6400")

library("LICOR6400")
library("tidyverse")

#licor_uncorrected = read_licor6400("data/LI6400XT_sample_raw")

#licor_corrected = correct_licor6400(licor_uncorrected)

#write.csv(licor_corrected, "LI6400XT_sample_corrected.csv", col.names = F)

correct_Tleaf = function(Tleaf, Tair, slope = 0.7838, intercept = 0.8092) {
  return(Tleaf - (slope*(Tair-Tleaf)+intercept))
}


# As it is, this is consistent with how we do it in the paper
correct_Tair = function(Tair, slope = 0.6705, intercept = 8.3243) {
  return(intercept + slope*Tair)
}

correct_Tair_2 = function(Tblock, Tair, slope = 1.7790, intercept = -0.2557) {
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
      total.leaf.area = Area
    ) %>% 
    
    # Correct air and leaf temperatures 
    mutate(
      CTleaf = correct_Tleaf(CTleaf_uncorrected, Tair_uncorrected),
      Tleaf = correct_Tleaf(Tleaf_uncorrected, Tair_uncorrected),
      Tair = correct_Tair_2(TBlk, Tair_uncorrected)
    ) %>% 
    
    # Correct other quantities based on revised leaf and air temperatures
    calc_licor()
  
}
