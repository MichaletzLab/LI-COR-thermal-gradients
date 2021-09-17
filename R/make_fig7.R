# Figure 7
# Limited homeothermy plot + supplementary plot
make_fig7 = function () {
  
  # Build LI-COR dataset from raw files
  # Read in raw LI-COR files
  rmbl2015_licor <- list.files(path = "data/licor_data_raw/RMBL 2015", full.names = TRUE) %>% # List filenames
    grep("\\.", x = ., value = TRUE, invert = TRUE) %>% # only include files w/o extension (ignore metadata, excel)
    set_names(basename(.)) %>% # Build path names
    map_df(read_licor6400, .id = "run") %>% # Read raw licor data for each file
    separate(run, into = c("initials", "uniqueid"), sep = "-") # Cleanup
  rmbl2016_licor <- list.files(path = "data/licor_data_raw/RMBL 2016", full.names = TRUE) %>% # List filenames
    grep("\\.", x = ., value = TRUE, invert = TRUE) %>% # only include files w/o extension (raw)
    grep("leaf", x = ., value = TRUE, invert = TRUE) %>% # exclude leaf scans folder
    set_names(basename(.)) %>% # Build path names
    map_df(read_licor6400, .id = "uniqueid") # Read raw licor data for each file

  # Skip leaf area correction - makes no difference for this analysis
  
  # Read in metadata
  meta_rmbl2015 = read_excel("data/licor_data_raw/RMBL 2015/DataMaster_RMBL2015.xlsx") %>% # Read in metadata
    select(-Area, -QC, -Filename) %>% mutate(Filename = sample) %>% select(-sample) %>% # Rename and remove junk
    mutate(Date = as.character(Date)) # Change to character for merging
  meta_rmbl2016 = read_excel("data/licor_data_raw/RMBL 2016/DataMaster_RMBL2016.xlsx") %>% # Read in metadata
    select(-QC) %>% select(-sample) %>% # Remove junk
    mutate(Date = as.character(Date))   # Change to character for merge
  
  ### Clean up metadata - RMBL 2016
  meta_rmbl2016$Filename = meta_rmbl2016$Filename %>% # Fix some misspellings
    gsub("062016-stm-taof1", "062016_stm_taof1", .) %>%
    gsub("062016-stm-vaoc1", "062016_stm_vaoc1", .) %>%
    gsub("062016-stm-vaoc2", "062016_stm_vaoc2", .)
  
  # Build full licor raw data set 
  full_licor <- bind_rows(RMBL_2015 = rmbl2015_licor, RMBL_2016 = rmbl2016_licor, .id = "place") %>% # Bind together
    select( -initials) %>% # Drop unneeded column
    mutate(TBlk_floor = floor(TBlk)) %>% # Use this as a grouping variable
    group_by(uniqueid, variable, parameter, level, unit, place, TBlk_floor)
  
  # Average by T level here
  licor_means = full_licor  %>%
    group_by(uniqueid, place, variable, parameter, unit, TBlk_floor) %>%
    summarize_all(mean) %>% select(-TBlk_floor)
  
  # Bind metadata
  metadata = bind_rows(meta_rmbl2015, meta_rmbl2016)
  metadata = mutate(metadata, uniqueid = Filename) %>% select(-Filename)
  
  # Merge averaged data with metadata
  licor_means = merge(licor_means, metadata, all.x = TRUE, by = "uniqueid")
  
  # Rename some columns for convenience
  licor_means = licor_means %>% mutate(Campaign = place) %>% select(-place)
  
  # Correct the leaf and air temperatures using new function
  licor_corrected = correct_licor6400(licor_means)

  
  # Open file
  pdf("figures/fig7.pdf", width = 7, height = 3.5)
  
  # Fit linear models to each curve in the dataset (uncorrected T values)
  licor_corrected %>% 
    group_by(uniqueid) %>% 
    do(model = lm(Tleaf_uncorrected ~ Tair_uncorrected, data = .)) -> tbl_models
  
  # Extract linear model coefficients
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$uniqueid[i] = tbl_models$uniqueid[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  model_coef_uncorrected = model_coef

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
    ggplot(data = model_coef, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0,1.25)) +
      ylim(c(0,7.5)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p1 = main_plot + annotation_custom(grob = inset_plot, xmin = 0, xmax = 27, ymin = 29, ymax = 50)
  
  # Fit linear model to each curve in the dataset (corrected T values)
  licor_corrected %>% 
    group_by(uniqueid) %>% 
    do(model = lm(Tleaf ~ Tair, data = .)) -> tbl_models
  
  # Extract model coefficients
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$uniqueid[i] = tbl_models$uniqueid[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  model_coef_corrected = model_coef
  slopes_full = model_coef$slope
  
  # Build main plot
  main_plot = ggplot(data = licor_corrected, aes(x = Tair, y = Tleaf)) + 
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
  inset_plot = ggplotGrob(
    ggplot(data = model_coef, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0,1.25)) + ylim(c(0,7.5)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p2 = main_plot + annotation_custom(grob = inset_plot, xmin = 0, xmax = 27, ymin = 29, ymax = 50)
  grid.arrange(p1, p2, ncol = 2)
  
  # Close file
  dev.off()
  
  # Generate supplemental figure S3
  
  # Trim data such that (Tblock-Tair) and (Tair-Tleaf) do not go out of 
  # the bounds of our regressions
  licor_corrected$diff_1 = licor_corrected$TBlk - licor_corrected$Tair
  licor_corrected$diff_2 = licor_corrected$Tair - licor_corrected$Tleaf
  licor_cut = subset(licor_corrected, diff_1 >= -1.73 & diff_1 <= 2.27)
  licor_cut = subset(licor_cut, diff_2 >= -3.33 & diff_2 <= 2.18)
  
  # Fit linear model to each curve in the truncated dataset (corrected T only)
  licor_cut %>% 
    group_by(uniqueid) %>% 
    do(model = lm(Tleaf ~ Tair, data = .)) -> tbl_models
  
  # Extract model coefficients
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$uniqueid[i] = tbl_models$uniqueid[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  model_coef_trunc = model_coef
  slopes_trunc = model_coef$slope
  
  # Open file
  pdf("figures/figS3.pdf", width = 4, height = 4)
  
  # Build main plot
  main_plot = ggplot(data = licor_cut, aes(x = Tair, y = Tleaf)) + 
    geom_point(size = 0.8) + 
    geom_smooth(method=lm, fill=NA, aes(group=uniqueid), lwd=0.3, color = palette_c[2]) +
    scale_color_manual(values=c("blue","red")) +
    geom_abline(slope = 1, lty=2) +
    my_theme +
    xlab("Corrected in-chamber air temperature (ºC)") +
    ylab("Corrected leaf temperature (ºC)") +
    xlim(c(12,38)) + ylim(c(12,38))
  
  # Build inset plot - slope distribution
  inset_plot = ggplotGrob(
    ggplot(data = model_coef, aes(x = slope)) + 
      geom_density() + 
      theme_classic() +
      xlim(c(0,1.25)) + ylim(c(0,7.5)) +
      theme(axis.title = element_text(size=8),
            rect = element_rect(fill = "transparent"),
            plot.background = element_rect(colour = "transparent") )
  )
  
  p2 = main_plot + annotation_custom(grob = inset_plot, xmin = 11, xmax = 28, ymin = 26, ymax = 39)
  grid.arrange(p2,ncol=1)
  
  # Close file
  dev.off()
  
  
  
  #### Stats
  # uncorrected
  
  # Quick stats: mean and stderr of slope values
  print("Mean slope, uncorrected:")
  print(mean(model_coef$slope))
  print("Std. error, uncorrected:")
  print(sd(model_coef$slope)/sqrt(length(model_coef$slope)))
  print("Variance, uncorrected:")
  print(var(model_coef$slope))
  
  # Quick stats: mean and stderr of slope values - corrected
  print("Mean slope, uncorrected:")
  print(mean(model_coef$slope))
  print("Std. error, uncorrected:")
  print(sd(model_coef$slope)/sqrt(length(model_coef$slope)))
  print("Variance, uncorrected:")
  print(var(model_coef$slope))
  
  # Quick stats: mean and stderr of slope values - supplement
  print("Mean slope, uncorrected:")
  print(mean(model_coef$slope, na.rm = T))
  print("Std. error, uncorrected:")
  print(sd(model_coef$slope, na.rm = T)/sqrt(sum(!is.na(model_coef$slope))))
  print("Variance, uncorrected:")
  print(var(model_coef$slope))
  
  # Test if truncation modifies slope distribution
  ad.test(slopes_full, slopes_trunc)

}
