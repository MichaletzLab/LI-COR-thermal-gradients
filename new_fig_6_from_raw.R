# Here I am going to remake the Blonder Michaletz figure from scratch,
# using raw Licor data and our new "correct_licor6400" function.


# Header / libraries
library(LICOR6400)
library(tidyverse)
library(readr)
library(readxl)
library(LeafArea)
library(dplyr)

source("correct_licor_function.R")
source("read_licor6400_jcg.R")

# Read in all the 6400 data.

rmbl2015_licor <- list.files(path = "data/licor_data_raw/RMBL 2015", full.names = TRUE) %>% # List filenames
  grep("\\.", x = ., value = TRUE, invert = TRUE) %>% # only include files w/o extension (ignore metadata, excel)
  set_names(basename(.)) %>% # Build path names
  map_df(read_licor6400_jcg, .id = "run") %>% # Read raw licor data for each file
  separate(run, into = c("initials", "uniqueid"), sep = "-") # Cleanup
###
rmbl2016_licor <- list.files(path = "data/licor_data_raw/RMBL 2016", full.names = TRUE) %>% # List filenames
  grep("\\.", x = ., value = TRUE, invert = TRUE) %>% # only include files w/o extension (raw)
  grep("leaf", x = ., value = TRUE, invert = TRUE) %>% # exclude leaf scans folder
  set_names(basename(.)) %>% # Build path names
  map_df(read_licor6400_jcg, .id = "uniqueid") # Read raw licor data for each file
###

# I am skipping the leaf area correction here bc it doesnt work and we don't need it


#leaf_area_rmbl2015 = read_excel("data/licor_data_raw/RMBL 2015/DataMaster_RMBL2015.xlsx")

# These we have to get from processing the leaf scan image files using LeafArea/ImageJ
#leaf_area_rmbl2016 = run.ij(path.imagej = '/Applications/ImageJ.app', 
#                            set.directory = "data/licor_data_raw/RMBL 2016/leaf scans/cropped",
#                            distance.pixel = 2480, trim.pixel = 20, prefix = "\\.")

# Clean RMBL 2015 data - No need to adjust leaf areas - all below 6 cm2
#leaf_area_rmbl2015 = mutate(leaf_area_rmbl2015, total.leaf.area = Area, uniqueid = sample) %>%
#  select(uniqueid, total.leaf.area) # drop junk

# RMBL 2016 -  unique IDs match!
#leaf_area_rmbl2016 = mutate(leaf_area_rmbl2016, uniqueid = gsub("_cropped", "", sample), # clean up uniqueid name
#                            total.leaf.area = if_else(total.leaf.area > 6, 6, total.leaf.area) # trim areas to 6 cm2
#) %>% select(-sample) # drop junk

meta_rmbl2015 = read_excel("data/licor_data_raw/RMBL 2015/DataMaster_RMBL2015.xlsx") %>% # Read in metadata
  select(-Area, -QC, -Filename) %>% mutate(Filename = sample) %>% select(-sample) %>% # Rename and remove junk
  mutate(Date = as.character(Date)) # Change to character for merging

meta_rmbl2016 = read_excel("data/licor_data_raw/RMBL 2016/DataMaster_RMBL2016.xlsx") %>% # Read in metadata
  select(-QC) %>% select(-sample) %>% # Remove junk
  mutate(Date = as.character(Date))   # Change to character for merge

### RMBL 2016
meta_rmbl2016$Filename = meta_rmbl2016$Filename %>% # Fix some misspellings
  gsub("062016-stm-taof1", "062016_stm_taof1", .) %>%
  gsub("062016-stm-vaoc1", "062016_stm_vaoc1", .) %>%
  gsub("062016-stm-vaoc2", "062016_stm_vaoc2", .)

# Build full licor raw data set - bind all sets together, get rid of some junk columns.
full_licor <- bind_rows(RMBL_2015 = rmbl2015_licor, RMBL_2016 = rmbl2016_licor, .id = "place") %>%
  select( -initials) %>% 
  mutate(TBlk_floor = floor(TBlk)) %>% 
  group_by(uniqueid, variable, parameter, level, unit, place, TBlk_floor)

# Remove T levels with less than ten measurements and keep only last ten at each level
#full_licor <- full_licor %>% 
#  filter(!(n < 10)) %>% #remove analyses with n < 10
#  slice((n[1] - 9):n[1])#keep only last 10 of n measurements

# Build full leaf area dataset - just bind all rows together (note no CR leaf area data)
#leaf_area_all = bind_rows(leaf_area_svalbard, leaf_area_peru, leaf_area_rmbl2015,
#                          leaf_area_rmbl2016, leaf_area_china2016, leaf_area_china2015, leaf_area_lanl)

# Merge licor data with leaf area data and clean them up
#licor_leaf = merge(full_licor, leaf_area_all, by = "uniqueid", all.x = TRUE) %>% # merge leaf area data with licor data
#  mutate(leaf.area.missing = if_else(is.na(total.leaf.area), TRUE, FALSE)) %>% # Add column to indicate if leaf area data is missing
#  mutate(total.leaf.area = if_else(is.na(total.leaf.area), 6, total.leaf.area)) # If lead area data is missing, put 6 in for area (no adjustment)

# Recalculate photo based on new leaf areas
#licor_leaf <- licor_leaf %>% recalc_licor

licor_leaf = full_licor

# Average by T level here
licor_means = licor_leaf  %>%
  group_by(uniqueid, place, variable, parameter, unit, TBlk_floor) %>%
  summarize_all(mean) %>% select(-TBlk_floor)

metadata = bind_rows(meta_rmbl2015, meta_rmbl2016)
metadata = mutate(metadata, uniqueid = Filename) %>% select(-Filename)

licor_means = merge(licor_means, metadata, all.x = TRUE, by = "uniqueid")

# Rename some columns for convenience
licor_means = licor_means %>% mutate(Campaign = place) %>% select(-place)

##### Remove one bad curve
#licor_means = subset(licor_means, uniqueid != "rvaoc1")


# Now we correct the leaf and air temperatures
licor_corrected = correct_licor6400(licor_means)

# Now we remake the figure

# There was one or more bad curves we removed.... see notes?
#bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes

# Quick stats: how many leaves, points, and species?
print("Number of observations total:")
print(dim(licor_corrected)[1])
print("Number of leaves:")
print(length(unique(licor_corrected$uniqueid)))
print("Number of species:")



pdf("figures/fig6_new.pdf", width = 7, height = 3.5)

licor_corrected %>% 
  group_by(uniqueid) %>% 
  do(model = lm(Tleaf_uncorrected ~ Tair_uncorrected, data = .)) -> tbl_models

model_coef = c()
for (i in 1:dim(tbl_models)[1]) {
  model_coef$uniqueid[i] = tbl_models$uniqueid[i]
  model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
  model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
}
model_coef = data.frame(model_coef)
model_coef$high_slope = model_coef$slope > 1
#  bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")

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

# Quick stats: mean and stderr of slope values
print("Mean slope, uncorrected:")
print(mean(model_coef$slope))
print("Std. error, uncorrected:")
print(sd(model_coef$slope)/sqrt(length(model_coef$slope)))
print("Variance, uncorrected:")
print(var(model_coef$slope))


licor_corrected %>% 
  group_by(uniqueid) %>% 
  do(model = lm(Tleaf ~ Tair, data = .)) -> tbl_models

model_coef = c()
for (i in 1:dim(tbl_models)[1]) {
  model_coef$uniqueid[i] = tbl_models$uniqueid[i]
  model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
  model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
}
model_coef = data.frame(model_coef)
model_coef$high_slope = model_coef$slope > 1
#  bm_broadleaf = merge(bm_broadleaf, model_coef, by="curveID")
slopes_full = model_coef$slope

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

# Quick stats: mean and stderr of slope values
print("Mean slope, uncorrected:")
print(mean(model_coef$slope))
print("Std. error, uncorrected:")
print(sd(model_coef$slope)/sqrt(length(model_coef$slope)))
print("Variance, uncorrected:")
print(var(model_coef$slope))


grid.arrange(p1, p2, ncol = 2)
dev.off()




# Now I should also make the supplemental figure that goes with this one

# Start with licor_corrected. What are the bounds of our regression?
# First trim the data s.t. Tair-Tleaf is w/in the bounds of our data (-3.33,2.18)
# Then trim s.t. Tblock-Tair is w/in the bounds of our data (-1.73,2.27)
licor_corrected$diff_1 = licor_corrected$TBlk - licor_corrected$Tair
licor_corrected$diff_2 = licor_corrected$Tair - licor_corrected$Tleaf

licor_cut = subset(licor_corrected, diff_1 >= -1.73 & diff_1 <= 2.27)
licor_cut = subset(licor_cut, diff_2 >= -3.33 & diff_2 <= 2.18)

licor_cut %>% 
  group_by(uniqueid) %>% 
  do(model = lm(Tleaf ~ Tair, data = .)) -> tbl_models

model_coef = c()
for (i in 1:dim(tbl_models)[1]) {
  model_coef$uniqueid[i] = tbl_models$uniqueid[i]
  model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
  model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
}
model_coef = data.frame(model_coef)
model_coef$high_slope = model_coef$slope > 1

slopes_trunc = model_coef$slope

pdf("figS3_new.pdf", width = 4, height = 4)


main_plot = ggplot(data = licor_cut, aes(x = Tair, y = Tleaf)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method=lm, fill=NA, aes(group=uniqueid), lwd=0.3, color = palette_c[2]) +
  scale_color_manual(values=c("blue","red")) +
  geom_abline(slope = 1, lty=2) +
  my_theme +
  xlab("Corrected in-chamber air temperature (ºC)") +
  ylab("Corrected leaf temperature (ºC)") +
  xlim(c(12,38)) + ylim(c(12,38))

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

# Quick stats: mean and stderr of slope values
print("Mean slope, uncorrected:")
print(mean(model_coef$slope, na.rm = T))
print("Std. error, uncorrected:")
print(sd(model_coef$slope, na.rm = T)/sqrt(sum(!is.na(model_coef$slope))))
print("Variance, uncorrected:")
print(var(model_coef$slope))

p2
dev.off()


# Test if truncation modifies slope distribution
ad.test(slopes_full, slopes_trunc)


