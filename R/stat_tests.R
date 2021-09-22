# Statistical tests for LI-COR errors paper

# Modified 14 April, 2021

library(kSamples)

licor_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/licor_trials_all.csv")
growth_chamber_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/growth_chamber_trials_all.csv")
Tleaf_error_trials_all <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/Tleaf_error_trials_all.csv")

### Section 3.1.1 ###
# 
# # Slope and CI of empty 6400; corresponds to Fig. 1
# test_1 = subset(licor_trials_all, Licor_Type == "6400" & Species == "Empty")
# z1 = lm(T_leaf ~ T_air, data = test_1)
# confint(z1)
# summary(z1)
# 
# a = lmer(T_leaf ~ T_air + (T_air|Licor_name), data = test_1)
# 
# # Slope and CI of empty 6800; corresponds to fig. 1
# test_2 = subset(licor_trials_all, Licor_Type == "6800" & Species == "Empty")
# z2 = lm(T_leaf ~ T_air, data = test_2)
# confint(z2)
# summary(z2)
# 
# a = lmer(T_leaf ~ T_air + (T_air|Licor_name), data = test_2)
# 
# # Slopes of above and below air temperatures 6400; corresponds to fig. 2
# test_3 = subset(licor_trials_all, Licor_Type == "6400")
# z3_1 = lm(T_above ~ T_air, data = test_3)
# z3_2 = lm(T_below ~ T_air, data = test_3)
# confint(z3_1)
# confint(z3_2)
# summary(z3_1)
# summary(z3_2)
# 
# a = lmer(T_above ~ T_air + (T_air|Licor_name), data = test_3)
# b = lmer(T_below ~ T_air + (T_air|Licor_name), data = test_3)
# 
# # Slopes of above and below air temperatures 6800; corresponds to fig. 2
# test_4 = subset(licor_trials_all, Licor_Type == "6800")
# z4_1 = lm(T_above ~ T_air, data = test_4)
# z4_2 = lm(T_below ~ T_air, data = test_4)
# confint(z4_1)
# confint(z4_2)
# summary(z4_1)
# summary(z4_2)
# 
# a = lmer(T_above ~ T_air + (T_air|Licor_name), data = test_4)
# b = lmer(T_below ~ T_air + (T_air|Licor_name), data = test_4)
# 
# # Are the slopes different for 6800?
# #test_5 = rbind(data.frame(T_ar = test_4$T_air, T_am = test_4$T_above, Type = "above"),
# #               data.frame(T_ar = test_4$T_air, T_am = test_4$T_below, Type = "below"))
# #anova(lm(T_am ~ T_ar*Type, data = test_5))
# 
# 
# # Growth chamber trials, above and below leaf air temps in 6400; corresponds to fig. 3
# test_5 = subset(growth_chamber_trials_all, Licor == "6400")
# z5_1 = lm(T_above ~ T_air, data = test_5)
# z5_2 = lm(T_below ~ T_air, data = test_5)
# confint(z5_1)
# confint(z5_2)
# summary(z5_1)
# summary(z5_2)
# 
# # Growth chamber trials, above and below leaf air temps in 6800; corresponds to fig. 3
# test_6 = subset(growth_chamber_trials_all, Licor == "6800")
# z6_1 = lm(T_above ~ T_air, data = test_6)
# z6_2 = lm(T_below ~ T_air, data = test_6)
# confint(z6_1)
# confint(z6_2)
# summary(z6_1)
# summary(z6_2)
# 
# 

# 
# ### Section 3.1.2
# 
# # Error in Tleaf as a function of Tair-Tleaf in the LI-6400XT
# test_7 = subset(Tleaf_error_trials_all, Licor_Type == "6400")
# test_7 = subset(test_7, !is.na(T_air_below))
# test_7$T_leaf_error = (test_7$T_leaf-test_7$T_below)
# test_7$diff = test_7$T_air - test_7$T_leaf
# z7 = lm(T_leaf_error ~ diff, data = test_7)
# confint(z7)
# summary(z7)
# 
# summary(test_7$diff)
# summary(test_7$T_leaf_error)
# 
# # Error in Tleaf as a function of Tair-Tleaf in the LI-6800
# test_8 = subset(Tleaf_error_trials_all, Licor_Type == "6800")
# test_8 = subset(test_8, !is.na(T_air_below))
# test_8$T_leaf_error = (test_8$T_leaf-test_8$T_below)
# test_8$diff = test_8$T_air - test_8$T_leaf
# z8 = lm(T_leaf_error ~ diff, data = test_8)
# confint(z8)
# summary(z8)
# 
# summary(test_8$diff)
# summary(test_8$T_leaf_error)

# 
test_9 = subset(Tleaf_error_trials_all, Licor_Type == "6400")
test_9 = subset(test_9, !is.na(T_air_below))
test_9$T_leaf_error = (test_9$T_leaf-test_9$T_below)
test_9$diff = test_9$T_air_below - test_9$T_below
z9 = lm(T_leaf_error ~ diff, data = test_9)
confint(z9)
summary(z9)

summary(test_9$diff)

#
test_10 = subset(Tleaf_error_trials_all, Licor_Type == "6800")
test_10 = subset(test_10, !is.na(T_air_below))
test_10$T_leaf_error = (test_10$T_leaf-test_10$T_below)
test_10$diff = test_10$T_air_below - test_10$T_below
z10 = lm(T_leaf_error ~ diff, data = test_10)
confint(z10)
summary(z10)

summary(test_10$diff)

#
test_11 = subset(Tleaf_error_trials_all, !is.na(T_air_below))
test_11$Licor_Type = as.factor(test_11$Licor_Type)
test_11$T_leaf_error = (test_11$T_leaf-test_11$T_below)
test_11$diff = test_11$T_air_below - test_11$T_below
z11 = lm(T_leaf_error ~ diff*Licor_Type, data = test_11)
anova(z11)
a = subset(test_11, Licor_Type == 6800)
confint(lm(T_leaf_error ~ diff, data = a))
a = subset(test_11, Licor_Type == 6400)
confint(lm(T_leaf_error ~ diff, data = a))


#
test_12 = subset(Tleaf_error_trials_all, !is.na(T_thread))
z12 = lm(T_below ~ T_thread, data = test_12)
confint(z12)
summary(z12)

# Corrected Tair as a function of Tair in the LI-6400XT
test_13 = subset(Tleaf_error_trials_all, Licor_Type == "6400")
test_13 = subset(test_13, !is.na(T_air_below))
test_13$Tavg = (test_13$T_air_below + test_13$T_air_above)/2
z13 = lm(Tavg ~ T_air, data = test_13)
confint(z13)
summary(z13)

# Error in Tleaf as a function of (Tair-Tleaf) in the LI-6400XT
test_14 = subset(Tleaf_error_trials_all, Licor_Type == "6400")
test_14 = subset(test_14, !is.na(T_air_below))
test_14$Tleaf_err = test_14$T_leaf - test_14$T_below 
test_14$T_diff = test_14$T_air - test_14$T_leaf
z14 = lm(Tleaf_err ~ T_diff, data = test_14)
confint(z14)
summary(z14)

# Error in Tair as a function of (Txcg-Tair) in the LI-6400XT (new regression)
test_15 = subset(Tleaf_error_trials_all, Licor_Type == "6400")
test_15 = subset(test_15, !is.na(T_air_below))
test_15$T_air_true = (test_15$T_air_below + test_15$T_air_above)/2
test_15$T_air_error = test_15$T_air - test_15$T_air_true
test_15$diff = test_15$T_block - test_15$T_air
z15 = lm(T_air_error ~ diff, data = test_15)
confint(z15)
summary(z15)

# Two-sample Anderson-Darling test to see if truncating data changes distribution of slopes
{
  bm_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/blonder_michaletz_fig_1a_data_cut.csv")
  
  bm_data$T_internal = -( (bm_data$Tair_C-bm_data$Tleaf_C)*0.7838 + 0.8092 - bm_data$Tleaf_C )
  bm_data$T_cham = 8.3243 + 0.6705*bm_data$Tair_C
  
  bm_broadleaf = subset(bm_data, Site != "SUMO")
  bm_broadleaf = subset(bm_broadleaf, !is.na(Tleaf_C))
  bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes
  
  bm_broadleaf %>% 
    group_by(curveID) %>% 
    do(model = lm(T_internal ~ T_cham, data = .)) -> tbl_models
  
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$curveID[i] = tbl_models$curveID[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  slopes_full = model_coef$slope
  
  
  bm_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/blonder_michaletz_fig_1a_data_cut.csv")
  
  bm_data = subset(bm_data, Tair_C >= 15.67 & Tair_C <= 37.56)
  bm_data$diff = bm_data$Tair_C-bm_data$Tleaf_C
  bm_data = subset(bm_data, diff >= -3.33 & diff <= 2.18)
  
  bm_data$T_internal = -( (bm_data$Tair_C-bm_data$Tleaf_C)*0.7838 + 0.8092 - bm_data$Tleaf_C )
  bm_data$T_cham = 8.3243 + 0.6705*bm_data$Tair_C
  
  bm_broadleaf = subset(bm_data, Site != "SUMO")
  bm_broadleaf = subset(bm_broadleaf, !is.na(Tleaf_C))
  bm_broadleaf = subset(bm_broadleaf, curveID != 21) # Remove one bad curve - see notes
  
  bm_broadleaf %>% 
    group_by(curveID) %>% 
    do(model = lm(T_internal ~ T_cham, data = .)) -> tbl_models
  
  model_coef = c()
  for (i in 1:dim(tbl_models)[1]) {
    model_coef$curveID[i] = tbl_models$curveID[i]
    model_coef$intercept[i] = coefficients(tbl_models$model[[i]])[1]
    model_coef$slope[i] = coefficients(tbl_models$model[[i]])[2]
  }
  model_coef = data.frame(model_coef)
  slopes_trunc = model_coef$slope
  
  ad.test(slopes_full, slopes_trunc)
}



source("error_prop_6400.R", echo=TRUE)
error_6400_data = read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_test_6400_subsampled.csv")
error_est_reg_6400 = est_error_6400_reg(error_6400_data, m = 0.7838, b = 0.8092)

summary(subset(error_est_reg_6400, variable == "g_sw")$error)
sd(subset(error_est_reg_6400, variable == "g_sw")$error)

summary(subset(error_est_reg_6400, variable == "g_tw")$error)
sd(subset(error_est_reg_6400, variable == "g_tw")$error)

summary(subset(error_est_reg_6400, variable == "g_tc")$error)
sd(subset(error_est_reg_6400, variable == "g_tc")$error)

summary(subset(error_est_reg_6400, variable == "C_i")$error)
sd(subset(error_est_reg_6400, variable == "C_i")$error)



source("error_prop_6800.R", echo=TRUE)
error_6800_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_test_6800_subsampled.csv")
error_est_reg_6800 = est_error_6800_reg(error_6800_data, m = 0.4941, b = -1.1436)

summary(subset(error_est_reg_6800, variable == "g_sw")$error)
sd(subset(error_est_reg_6800, variable == "g_sw")$error)

summary(subset(error_est_reg_6800, variable == "g_tw")$error)
sd(subset(error_est_reg_6800, variable == "g_tw")$error)

summary(subset(error_est_reg_6800, variable == "g_tc")$error)
sd(subset(error_est_reg_6800, variable == "g_tc")$error)

summary(subset(error_est_reg_6800, variable == "C_i")$error)
sd(subset(error_est_reg_6800, variable == "C_i")$error)
