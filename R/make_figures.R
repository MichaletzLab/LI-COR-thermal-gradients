# Code to generate figures for LI-COR gradients manuscript
#
#
# 4 June 2020 Josef Garen

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(readxl)
library(rTPC)
library(nls.multstart)
library(nlstools)
library(plantecophys)
library(kSamples)

##### Add thing here:
library(LICOR6400)

source("R/correct_licor_function.R")
#source("R/error_prop_6400.R")
#source("R/error_prop_6800.R")

source("R/make_figS1.R")
source("R/make_figS2.R")
source("R/make_fig2.R")
source("R/make_fig3.R")
source("R/make_fig4.R")
source("R/make_fig5.R")
source("R/make_fig6.R")
source("R/make_fig7.R")
source("R/make_fig8.R")
source("R/make_fig9.R")
#etc... or maybe you can just do something like "source("R")"

# Need to get this updated on the github
source("R/calc_licor_jcg.R")

# Generate color palettes for the different figures
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_a = c("#E69F00", "#56B4E9")
palette_b = c("#D55E00", "#0072B2")
palette_c = c("#CC79A7","#009E73")

# Generate standard theme
my_theme = theme_bw() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create directory for figures if none exists
if(!dir.exists("figures")) { dir.create("figures") }

file.remove("stats.txt")
file.remove("stats_supplement.txt")

# Generate supplemental figures 1 and 2 and stats(Fig. S3 below)
make_figS1() # OK
make_figS2() # fig runs clean; no stats to speak of?

# Generate main text figures and stats. Fig. 1 not included.
make_fig2() # OK
make_fig3() # OK
make_fig4() # OK
make_fig5() # OK
make_fig6() # runs, but very clunky. ERRORS NEED TO BE CORRECTED HERE.
make_fig7() # OK; Also generates Fig. S3
make_fig8() # OK
make_fig9() # OK

########
# Done #
########