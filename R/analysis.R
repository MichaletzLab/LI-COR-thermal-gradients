# Code to generate figures  and stats for paper "Gas exchange analyzers
# exhibit large errors driven by internal thermal gradients"
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
library(LICOR6400)

# source("R/correct_licor6400.R")
# source("R/correct_licor6800.R")
# 
# source("R/make_figS1.R")
# source("R/make_figS2.R")
# source("R/make_fig2.R")
# source("R/make_fig3.R")
# source("R/make_fig4.R")
# source("R/make_fig5.R")
# source("R/make_fig6.R")
# source("R/make_fig7.R")
# source("R/make_fig8.R")
# source("R/make_fig9.R")
#etc... or maybe you can just do something like "source("R")"
sapply(list.files("R", full.names = T)[2:13], source)

# Generate color palettes for the different figures
palette_a = c("#E69F00", "#56B4E9")
palette_b = c("#D55E00", "#0072B2")
palette_c = c("#CC79A7","#009E73")

# Generate standard theme
my_theme = theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

theme_transparent =   theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent", color=NA) # get rid of legend panel bg
)

# Create directory for figures if none exists, clear stats
if(!dir.exists("figures")) { dir.create("figures") }
if(file.exists("stats.txt")) { file.remove("stats.txt") }
if(file.exists("stats.txt")) { file.remove("stats_supplement.txt") }

# Generate supplemental figures 1 and 2 and stats(Fig. S3 below)
make_figS1() # OK
make_figS2() # OK

# Generate main text figures and stats. Fig. 1 not included.
make_fig2() # Modified - make this one
make_fig3() # OK
make_fig4() # OK
make_fig5() # Modified - make this one
make_fig6() # Need new dataset here, final touches to fig. else ok
make_fig7() # OK; still makes blank page?; Also generates Fig. S3
make_fig8() # OK;
make_fig9() # OK;

########
# Done #
########