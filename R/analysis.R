# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
# Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
# New Phytologist 2022
#
# Scripts for producing figures and statistics
#
# Last edited 13 July 2022, Josef Garen

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(rTPC)
library(nls.multstart)
library(nlstools)
library(plantecophys)
library(LICOR6400)
library(nlme)

source("R/correct_licor6400.R")
source("R/correct_licor6800.R")
 
source("R/make_figS1.R")
source("R/make_figS2.R")
source("R/make_figS3.R")

source("R/make_fig2.R")
source("R/make_fig3.R")
source("R/make_fig4.R")
source("R/make_fig5.R")
source("R/make_fig6.R")
source("R/make_fig7.R")
source("R/make_fig8.R")
source("R/make_fig9.R")


# Generate color palettes for figures
palette_a = c("#ECB63D", "#4188B0")
palette_b = c("#DF853D", "#005686")
palette_c = c("#D899BC", "#007757")

# Generate standard theme
my_theme = theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create directory for figures if none exists, clear stats
if(!dir.exists("figures")) { dir.create("figures") }
if(file.exists("stats.txt")) { file.remove("stats.txt") }
if(file.exists("stats.txt")) { file.remove("stats_supplement.txt") }

# Generate supplemental figures and associated stats
make_figS1()
make_figS2()
make_figS3()

# Generate main text figures and stats. Fig. 1 not included.
make_fig2() 
make_fig3()
make_fig4()
make_fig5()
make_fig6()
make_fig7()
make_fig8()
make_fig9()

#######
# End #
#######