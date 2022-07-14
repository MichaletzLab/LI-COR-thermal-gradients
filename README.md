# Gas exchange analyzers exhibit large errors driven by internal thermal gradients
## Josef Garen, Haley Branch, Isaac Borrego, Benjamin Blonder, Joseph Stinziano, and Sean Michaletz
## New Phytologist 2022

This repo contains scripts for reproducing the figures and statistics in Garen et al. 2022. All figures and statistics may be reproduced by running the file R/analysis.R.

This repo also contains code which may be used to correct for errors in leaf and air temperatures, as well as downstream derived quantities such as stomatal conductance and leaf intercellular CO2 concentration. The parameter values used in the correction functions defined in the associated file R/correct_lifor6400.R are likely to vary in a manner that is specific to an individual instrument and set of environmental conditions. We urge potential users of this code to independently measure leaf and air temperatures in their LI-COR instruments and environmental conditions to parameterize these correction functions prior to use. An example of the use of this correction function may be found in examples/example_analysis.R
