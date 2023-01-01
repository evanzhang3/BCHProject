# What is this repo for?

This repo maintains the brain image data and code/programs that can be used to harmonize the data and draw various plots based on the harmonized data.

# Prerequisite:
There are two types of code maintained in this repo: R code and Python code. 

The following R libraries need to be preinstalled:
* ggplot2
* dplyr
* XLConnect
* VGAM
* reshape2
* reticulate (this package enables python in R)

The following Python modules need to be installed as well:
* seaborn
* pandas
* matplotlib
* neuroCombat
* numpy
* shutil

# How to run the program:
This program can be easily executed with the following steps:
1. Open the harmonization.R program with RStuido
2. Source the harmonization.R code
3. Run the process_all() function

# Output

After calling the process_all() function, the following directories will be created:

1. Scatter Plots: it contains the scatter plots with all subjects together. The plots also show median and percentile curves
2. Hemisphere Plots: it contains the plots that compare left vs right hemispheres, curves with shaded areas, no differentiation of sexes.
3. Gender Plots: it contains the plots that compare male vs female, curves with shaded areas.
4. Anova Result: it contains the Anova analysis result for each brain matrix

# Note:
This program is tested on macOS operating system. Large disk space (> 50GB) is required to run this program. And it takes a significantly long time (>60 minutes on macBookPro) to finish all the matrixes. The sample output can be found at: 
https://drive.google.com/drive/folders/1XD0aMCPCOJugqh5F32pLPzP_rTnCyg3j?usp=sharing
