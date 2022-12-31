#!/usr/bin/env python3

import os
import shutil
import glob
import re

scatter_dir = "Scatter Plots"
anova_dir = "Anova Result"
hemisphere_dir = "Hemisphere Plots"
gender_plot_dir = "Gender Plots"
anova_analysis_file = "anova_analysis.txt"

def categorize_data(clean_leftover=False):
  # move scatter plots to scatter_dir
  if not os.path.isdir(scatter_dir):
    os.makedirs(scatter_dir)
    print("created folder: %s" % scatter_dir)
  files = [f for f in glob.glob("*.png") if ("lh_" in f or "rh_" in f) and \
    ("_harmo_no_all.png" in f or "_harmo_no_all_s.png" in f) ]
  for f in files:
    dest = os.path.join(scatter_dir, f)
    shutil.move(f, dest)

  # move Anova results to anova_dir
  if not os.path.isdir(anova_dir):
    os.makedirs(anova_dir)
    print("created folder: %s" % anova_dir )
  files = [f for f in glob.glob("*.xlsx") if ("lh_" in f or "rh_" in f) and \
          ("anova_age_as_key.xlsx" in f) ]
  for f in files:
    dest = os.path.join(anova_dir, f)
    shutil.move(f, dest)
  if os.path.isfile(anova_analysis_file):
    dest = os.path.join(anova_dir, anova_analysis_file)
    shutil.move(anova_analysis_file, dest)

  # move hemisphere plots to hemisphere_dir
  if not os.path.isdir(hemisphere_dir):
    os.makedirs(hemisphere_dir)
    print("created folder: %s" % hemisphere_dir)
  files = [f for f in glob.glob("*_hemisphere.png") ]
  for f in files:
    dest = os.path.join(hemisphere_dir, f)
    shutil.move(f, dest)

  # move gender plots
  if not os.path.isdir(gender_plot_dir):
    os.makedirs(gender_plot_dir)
    print("created folder: %s" % gender_plot_dir)
  files = [f for f in glob.glob("lh_*.png") + glob.glob("rh_*.png") ]
  for f in files:
    dest = os.path.join(gender_plot_dir, f)
    shutil.move(f, dest)
  
  if(clean_leftover):
    print("Cleaning leftover files...")
    files = [f for f in glob.glob("lh_*.csv") + glob.glob("rh_*.csv") + glob.glob("*_hemisphere.csv") + glob.glob("*_clean.csv") ]
    for f in files:
      os.remove(f)
