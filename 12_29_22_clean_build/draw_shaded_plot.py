import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import rcParams

# set output plot size
a4_dims = (11.7, 8.27)
sns.set()

def draw_shaded_plot(feature):
  print("working on %s" % feature)
  plot_file_path = feature + "_harmo_no_all_plot" + ".csv"
  df = pd.read_csv(plot_file_path)

  fig, ax = plt.subplots(figsize=a4_dims)
  sns_lineplot = sns.lineplot(ax=ax, x='Age', y=feature, data=df.query("Sex != 'Unknown' & Age<=85"), hue='Sex')
  plt.title("harmonized " + feature + " after outlier removal")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'.png')
  plt.clf()
  plt.close()
  return True

def draw_shade_hemisphere_plot(feature):
  print("working on %s" % feature)
  plot_file_path = feature+ "_hemisphere.csv"
  df = pd.read_csv(plot_file_path)

  fig, ax = plt.subplots(figsize=a4_dims)
  sns_lineplot = sns.lineplot(ax=ax, x='Age', y=feature, data=df.query("Sex != 'Unknown' & Age<=85"), hue='hemisphere')
  plt.title("harmonized " + feature + " after outlier removal")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'_hemisphere.png')
  plt.clf()
  plt.close()
  return True

def draw_all_plots():
  lh_curvind_region_list = [    "lh_bankssts_curvind",
                               "lh_caudalanteriorcingulate_curvind",
                               "lh_caudalmiddlefrontal_curvind",
                               "lh_cuneus_curvind",
                               "lh_entorhinal_curvind",
                               "lh_fusiform_curvind",
                               "lh_inferiorparietal_curvind",
                               "lh_inferiortemporal_curvind",
                               "lh_isthmuscingulate_curvind",
                               "lh_lateraloccipital_curvind",
                               "lh_lateralorbitofrontal_curvind",
                               "lh_lingual_curvind",
                               "lh_medialorbitofrontal_curvind",
                               "lh_middletemporal_curvind",
                               "lh_parahippocampal_curvind",
                               "lh_paracentral_curvind",
                               "lh_parsopercularis_curvind",
                               "lh_parsorbitalis_curvind",
                               "lh_parstriangularis_curvind",
                               "lh_pericalcarine_curvind",
                               "lh_postcentral_curvind",
                               "lh_posteriorcingulate_curvind",
                               "lh_precentral_curvind",
                               "lh_precuneus_curvind",
                               "lh_rostralanteriorcingulate_curvind",
                               "lh_rostralmiddlefrontal_curvind",
                               "lh_superiorfrontal_curvind",
                               "lh_superiorparietal_curvind",
                               "lh_superiortemporal_curvind",
                               "lh_supramarginal_curvind",
                               "lh_frontalpole_curvind",
                               "lh_temporalpole_curvind",
                               "lh_transversetemporal_curvind",
                               "lh_insula_curvind"
                          ]

  for feature in lh_curvind_region_list:
    draw_shaded_plot(feature)

  rh_curvind_region_list = [   "rh_bankssts_curvind",
                               "rh_caudalanteriorcingulate_curvind",
                               "rh_caudalmiddlefrontal_curvind",
                               "rh_cuneus_curvind",
                               "rh_entorhinal_curvind",
                               "rh_fusiform_curvind",
                               "rh_inferiorparietal_curvind",
                               "rh_inferiortemporal_curvind",
                               "rh_isthmuscingulate_curvind",
                               "rh_lateraloccipital_curvind",
                               "rh_lateralorbitofrontal_curvind",
                               "rh_lingual_curvind",
                               "rh_medialorbitofrontal_curvind",
                               "rh_middletemporal_curvind",
                               "rh_parahippocampal_curvind",
                               "rh_paracentral_curvind",
                               "rh_parsopercularis_curvind",
                               "rh_parsorbitalis_curvind",
                               "rh_parstriangularis_curvind",
                               "rh_pericalcarine_curvind",
                               "rh_postcentral_curvind",
                               "rh_posteriorcingulate_curvind",
                               "rh_precentral_curvind",
                               "rh_precuneus_curvind",
                               "rh_rostralanteriorcingulate_curvind",
                               "rh_rostralmiddlefrontal_curvind",
                               "rh_superiorfrontal_curvind",
                               "rh_superiorparietal_curvind",
                               "rh_superiortemporal_curvind",
                               "rh_supramarginal_curvind",
                               "rh_frontalpole_curvind",
                               "rh_temporalpole_curvind",
                               "rh_transversetemporal_curvind",
                               "rh_insula_curvind"
                          ]
  for feature in rh_curvind_region_list:
    draw_shaded_plot(feature)

  lh_foldind_region_list =  [  "lh_bankssts_foldind",
                               "lh_caudalanteriorcingulate_foldind",
                               "lh_caudalmiddlefrontal_foldind",
                               "lh_cuneus_foldind",
                               "lh_entorhinal_foldind",
                               "lh_fusiform_foldind",
                               "lh_inferiorparietal_foldind",
                               "lh_inferiortemporal_foldind",
                               "lh_isthmuscingulate_foldind",
                               "lh_lateraloccipital_foldind",
                               "lh_lateralorbitofrontal_foldind",
                               "lh_lingual_foldind",
                               "lh_medialorbitofrontal_foldind",
                               "lh_middletemporal_foldind",
                               "lh_parahippocampal_foldind",
                               "lh_paracentral_foldind",
                               "lh_parsopercularis_foldind",
                               "lh_parsorbitalis_foldind",
                               "lh_parstriangularis_foldind",
                               "lh_pericalcarine_foldind",
                               "lh_postcentral_foldind",
                               "lh_posteriorcingulate_foldind",
                               "lh_precentral_foldind",
                               "lh_precuneus_foldind",
                               "lh_rostralanteriorcingulate_foldind",
                               "lh_rostralmiddlefrontal_foldind",
                               "lh_superiorfrontal_foldind",
                               "lh_superiorparietal_foldind",
                               "lh_superiortemporal_foldind",
                               "lh_supramarginal_foldind",
                               "lh_frontalpole_foldind",
                               "lh_temporalpole_foldind",
                               "lh_transversetemporal_foldind",
                               "lh_insula_foldind"
                            ]
  for feature in lh_foldind_region_list:
    draw_shaded_plot(feature)

  rh_foldind_region_list =  [  "rh_bankssts_foldind",
                               "rh_caudalanteriorcingulate_foldind",
                               "rh_caudalmiddlefrontal_foldind",
                               "rh_cuneus_foldind",
                               "rh_entorhinal_foldind",
                               "rh_fusiform_foldind",
                               "rh_inferiorparietal_foldind",
                               "rh_inferiortemporal_foldind",
                               "rh_isthmuscingulate_foldind",
                               "rh_lateraloccipital_foldind",
                               "rh_lateralorbitofrontal_foldind",
                               "rh_lingual_foldind",
                               "rh_medialorbitofrontal_foldind",
                               "rh_middletemporal_foldind",
                               "rh_parahippocampal_foldind",
                               "rh_paracentral_foldind",
                               "rh_parsopercularis_foldind",
                               "rh_parsorbitalis_foldind",
                               "rh_parstriangularis_foldind",
                               "rh_pericalcarine_foldind",
                               "rh_postcentral_foldind",
                               "rh_posteriorcingulate_foldind",
                               "rh_precentral_foldind",
                               "rh_precuneus_foldind",
                               "rh_rostralanteriorcingulate_foldind",
                               "rh_rostralmiddlefrontal_foldind",
                               "rh_superiorfrontal_foldind",
                               "rh_superiorparietal_foldind",
                               "rh_superiortemporal_foldind",
                               "rh_supramarginal_foldind",
                               "rh_frontalpole_foldind",
                               "rh_temporalpole_foldind",
                               "rh_transversetemporal_foldind",
                               "rh_insula_foldind"
                            ]
  for feature in rh_foldind_region_list:
    draw_shaded_plot(feature)

  lh_gauscurv_region_list = [   "lh_bankssts_gauscurv",
                                "lh_caudalanteriorcingulate_gauscurv",
                                "lh_caudalmiddlefrontal_gauscurv",
                                "lh_cuneus_gauscurv",
                                "lh_entorhinal_gauscurv",
                                "lh_fusiform_gauscurv",
                                "lh_inferiorparietal_gauscurv",
                                "lh_inferiortemporal_gauscurv",
                                "lh_isthmuscingulate_gauscurv",
                                "lh_lateraloccipital_gauscurv",
                                "lh_lateralorbitofrontal_gauscurv",
                                "lh_lingual_gauscurv",
                                "lh_medialorbitofrontal_gauscurv",
                                "lh_middletemporal_gauscurv",
                                "lh_parahippocampal_gauscurv",
                                "lh_paracentral_gauscurv",
                                "lh_parsopercularis_gauscurv",
                                "lh_parsorbitalis_gauscurv",
                                "lh_parstriangularis_gauscurv",
                                "lh_pericalcarine_gauscurv",
                                "lh_postcentral_gauscurv",
                                "lh_posteriorcingulate_gauscurv",
                                "lh_precentral_gauscurv",
                                "lh_precuneus_gauscurv",
                                "lh_rostralanteriorcingulate_gauscurv",
                                "lh_rostralmiddlefrontal_gauscurv",
                                "lh_superiorfrontal_gauscurv",
                                "lh_superiorparietal_gauscurv",
                                "lh_superiortemporal_gauscurv",
                                "lh_supramarginal_gauscurv",
                                "lh_frontalpole_gauscurv",
                                "lh_temporalpole_gauscurv",
                                "lh_transversetemporal_gauscurv",
                                "lh_insula_gauscurv"
                            ]
  for feature in lh_gauscurv_region_list:
    draw_shaded_plot(feature)

  rh_gauscurv_region_list = [   "rh_bankssts_gauscurv",
                                "rh_caudalanteriorcingulate_gauscurv",
                                "rh_caudalmiddlefrontal_gauscurv",
                                "rh_cuneus_gauscurv",
                                "rh_entorhinal_gauscurv",
                                "rh_fusiform_gauscurv",
                                "rh_inferiorparietal_gauscurv",
                                "rh_inferiortemporal_gauscurv",
                                "rh_isthmuscingulate_gauscurv",
                                "rh_lateraloccipital_gauscurv",
                                "rh_lateralorbitofrontal_gauscurv",
                                "rh_lingual_gauscurv",
                                "rh_medialorbitofrontal_gauscurv",
                                "rh_middletemporal_gauscurv",
                                "rh_parahippocampal_gauscurv",
                                "rh_paracentral_gauscurv",
                                "rh_parsopercularis_gauscurv",
                                "rh_parsorbitalis_gauscurv",
                                "rh_parstriangularis_gauscurv",
                                "rh_pericalcarine_gauscurv",
                                "rh_postcentral_gauscurv",
                                "rh_posteriorcingulate_gauscurv",
                                "rh_precentral_gauscurv",
                                "rh_precuneus_gauscurv",
                                "rh_rostralanteriorcingulate_gauscurv",
                                "rh_rostralmiddlefrontal_gauscurv",
                                "rh_superiorfrontal_gauscurv",
                                "rh_superiorparietal_gauscurv",
                                "rh_superiortemporal_gauscurv",
                                "rh_supramarginal_gauscurv",
                                "rh_frontalpole_gauscurv",
                                "rh_temporalpole_gauscurv",
                                "rh_transversetemporal_gauscurv",
                                "rh_insula_gauscurv"
                            ]
  for feature in rh_gauscurv_region_list:
    draw_shaded_plot(feature)

  lh_meancurv_region_list = [   "lh_bankssts_meancurv",
                                "lh_caudalanteriorcingulate_meancurv",
                                "lh_caudalmiddlefrontal_meancurv",
                                "lh_cuneus_meancurv",
                                "lh_entorhinal_meancurv",
                                "lh_fusiform_meancurv",
                                "lh_inferiorparietal_meancurv",
                                "lh_inferiortemporal_meancurv",
                                "lh_isthmuscingulate_meancurv",
                                "lh_lateraloccipital_meancurv",
                                "lh_lateralorbitofrontal_meancurv",
                                "lh_lingual_meancurv",
                                "lh_medialorbitofrontal_meancurv",
                                "lh_middletemporal_meancurv",
                                "lh_parahippocampal_meancurv",
                                "lh_paracentral_meancurv",
                                "lh_parsopercularis_meancurv",
                                "lh_parsorbitalis_meancurv",
                                "lh_parstriangularis_meancurv",
                                "lh_pericalcarine_meancurv",
                                "lh_postcentral_meancurv",
                                "lh_posteriorcingulate_meancurv",
                                "lh_precentral_meancurv",
                                "lh_precuneus_meancurv",
                                "lh_rostralanteriorcingulate_meancurv",
                                "lh_rostralmiddlefrontal_meancurv",
                                "lh_superiorfrontal_meancurv",
                                "lh_superiorparietal_meancurv",
                                "lh_superiortemporal_meancurv",
                                "lh_supramarginal_meancurv",
                                "lh_frontalpole_meancurv",
                                "lh_temporalpole_meancurv",
                                "lh_transversetemporal_meancurv",
                                "lh_insula_meancurv"
                            ]
  for feature in lh_meancurv_region_list:
    draw_shaded_plot(feature)

  rh_meancurv_region_list = [   "rh_bankssts_meancurv",
                                "rh_caudalanteriorcingulate_meancurv",
                                "rh_caudalmiddlefrontal_meancurv",
                                "rh_cuneus_meancurv",
                                "rh_entorhinal_meancurv",
                                "rh_fusiform_meancurv",
                                "rh_inferiorparietal_meancurv",
                                "rh_inferiortemporal_meancurv",
                                "rh_isthmuscingulate_meancurv",
                                "rh_lateraloccipital_meancurv",
                                "rh_lateralorbitofrontal_meancurv",
                                "rh_lingual_meancurv",
                                "rh_medialorbitofrontal_meancurv",
                                "rh_middletemporal_meancurv",
                                "rh_parahippocampal_meancurv",
                                "rh_paracentral_meancurv",
                                "rh_parsopercularis_meancurv",
                                "rh_parsorbitalis_meancurv",
                                "rh_parstriangularis_meancurv",
                                "rh_pericalcarine_meancurv",
                                "rh_postcentral_meancurv",
                                "rh_posteriorcingulate_meancurv",
                                "rh_precentral_meancurv",
                                "rh_precuneus_meancurv",
                                "rh_rostralanteriorcingulate_meancurv",
                                "rh_rostralmiddlefrontal_meancurv",
                                "rh_superiorfrontal_meancurv",
                                "rh_superiorparietal_meancurv",
                                "rh_superiortemporal_meancurv",
                                "rh_supramarginal_meancurv",
                                "rh_frontalpole_meancurv",
                                "rh_temporalpole_meancurv",
                                "rh_transversetemporal_meancurv",
                                "rh_insula_meancurv"
                            ]
  for feature in rh_meancurv_region_list:
    draw_shaded_plot(feature)                            

  lh_thicknessstd_region_list =  [  "lh_bankssts_thicknessstd",
                                    "lh_caudalanteriorcingulate_thicknessstd",
                                    "lh_caudalmiddlefrontal_thicknessstd",
                                    "lh_cuneus_thicknessstd",
                                    "lh_entorhinal_thicknessstd",
                                    "lh_fusiform_thicknessstd",
                                    "lh_inferiorparietal_thicknessstd",
                                    "lh_inferiortemporal_thicknessstd",
                                    "lh_isthmuscingulate_thicknessstd",
                                    "lh_lateraloccipital_thicknessstd",
                                    "lh_lateralorbitofrontal_thicknessstd",
                                    "lh_lingual_thicknessstd",
                                    "lh_medialorbitofrontal_thicknessstd",
                                    "lh_middletemporal_thicknessstd",
                                    "lh_parahippocampal_thicknessstd",
                                    "lh_paracentral_thicknessstd",
                                    "lh_parsopercularis_thicknessstd",
                                    "lh_parsorbitalis_thicknessstd",
                                    "lh_parstriangularis_thicknessstd",
                                    "lh_pericalcarine_thicknessstd",
                                    "lh_postcentral_thicknessstd",
                                    "lh_posteriorcingulate_thicknessstd",
                                    "lh_precentral_thicknessstd",
                                    "lh_precuneus_thicknessstd",
                                    "lh_rostralanteriorcingulate_thicknessstd",
                                    "lh_rostralmiddlefrontal_thicknessstd",
                                    "lh_superiorfrontal_thicknessstd",
                                    "lh_superiorparietal_thicknessstd",
                                    "lh_superiortemporal_thicknessstd",
                                    "lh_supramarginal_thicknessstd",
                                    "lh_frontalpole_thicknessstd",
                                    "lh_temporalpole_thicknessstd",
                                    "lh_transversetemporal_thicknessstd",
                                    "lh_insula_thicknessstd"
                                  ]
  for feature in lh_thicknessstd_region_list:
    draw_shaded_plot(feature)

  rh_thicknessstd_region_list =  [  "rh_bankssts_thicknessstd",
                                    "rh_caudalanteriorcingulate_thicknessstd",
                                    "rh_caudalmiddlefrontal_thicknessstd",
                                    "rh_cuneus_thicknessstd",
                                    "rh_entorhinal_thicknessstd",
                                    "rh_fusiform_thicknessstd",
                                    "rh_inferiorparietal_thicknessstd",
                                    "rh_inferiortemporal_thicknessstd",
                                    "rh_isthmuscingulate_thicknessstd",
                                    "rh_lateraloccipital_thicknessstd",
                                    "rh_lateralorbitofrontal_thicknessstd",
                                    "rh_lingual_thicknessstd",
                                    "rh_medialorbitofrontal_thicknessstd",
                                    "rh_middletemporal_thicknessstd",
                                    "rh_parahippocampal_thicknessstd",
                                    "rh_paracentral_thicknessstd",
                                    "rh_parsopercularis_thicknessstd",
                                    "rh_parsorbitalis_thicknessstd",
                                    "rh_parstriangularis_thicknessstd",
                                    "rh_pericalcarine_thicknessstd",
                                    "rh_postcentral_thicknessstd",
                                    "rh_posteriorcingulate_thicknessstd",
                                    "rh_precentral_thicknessstd",
                                    "rh_precuneus_thicknessstd",
                                    "rh_rostralanteriorcingulate_thicknessstd",
                                    "rh_rostralmiddlefrontal_thicknessstd",
                                    "rh_superiorfrontal_thicknessstd",
                                    "rh_superiorparietal_thicknessstd",
                                    "rh_superiortemporal_thicknessstd",
                                    "rh_supramarginal_thicknessstd",
                                    "rh_frontalpole_thicknessstd",
                                    "rh_temporalpole_thicknessstd",
                                    "rh_transversetemporal_thicknessstd",
                                    "rh_insula_thicknessstd"
                                  ]                        
  for feature in rh_thicknessstd_region_list:
    draw_shaded_plot(feature)

  lh_thickness_region_list =  [  "lh_bankssts_thickness",
                                 "lh_caudalanteriorcingulate_thickness",
                                 "lh_caudalmiddlefrontal_thickness",
                                 "lh_cuneus_thickness",
                                 "lh_entorhinal_thickness",
                                 "lh_fusiform_thickness",
                                 "lh_inferiorparietal_thickness",
                                 "lh_inferiortemporal_thickness",
                                 "lh_isthmuscingulate_thickness",
                                 "lh_lateraloccipital_thickness",
                                 "lh_lateralorbitofrontal_thickness",
                                 "lh_lingual_thickness",
                                 "lh_medialorbitofrontal_thickness",
                                 "lh_middletemporal_thickness",
                                 "lh_parahippocampal_thickness",
                                 "lh_paracentral_thickness",
                                 "lh_parsopercularis_thickness",
                                 "lh_parsorbitalis_thickness",
                                 "lh_parstriangularis_thickness",
                                 "lh_pericalcarine_thickness",
                                 "lh_postcentral_thickness",
                                 "lh_posteriorcingulate_thickness",
                                 "lh_precentral_thickness",
                                 "lh_precuneus_thickness",
                                 "lh_rostralanteriorcingulate_thickness",
                                 "lh_rostralmiddlefrontal_thickness",
                                 "lh_superiorfrontal_thickness",
                                 "lh_superiorparietal_thickness",
                                 "lh_superiortemporal_thickness",
                                 "lh_supramarginal_thickness",
                                 "lh_frontalpole_thickness",
                                 "lh_temporalpole_thickness",
                                 "lh_transversetemporal_thickness",
                                 "lh_insula_thickness"
                              ]
  for feature in lh_thickness_region_list:
    draw_shaded_plot(feature)

  rh_thickness_region_list =  [  "rh_bankssts_thickness",
                                 "rh_caudalanteriorcingulate_thickness",
                                 "rh_caudalmiddlefrontal_thickness",
                                 "rh_cuneus_thickness",
                                 "rh_entorhinal_thickness",
                                 "rh_fusiform_thickness",
                                 "rh_inferiorparietal_thickness",
                                 "rh_inferiortemporal_thickness",
                                 "rh_isthmuscingulate_thickness",
                                 "rh_lateraloccipital_thickness",
                                 "rh_lateralorbitofrontal_thickness",
                                 "rh_lingual_thickness",
                                 "rh_medialorbitofrontal_thickness",
                                 "rh_middletemporal_thickness",
                                 "rh_parahippocampal_thickness",
                                 "rh_paracentral_thickness",
                                 "rh_parsopercularis_thickness",
                                 "rh_parsorbitalis_thickness",
                                 "rh_parstriangularis_thickness",
                                 "rh_pericalcarine_thickness",
                                 "rh_postcentral_thickness",
                                 "rh_posteriorcingulate_thickness",
                                 "rh_precentral_thickness",
                                 "rh_precuneus_thickness",
                                 "rh_rostralanteriorcingulate_thickness",
                                 "rh_rostralmiddlefrontal_thickness",
                                 "rh_superiorfrontal_thickness",
                                 "rh_superiorparietal_thickness",
                                 "rh_superiortemporal_thickness",
                                 "rh_supramarginal_thickness",
                                 "rh_frontalpole_thickness",
                                 "rh_temporalpole_thickness",
                                 "rh_transversetemporal_thickness",
                                 "rh_insula_thickness"
                              ]
  for feature in rh_thickness_region_list:
    draw_shaded_plot(feature)

  print("Done")
  # end of draw_all_plots()
