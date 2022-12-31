# install.packages("XLConnect")
# install.packages("reticulate")
# install.packages("VGAM")
# install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(XLConnect)

library(VGAM)
library(reshape2)
library(reticulate)  # this package enable python in R

use_python("/usr/local/bin/python3")

wd <- getwd()
setwd(wd)
source_python("draw_shaded_plot.py")
source_python("harmonDataBuilder_9_R_age_key.py")
source_python("categorize_data.py")

gen_raw_harmo_data <- function() {
  file_list <- list("all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_LH_thicknessstd_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_thickness_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_LH_foldind_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_curvind_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_thicknessstd_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_LH_gauscurv_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_foldind_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_LH_meancurv_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_gauscurv_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_LH_thickness_09_15_2022_preHarmo.csv",
                    "all_subjects_cortical_metrics_RH_meancurv_09_15_2022_preHarmo.csv")
  
  for (file_path in file_list) {
    print(paste("working on :", file_path, sep = ''))
    file_name <- strsplit(file_path, "\\.")[[1]][1]
    file_extension <- strsplit(file_path, "\\.")[[1]][2]
    data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
    row_num_before <- nrow(data_raw)
    
    data_raw_1 <- data_raw[ which(data_raw$Age!="" & data_raw$Age!="Siemens"  &
                                    (data_raw$Sex==1 | data_raw$Sex==2 |
                                       data_raw$Sex=="M" | data_raw$Sex=="m" | data_raw$Sex=="F" |
                                       data_raw$Sex=="f")
    ),  ]
    row_num_after_sex <- nrow(data_raw_1)
    
    outfile <- paste(file_name, '_clean', '.csv', sep="")
    print(paste("creating cleanfile: ", outfile, sep = ''))
    write.csv(data_raw_1, outfile, row.names = FALSE, quote = FALSE)
    
    # harmonize the outfile
    harmo_file <- gsub("preHarmo", "postHarmo", outfile)
    print(paste("creating harmo file: ", harmo_file, sep = ''))
    print("------------")
    harmonize(outfile, harmo_file, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE)
  }
}


adjustByMeanData <- function(data, feature, byETIV=TRUE, IsLH=TRUE) {
  if (byETIV) {
    data1 <-data[data$eTIV != 0, ]
    adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$eTIV)))
    return(adjByMeanData)
  } else {
    # this is by chickness case
    if (IsLH) {
      data1 <-data[data$lh_MeanThickness_thickness != 0, ]
      adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$lh_MeanThickness_thickness)))
    } else {
      data1 <-data[data$rh_MeanThickness_thickness != 0, ]
      adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$rh_MeanThickness_thickness)))
    }
    return(adjByMeanData)
  }
}

getRobustZScoreByDataset <- function(data, feature, byETIV=TRUE, IsLH=TRUE) {
  if (byETIV) {
    eTIV_data<-adjustByMeanData(data, get(feature), byETIV)
    grouped_data <- eTIV_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
    merged_data <- merge(eTIV_data, grouped_data, by="Dataset")
  } else {
    if (IsLH) {
      lh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), byETIV, IsLH)
      grouped_data <- lh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(lh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    } else {
      rh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), byETIV, IsLH)
      grouped_data <- rh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(rh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    }
  }

  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - group_median)/group_mad)
  return(zscore_data)
}

rmOutlier <- function(data, low, high) {
  #data[data$zscore<high & data$zscore>low, ]
  data_no_outlier <- subset(data, data$zscore>low & data$zscore < high)
  data_no_outlier
}

gender_suffix <- function(sex=3) {
  if (sex == 1) {
    sex_suffix = "_male"
  } else if (sex==2) {
    sex_suffix = "_female"
  } else {
    sex_suffix = "_all"
  }
  return( sex_suffix)
}

sort_by_sex <- function(data, sex=3) {
  if (sex == 1) {
    sexSubset <- data %>% filter(Sex=="M" | Sex=="1")
  } else if (sex==2) {
    sexSubset <- data %>% filter(Sex=="F" | Sex=="2")
  } else {
    sexSubset <- data
  }
  return(sexSubset)
}

# sex: 1-> Male; 2->Female; 3->all
data_analyse <- function(data_raw, data_harmo, feature, gen_csv=TRUE, gen_TukeyHSD=FALSE, 
                low=-3.5, high=3.5, sex=3, byETIV=TRUE, wb=NULL, index=0, IsLH=TRUE, gen_shaded_plot=FALSE) {
  print(paste("working on ", feature, sep = ''))
  # step1: generation raw file
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature, byETIV, IsLH)
  rawSubset <- data_raw_z %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawSubset, sex)
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  raw_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  # step2: generation raw_no_outlier file
  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature, byETIV, IsLH)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  if (byETIV) {
    rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, eTIV, zscore, {{feature}})
  } else {  # by chickness cases
    if (IsLH) {
      rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, lh_MeanThickness_thickness, zscore, {{feature}})
    } else {
      rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, rh_MeanThickness_thickness, zscore, {{feature}})
    }
  }
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  rawNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
    # Note: the output raw_no_outlier file contains zscore column. But that zscore coloumn will be ignored during later on harmonization step
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  # # step3: generating harmon file. Note: this harmon file is based on raw (before outlier removal) data 
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature, byETIV, IsLH)
  harmoSubset <- data_harmo_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harm_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }
  
  # step4: generation harmo_no_outlier file. This is based on the step 2 output.
  src_no_file <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
  outfile <- paste(wd, '/', feature, '_harmo_no', gender_suffix(sex), '.csv', sep="")
  rslt <- harmonize(src_no_file, outfile, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE, gen_plot=TRUE)
   if (!rslt) {
    print(paste("Harmonization failed for", feature))
    return(FALSE)
  }

  # draw shaded plot
  if (gen_shaded_plot) {
    rslt <- draw_shaded_plot(feature)
    if (!rslt) {
      print(paste("failed to draw shaded plot for", feature, "!!!"))
    }
  }

  data_harmo_no <- read.csv(outfile, stringsAsFactors = TRUE)
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo_no, feature, byETIV, IsLH)
  # harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoSubset <-  data_harmo_z1 %>% select(Dataset, Age, Sex, zscore, {{feature}})
  #sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harmoNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  if (!is.null(wb)) {
    writeWorksheet(wb, raw_Anova, sheet="workSheet", startRow=index*3+2, startCol = 3)
    writeWorksheet(wb, rawNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 9)
    writeWorksheet(wb, harm_Anova, sheet="workSheet", startRow=index*3+2, startCol = 15)
    writeWorksheet(wb, harmoNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 21)
  }
  
  sink("anova_analysis.txt", append=TRUE)
  print(paste("==============", feature, "=============="))
  print(summary(raw_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(raw_aov_rst))
  }

  print(summary(rawNoOutlier_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(rawNoOutlier_aov_rst))
  }
  print(summary(harm_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(harm_aov_rst))
  }
  print(summary(harmoNoOutlier_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(harmoNoOutlier_aov_rst))
  }
  sink(NULL)
}

# option: 1->raw; 2->raw_wo_outlier; 3->harmo; 4->harmo_wo_outlier
# sex: 1->Male; 2->Female; 3->All gender
draw_plot <- function(feature, option=1, sex=3, is_boxplot=FALSE) {
  print(paste("ploting", feature))
  if (option==1) {
    suffix <- "raw"
    title <- paste("Raw", feature, gender_title(sex), sep=" ")
  } else if (option==2) {
    suffix <- "raw_no"
    title <- paste(feature, "with outlier removed", gender_title(sex), sep=" ")
  } else if (option==3) {
    suffix <- "harmo"
    title <- paste("Harmonized", feature, gender_title(sex), sep=" ")
  } else if (option==4) {
    suffix <- "harmo_no"
    title <- paste("Harmonized", feature, "with outlier removed", gender_title(sex), sep=" ")
  } else {
    print("invalid option")
    return()
  }
  file_path <- paste(wd, '/', feature, '_', suffix, gender_suffix(sex), '.csv', sep = "")
  print(file_path)
  gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE)
  gg_data <- gg_data0 %>% filter(Age !="") %>% filter(get(feature)!=0 & get(feature)>0)
  
  if (is_boxplot) {
    v <- ggplot(data=gg_data, aes(x=Dataset, y=zscore, color=Dataset)) +
      geom_boxplot() + ggtitle(title) + ylab(feature) + xlab('Dataset') +
      theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5))
    return(v)
  }
  
  dmelt <- NULL
  fitted.values <- NULL
  fit4 <- NULL
  draw_vgam <- tryCatch( 
    {
      #fit4 <- vgam(get(feature) ~ s(Age, df = c(4, 2)), lms.bcn(zero = 1), data = gg_data, trace = TRUE)
      fit4 <- vgam(get(feature) ~ s(Age, df = 2), lms.bcn(zero = 1), data = gg_data, trace = TRUE, 
                    na.action=na.omit,  eps = 1e-16)
      if (is.null(fit4)) {
        print("fit4 is null")
      }
      fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(1,25,50,75,99))$fitted.values)
      fitted.values[, 'Dataset'] <- gg_data[, 'Dataset']
      fitted.values[, 'Age'] <- gg_data[, 'Age']
      fitted.values[, 'Sex'] <- gg_data[, 'Sex']
      fitted.values[, feature] <- gg_data[, feature]
      
      dmelt <- melt(fitted.values, id.vars=c('Dataset', 'Age', 'Sex', feature))
      TRUE
    },
    
    error = function(err) {
      print(paste("Skipping vgam: ", file_path))
      print(paste("MY_ERROR: ", err))
      FALSE
    }
  )
  
  if (draw_vgam) {
    u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.5) + ggtitle(title) + ylab(feature)+ xlab('Age')
    u <- u + geom_line(aes(Age, value, group=variable), color='black')
  } else {
    u <- ggplot(data=gg_data, aes(x=Age, y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.5) + geom_point(size=0.5) +
          ggtitle(title) + ylab(feature)+ xlab('Age')
    u <- u +  geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5))
  }
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=5))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  if (draw_vgam) {
    u <- u + annotate(geom='text',
                      x = 98,
                      y = unlist(fitted.values[which.max(fitted.values[, 'Age']), c(1:5)]),
                      label=c('1%', '25%', '50%', '75%', '99%'), size=3)
  }
  u
  return(draw_vgam)
}


set_anova_skeleton <- function(wb, region_list) {
  # Each region take three rows. The 1st row is region name, followed by 2 empty rows
  region.df <- data.frame(Region=c(''))
  writeWorksheet(wb, region.df, sheet="workSheet", startRow=1, startCol = 1)
  index <- 0
  for (region_name in region_list) {
    region.df <- data.frame(place_holder=c(''))
    colnames(region.df) <- c(region_name)
    writeWorksheet(wb, region.df, sheet="workSheet", startRow=index*3+2, startCol = 1)
    index <- index + 1
  }

  Raw.df <- data.frame(Raw=c(''))
  Raw_no_outlier.df <- data.frame(Raw_wo_outliers=c(''))
  harmo.df <- data.frame(Harmonized=c(''))
  harmo_no_outlier.df <- data.frame(Harmonized_wo_outliers=c(''))
  writeWorksheet(wb, Raw.df, sheet="workSheet", startRow=1, startCol = 3)
  writeWorksheet(wb, Raw_no_outlier.df, sheet="workSheet", startRow=1, startCol = 9)
  writeWorksheet(wb, harmo.df, sheet="workSheet", startRow=1, startCol = 15)
  writeWorksheet(wb, harmo_no_outlier.df, sheet="workSheet", startRow=1, startCol = 21)
}

gen_4_sheet_all_regions <- function(data_raw=NULL, data_harmo=NULL, sex=3, byETIV=TRUE,
                                    region_list=lh_thickness_region_list,
                                    outfile="unnamed_anova.xlsx", genTukeyHSD=FALSE,
                                    genShadedPlot=FALSE, isLH=FALSE) {
  # generate skeleton anova output file
  wb <- loadWorkbook(outfile, create = TRUE)
  createSheet(wb, name="workSheet")
  set_anova_skeleton(wb, region_list)
  
  index <- 0
  for (region_name in region_list) {
    data_analyse(data_raw, data_harmo, region_name, TRUE, genTukeyHSD, -3.5, 3.5,
                 sex, byETIV, wb, index, IsLH=isLH, gen_shaded_plot=genShadedPlot)
    index <- index + 1
  }
  
  saveWorkbook(wb)
  
  print( paste("All Done!!!  Finished total", index, " regions."))
}

gender_title <- function(sex=3) {
  if (sex == 1) {
    sex_suffix = "(male)"
  } else if (sex==2) {
    sex_suffix = "(female)"
  } else {
    sex_suffix = "(all gender)"
  }
  return( sex_suffix)
}

gen_file_path <- function(feature, option, sex) {
  if (option==1) {
    suffix <- "raw"
  } else if (option==2) {
    suffix <- "raw_no_outlier"
  } else if (option==3) {
    suffix <- "harmo"
  } else if (option==4) {
    suffix <- "harmo_no_outlier"
  } else {
    print("invalid option")
    return()
  }
  file_path <- paste(wd, '/', feature, '_', suffix, gender_suffix(sex), '.csv', sep = "")
  return (file_path)
}

gen_plot_all_region <- function(region_list, sex) {
  skip_count <- 0
  created_file_count <- 0
  skip_file_list <- list()
  option_list <- list("_raw", "_raw_no", "_harmo", "_harmo_no")
  sex_list <- list("_male", "_female", "_all")
  for (region_name in region_list) {
    # for (option in 1:4) {
    # for now we only care about after harmonization and after outliers removed data 
    for (option in 4:4) {
      draw_vgam <- draw_plot(region_name, option, sex)
      if (draw_vgam) {
        created_file_count <- created_file_count + 1
        file_name <-paste(region_name, option_list[option], sex_list[sex], '.png', sep='')
      } else {
        skip_file <- gen_file_path(region_name, option, sex)
        skip_file_list <- append(skip_file_list, skip_file)
        skip_count <- skip_count + 1
        file_name <-paste(region_name, option_list[option], sex_list[sex], '_s', '.png', sep='')
      }
      ggsave(file_name)
    }
  }
  
  if (created_file_count > 0 ) {
    print(paste("There are ", created_file_count, " plot files generated", sep = ''))
  }
  
  if (skip_count > 0) {
    print(paste("Failed to generate images for the following ", skip_count, " file due to vgram issue:", sep = ''))
    for (i in 1:skip_count) {
      print(paste("  ", skip_file_list[i]))
    }
  }
}

gen_boxplot_all_region <- function(region_list, sex) {
  option_list <- list("_raw", "_raw_no_outlier", "_harmo", "_harmo_no_outlier")
  sex_list <- list("_male", "_female", "_all")
  for (region_name in region_list) {
    for (option in 1:4) {
      draw_plot(region_name, option, sex, is_boxplot=TRUE)
      file_name = paste(region_name, option_list[option], sex_list[sex], '_boxplot', '.png', sep='')
      print(paste("generating:", file_name))
      ggsave(file_name)
    }
  }
}

# this function draw plots that shows shaded left and right hemisphere comparision
gen_hemisphere_plots<- function(feature_list) {
  for (rh_feature in feature_list) {
    # get feature name without hemsphere prefix. like 
    # rh_entorhinal_thickness -> entorhinal_thickness
    feature = substr(rh_feature, 4, nchar(rh_feature))
    lh_feature = paste("lh_", feature, sep='')
    data_lh <- read.csv(paste(lh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE)
    data_rh <- read.csv(paste(rh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE)
    
    # remove "lh_MeanThickness_thickness" or "eTIV" from data_lh
    # we need this step; otherwise, the left and right data table would have different column name
    # then they cannot be combined
    if ("lh_MeanThickness_thickness" %in% colnames(data_lh)) {
      # drop lh_MeanThickness_thickness column
      data_lh = subset(data_lh, select = -c(lh_MeanThickness_thickness))
    }
    
    # rename lh_entorhinal_thickness to entorhinal_thickness
    colnames(data_lh)[colnames(data_lh) == lh_feature] = feature
    # add column "hemsphere" and set value "left"
    data_lh["hemisphere"] = 'left'
    
    # remove "rh_MeanThickness_thickness" or "eTIV" from data_rh
    if ("rh_MeanThickness_thickness" %in% colnames(data_rh)) {
      # drop rh_MeanThickness_thickness column
      data_rh = subset(data_rh, select = -c(rh_MeanThickness_thickness))
    }
    
    # rename rh_entorhinal_thickness to entorhinal_thickness
    colnames(data_rh)[colnames(data_rh) == rh_feature] = feature
    # add column "hemsphere" and set value "right"
    data_rh["hemisphere"] = 'right'
    
    # combined the left and right table
    combined_data = rbind(data_lh, data_rh)
    
    outfile = paste(feature, "_hemisphere.csv", sep='')
    write.csv(combined_data, outfile, row.names=FALSE, quote=FALSE)
    
    draw_shade_hemisphere_plot(feature)
  }
}




process_lh_curvind <- function() {
  lh_curvind_region_list <- list("lh_bankssts_curvind",
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
                                 "lh_insula_curvind")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_curvind_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=lh_curvind_region_list, outfile="lh_curvind_anova_age_as_key.xlsx", 
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_curvind_region_list, 3)
}

process_rh_curvind <- function(draw_hemi_plot) {
  rh_curvind_region_list <- list("rh_bankssts_curvind",
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
                                 "rh_insula_curvind")
  
  data_raw <- read.csv("all_subjects_cortical_metrics_RH_curvind_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_curvind_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=rh_curvind_region_list, outfile="rh_curvind_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_curvind_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_curvind_region_list)
  }
}
  
process_lh_foldind <- function() {
  lh_foldind_region_list <- list("lh_bankssts_foldind",
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
                                 "lh_insula_foldind")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_foldind_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_foldind_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=lh_foldind_region_list, outfile="lh_foldind_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_foldind_region_list, 3)
}

process_rh_foldind <- function(draw_hemi_plot) {
  rh_foldind_region_list <- list("rh_bankssts_foldind",
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
                                 "rh_insula_foldind")
  data_raw <- read.csv("all_subjects_cortical_metrics_RH_foldind_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_foldind_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=rh_foldind_region_list, outfile="rh_foldind_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_foldind_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_foldind_region_list)
  }
}
  
  
  
process_lh_gauscurv <- function() {
  lh_gauscurv_region_list <- list("lh_bankssts_gauscurv",
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
                                  "lh_insula_gauscurv")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_gauscurv_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_gauscurv_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_gauscurv_region_list, 3)
}

process_rh_gauscurv <- function(draw_hemi_plot) {
  rh_gauscurv_region_list <- list("rh_bankssts_gauscurv",
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
                                  "rh_insula_gauscurv")
  data_raw <- read.csv("all_subjects_cortical_metrics_RH_gauscurv_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_gauscurv_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_gauscurv_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_gauscurv_region_list)
  }
}

process_lh_meancurv <- function() {
  lh_meancurv_region_list <- list("lh_bankssts_meancurv",
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
                                  "lh_insula_meancurv")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_meancurv_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_meancurv_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_meancurv_region_list, 3)
}

process_rh_meancurv <- function(draw_hemi_plot) {
  rh_meancurv_region_list <- list("rh_bankssts_meancurv",
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
                                  "rh_insula_meancurv")
  data_raw <- read.csv("all_subjects_cortical_metrics_RH_meancurv_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_meancurv_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_meancurv_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_meancurv_region_list)
  }
}

process_lh_thicknessstd <- function() {
  lh_thicknessstd_region_list <- list("lh_bankssts_thicknessstd",
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
                                      "lh_insula_thicknessstd")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_thicknessstd_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_thicknessstd_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_thicknessstd_region_list, 3)
}

process_rh_thicknessstd <- function(draw_hemi_plot) {
  rh_thicknessstd_region_list <- list("rh_bankssts_thicknessstd",
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
                                      "rh_insula_thicknessstd")
  data_raw <- read.csv("all_subjects_cortical_metrics_RH_thicknessstd_09_15_2022_preHarmo_clean.csv",
                       stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_thicknessstd_09_15_2022_postHarmo_clean.csv",
                         stringsAsFactors = TRUE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova_age_as_key.xlsx",
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_thicknessstd_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_thicknessstd_region_list)
  }
}

process_lh_thickness <- function() {
  lh_thickness_region_list <- list("lh_bankssts_thickness",
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
                                   "lh_insula_thickness")
  data_raw <- read.csv("all_subjects_cortical_metrics_LH_thickness_09_15_2022_preHarmo_clean.csv", stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_LH_thickness_09_15_2022_postHarmo_clean.csv", stringsAsFactors = TRUE)

  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=FALSE, 
                          region_list=lh_thickness_region_list, outfile="lh_thickness_anova_age_as_key.xlsx", isLH=TRUE, 
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(lh_thickness_region_list, 3)
}

process_rh_thickness <- function(draw_hemi_plot) {
  rh_thickness_region_list <- list("rh_bankssts_thickness",
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
                                   "rh_insula_thickness")

  data_raw <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_preHarmo_clean.csv", stringsAsFactors = TRUE)
  data_harmo <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_postHarmo_clean.csv", stringsAsFactors = TRUE)

  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=FALSE, 
                          region_list=rh_thickness_region_list, outfile="rh_thickness_anova_age_as_key.xlsx", isLH=FALSE,
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(rh_thickness_region_list, 3)
  
  if (draw_hemi_plot) {
    gen_hemisphere_plots(rh_thickness_region_list)
  }
}

process_all <- function(draw_hemi_plot=TRUE, clean_leftover=TRUE) {
  gen_raw_harmo_data()
  process_lh_curvind()
  process_rh_curvind(draw_hemi_plot)
  process_lh_foldind()
  process_rh_foldind(draw_hemi_plot)
  process_lh_gauscurv()
  process_rh_gauscurv(draw_hemi_plot)
  process_lh_meancurv()
  process_rh_meancurv(draw_hemi_plot)
  process_lh_thicknessstd()
  process_rh_thicknessstd(draw_hemi_plot)
  process_lh_thickness()
  process_rh_thickness(draw_hemi_plot)
  categorize_data(clean_leftover)
}


#  The followings are test code 
### 1. lh_curvind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_age_as_key.xlsx", 
                        genTukeyHSD=TRUE, genShadedPlot=TRUE)
lh_curvind_region_list1 <- list("lh_bankssts_curvind")
gen_plot_all_region(lh_curvind_region_list1, 3)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_male.xlsx")
gen_plot_all_region(lh_curvind_region_list, 1)

  
gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE,
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_female.xlsx")
gen_plot_all_region(lh_curvind_region_list, 2)


### 2. rh_curvind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova_male.xlsx")
gen_plot_all_region(rh_curvind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova_female.xlsx")
gen_plot_all_region(rh_curvind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova.xlsx")
gen_plot_all_region(rh_curvind_region_list, 3)

### 3. lh_foldind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova_male.xlsx")
gen_plot_all_region(lh_foldind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova_female.xlsx")
gen_plot_all_region(lh_foldind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova.xlsx")
gen_plot_all_region(lh_foldind_region_list, 3)

### 4. rh_foldind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova_male.xlsx")
gen_plot_all_region(rh_foldind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova_female.xlsx")
gen_plot_all_region(rh_foldind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova.xlsx")
gen_plot_all_region(rh_foldind_region_list, 3)


### 5. lh_gauscurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova_male.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova_female.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 3)

### 6. rh_gauscurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova_male.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova_female.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 3)


### 7. lh_meancurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova_male.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova_female.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 3)

### 8. rh_meancurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova_male.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova_female.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 3)

### 9. lh_thicknessstd
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova_male.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova_female.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 3)

### 10. rh_thicknessstd
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova_male.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova_female.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 3)



rh_thicknessstd_region_list_1 <- list("rh_bankssts_thicknessstd")
rh_thicknessstd_region_list_2 <- list("rh_caudalanteriorcingulate_thicknessstd")

gen_plot_all_region(rh_thicknessstd_region_list, 1)

###################### test cmds begin ######################

rh_curvind_region_list_1 <- list("rh_bankssts_curvind")


gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV =TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova_male.xlsx")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV =  TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova_female.xlsx")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV =TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova.xlsx")


draw_plot("rh_bankssts_curvind", option=2, sex=1, is_boxplot=FALSE)

gen_plot_all_region(rh_curvind_region_list_1, 1)

###################### test cmds end ######################



