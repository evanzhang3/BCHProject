#!/usr/bin/env python3

from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import os
import re
import sys
import argparse
import math

# This script harmonizes data from input pre-harmo file and generates post-harmo output file.
# the covars for neuroCombat are: { 'scanner magnetic strength' }
# and categorical_cols are {'sex', 'Scanner_type' }

# cmd line format: python harmonDataBuilder_8_abcd.py -f pre_harmo.csv -o post_harmo.csv

def extractDataSet(filename):
  ageList_ = []
  genderList_ = []
  # scannerList is for Magnetic_field_of_strength
  scannerList_ = []
  scannerTypeList_ = []
  datasetList_ = []
  headline_ = []
  extractedDataDir_ = {}

  with open(filename, 'r') as f:
    lines = f.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()

    for headItem in headline_:
      extractedDataDir_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      for i in range(len(headline_)):
        extractedDataDir_[headline_[i]].append(linelist[i].strip()) # use strip() to remove "\n"

    # generate genderList: Male->1; Female->2; unknown->3
    # for ABCD dataset, sex directly shows as number: Male->1; Female->2; unknown->3
    for i in range(len(extractedDataDir_['Sex'])):
      if extractedDataDir_['Sex'][i] == "M" or extractedDataDir_['Sex'][i] == '1':
        genderList_.append(1)
      elif extractedDataDir_['Sex'][i] == "F" or extractedDataDir_['Sex'][i] == '2':
        genderList_.append(2)
      else:
        #print("for i=%d sex is:%s" % (i, extractedDataDir_['Sex'][i]))
        genderList_.append(3)

    # generate Scanner Strength list: 1.5T->1; 3T->2; other->3
    # for ABCD dataset, strength shows as number
    for i in range(len(extractedDataDir_['Magnetic_field_of_strength'])):
      if extractedDataDir_['Magnetic_field_of_strength'][i] == "1.5T" or \
           extractedDataDir_['Magnetic_field_of_strength'][i] == "1.494T" or \
           extractedDataDir_['Magnetic_field_of_strength'][i] == '1':
        scannerList_.append(1)
      elif extractedDataDir_['Magnetic_field_of_strength'][i] == "3T" or \
        extractedDataDir_['Magnetic_field_of_strength'][i] == '2':
        scannerList_.append(2)
      else:
        #print("for i=%d strength is: %s" % (i, extractedDataDir_['Magnetic_field_of_strength'][i]))
        scannerList_.append(3)

    # generate Scanner_type list: Siemens=1, GE=2, Philips=3, Blank=4
    # for ABCD dataset, Scanner_type shows as number
    for i in range(len(extractedDataDir_['Scanner_type'])):
      if extractedDataDir_['Scanner_type'][i] == 'Siemens' or \
        extractedDataDir_['Scanner_type'][i] == '1':
        scannerTypeList_.append(1)
      elif extractedDataDir_['Scanner_type'][i] == 'GE' or \
        extractedDataDir_['Scanner_type'][i] == '2':
        scannerTypeList_.append(2)
      elif extractedDataDir_['Scanner_type'][i] == 'Philips' or \
        extractedDataDir_['Scanner_type'][i] == '3':
        scannerTypeList_.append(3)
      else:
        #print("for i=%d Scanner_type:%s" % (i, extractedDataDir_['Scanner_type'][i]))
        scannerTypeList_.append(4)

    # generate dataset list:
    for i in range(len(extractedDataDir_['Dataset'])):
      if extractedDataDir_['Dataset'][i] == "ABIDE_I":
        datasetList_.append(1)
      elif extractedDataDir_['Dataset'][i] == "BCH":
        datasetList_.append(2)
      elif extractedDataDir_['Dataset'][i] == "beijingEn":
        datasetList_.append(3)
      elif extractedDataDir_['Dataset'][i] == "BGSP":
        datasetList_.append(4)
      elif extractedDataDir_['Dataset'][i] == "DLBS":
        datasetList_.append(5)
      elif extractedDataDir_['Dataset'][i] == "IXI_600":
        datasetList_.append(6)
      elif extractedDataDir_['Dataset'][i] == "MGH":
        datasetList_.append(7)
      elif extractedDataDir_['Dataset'][i] == "NIH_PD":
        datasetList_.append(8)
      elif extractedDataDir_['Dataset'][i] == "OASIS_3":
        datasetList_.append(9)
      elif extractedDataDir_['Dataset'][i] == "ABCD":
        datasetList_.append(10)
      elif extractedDataDir_['Dataset'][i] == "NKI_Rockland":
        datasetList_.append(11)
      elif extractedDataDir_['Dataset'][i] == "MCIC":
        datasetList_.append(12)
      else:
        print("Unknow dataset: %s" % extractedDataDir_['Dataset'][i])

    #generate age list:
    for i in range(len(extractedDataDir_['Age'])):
      ageList_.append(math.ceil(float(extractedDataDir_['Age'][i])))

    return ageList_, genderList_, scannerList_, scannerTypeList_, datasetList_, headline_, extractedDataDir_

def harmonize(srcfile, output, scanertype=False, dataset=False, Eb=True, Mean_only=False, Parametric=True, Age=False, batch_key=1, gen_plot=False):
  # eb: if False, each feature should be fit separately. True by default
  # parametric: should parametric adjustment be performed? True by default. (This cannot set to FALSE, otherwise, cause error)
  # mean_only: should only be means adjusted (no scaling)? False by default
  # gen_plot: generate file for ploting. Its age are set to age's ceiling value so that can be used as discrete value. For now
  # we only do that for harmonized data after outlier removal
  if not os.path.exists(srcfile):
    print("harmonize(): source file: %s does not exist!" % srcfile)
    return False

  ageList0, genderList0, scannerList0, scannerTypeList0, datasetList0, headline, extractedDataDir = \
    extractDataSet(srcfile)

  # postHarmonCombinedData is to save all the postHarmon data into a Dict
  postHarmonCombinedData = {}

  harmoDataList = []
  harmoColList = []
  for colname, datalist in extractedDataDir.items():
    if colname == "Dataset" or colname == "subjectId" or colname == "Age" or colname == "path" \
      or colname == "Sex" or colname == "Scanner_type" or colname == "Magnetic_field_of_strength" \
      or colname == "Volume" or colname == "Type" or colname == "zscore":
      continue

    harmoColList.append(colname)
    harmoDataList.append(datalist)

  inputArr = np.array(harmoDataList, dtype=float)

  covars =  {
              'batch': scannerList0,
              'gender': genderList0,
              #'scannerType': scannerTypeList0
              # 'dataset': datasetList0
            }

  if scanertype:
    covars['scanertype'] = scannerTypeList0

  if dataset:
    covars['Dataset'] = datasetList0

  if Age:
    covars['Age'] = ageList0

  covars = pd.DataFrame(covars)  

  # To specify names of the variables that are categorical:
  # categorical_cols = ['gender', 'dataset']
  categorical_cols = ['gender']
  if scanertype:
    categorical_cols.append('scanertype')
  if dataset:
    categorical_cols.append('Dataset')
  if Age:
    categorical_cols.append('Age')

  # To specify the name of the variable that encodes for the scanner/batch covariate:
  batch_col = 'batch'

  batch_col = 'Age'
  categorical_cols.remove('Age')
  categorical_cols.append('batch')

  #Harmonization step:
  try:
    data_combat = neuroCombat(dat=inputArr,
        covars=covars,
        batch_col=batch_col,
        categorical_cols=categorical_cols,
        mean_only=Mean_only,
        eb=Eb,
        parametric=Parametric)["data"]
  except Exception as e:
    print("data_combat failed!!!")
    print(e)
    print("batch len: %d.... gender len: %d" % (len(covars["batch"]), len(covars["gender"])))
    print("------------------")
    return False

  for i in range(len(harmoColList)):
    postHarmonCombinedData[harmoColList[i]] = data_combat[i]

  # Create single output PostHarm file
  with open(output, "w") as f:
    colnamelist = postHarmonCombinedData.keys()
    f.write("#,Dataset,Age,Sex,%s\n" % (','.join(colnamelist)))
    for i in range(len(genderList0)):
      # build data list
      postDatalist = []
      for key in colnamelist:
        postDatalist.append(postHarmonCombinedData[key][i])
      # note: some data elements are number (float), so use map(str, list) to change them to string
      f.write("%d,%s,%s,%s,%s\n" % ((i+1),\
        extractedDataDir['Dataset'][i], extractedDataDir['Age'][i], extractedDataDir['Sex'][i], ','.join(map(str, postDatalist))))

  # the following code generate csv file for shaded plots. The only difference from the above created file is that
  # the the age is integer (ceiling of age). This is required by seaborn and matplotlib python module
  if (gen_plot):
    # Create single output PostHarm file
    split_tup = os.path.splitext(output)
    plot_output = split_tup[0] + "_plot" + split_tup[1]
    with open(plot_output, "w") as f:
      colnamelist = postHarmonCombinedData.keys()
      f.write("#,Dataset,Age,Sex,%s\n" % (','.join(colnamelist)))
      for i in range(len(genderList0)):
        # build data list
        postDatalist = []
        for key in colnamelist:
          postDatalist.append(postHarmonCombinedData[key][i])
        
        # skip the records that does not have age info
        try:
          float(extractedDataDir['Age'][i])
        except ValueError:
          continue

        if extractedDataDir['Sex'][i] == '1' or extractedDataDir['Sex'][i] == 'M' or \
          extractedDataDir['Sex'][i] == 'm':
          gender = "Male"
        elif extractedDataDir['Sex'][i] == '2' or extractedDataDir['Sex'][i] == 'F' or \
          extractedDataDir['Sex'][i] == 'f':
          gender = "Female"
        else:
          gender = "Unknown"

        # note: some data elements are number (float), so use map(str, list) to change them to string
        f.write("%d,%s,%s,%s,%s\n" % ((i+1),\
          extractedDataDir['Dataset'][i], str(math.ceil(float(extractedDataDir['Age'][i]))), \
          #extractedDataDir['Dataset'][i], extractedDataDir['Age'][i], \
            gender, ','.join(map(str, postDatalist))))

  print("Harmonization done: total %d records have been processed." % (len(genderList0)))

  return True

