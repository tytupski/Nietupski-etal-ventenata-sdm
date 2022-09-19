#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author: Ty Nietupski

Code in support of the manuscript titled "An
  exploration of current and future
  niche-based invasion projections and
  uncertainty for an invasive grass in the
  western US"

Objective: helper to call r script to calculate spatial variable importance for
  each gcm separately.
"""

from subprocess import call
import os

main_dir = # path to the directory with all gcm directories
gcms = ['access1-0','bcc-csm1-1','bcc-csm1-1-m','canesm2','ccsm4',
        'cesm1-bgc','cesm1-cam5','cmcc-cm','cnrm-cm5','csiro-mk3-6-0',
        'fgoals-g2','fio-esm','gfdl-cm3','gfdl-esm2g','gfdl-esm2m',
        'giss-e2-r','hadgem2-ao','hadgem2-cc','hadgem2-es','inmcm4',
        'ipsl-cm5a-lr','ipsl-cm5a-mr','ipsl-cm5b-lr','miroc-esm',
        'miroc-esm-chem','miroc5','mpi-esm-lr','mpi-esm-mr',
        'mri-cgcm3','noresm1-m']

r_script = # path to the r script 10-ShaplyValues_Future.R

# loop through each time period for each gcm to predict vedu prob
for gcm in gcms:

    output_files = [
        os.path.join(main_dir,
                     gcm,
                     'spatial_importance',
                     f'Shap_values_{i}_2069_2099.tif')
        for i in range(1, 11)
        ]
    if all([os.path.exists(file) for file in output_files]):
        continue

    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print(f'Starting:    {gcm}')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')


    # calculate individual predictions from each model
    call(['Rscript',
          r_script,
          gcm])

    # print status message for each complete gcm
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print(f'Processing complete:    {gcm}')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')

