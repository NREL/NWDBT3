#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 10 20:22:27 2023

@author: jhao2
"""

import pandas as pd
import numpy as np
import glob
from auxiliary_torch import listfiles

path2folder = '/Users/jhao2/Desktop/NWDB/WTK_array/All/'
# get the file list for data cleanning 
files2clean, turbine_name = listfiles(path2folder)
# Sort turbine_name and reorder files2clean based on turbine_name's sorting
sorted_pairs = sorted(zip(turbine_name, files2clean))
turbine_name, files2clean = zip(*sorted_pairs)
# turbine_name.sort()

ge_mpc = pd.read_csv('/Users/jhao2/Desktop/NWDB/Data/GE1.5_ManufacurePowerCurve.csv')
ge_mpc = ge_mpc.values
mits_mpc = pd.read_csv('/Users/jhao2/Desktop/NWDB/Data/MITS1.0_ManufacurePowerCurve.csv')
mits_mpc = mits_mpc.values
turbine = []
data_percentile = np.zeros((len(files2clean),8))
# process the data
# for i in range(len(files2clean)):
for i in range(len(turbine_name)):    
    turbine.append(turbine_name[i])
    df = pd.read_excel(files2clean[i])
    df = df.drop(df[df.kw < 0].index) # df = df.drop(df[df.kw < -100].index)
    df = df.drop(df[df.ws < 0].index)
    df = df.drop(df[df.ws > 25].index)
    original_len = len(df)
    # linear interpolation
    if turbine_name[i][0] == "G":
        mpc_ref = np.interp(df['ws'].values, ge_mpc[:,0], ge_mpc[:,1])
        ws_cut_in = 4
        ws_rated = 13.5
        ws_cut_out = 25
    else:
        mpc_ref = np.interp(df['ws'].values, mits_mpc[:,0], mits_mpc[:,1])
        ws_cut_in = 5
        ws_rated = 12.5
        ws_cut_out = 25
    df_lof = pd.read_csv('/Users/jhao2/Desktop/NWDB/WTK_array/localOutlierFactor/'+turbine_name[i]+'.csv')
    df['diff'] = abs(df['kw'].values-mpc_ref)
    df = df.drop(df[df['diff'] > 200].index)
    filter_len = len(df)
    r1 = df[df['ws'] < ws_cut_in]
    r2 = df[(df['ws'] > ws_cut_in) & (df['ws'] < ws_rated)]
    r3 = df[df['ws'] > ws_rated]
    data_percentile[i][0] = filter_len/original_len
    data_percentile[i][1] = len(r1)/filter_len
    data_percentile[i][2] = len(r2)/filter_len
    data_percentile[i][3] = len(r3)/filter_len
    # numbers of LOF results
    r1_lof = df_lof[df_lof['ws'] < ws_cut_in]
    r2_lof = df_lof[(df_lof['ws'] > ws_cut_in) & (df_lof['ws'] < ws_rated)]
    r3_lof = df_lof[df_lof['ws'] > ws_rated]
    data_percentile[i][4] = filter_len/len(df_lof)
    data_percentile[i][5] = len(r1)/len(r1_lof)
    data_percentile[i][6] = len(r2)/len(r2_lof)
    data_percentile[i][7] = len(r3)/len(r3_lof)
    # drop column "diff"
    df.drop('diff', axis=1, inplace=True)
    save2csv = '/Users/jhao2/Desktop/NWDB/WTK_array/mc_clean/'+turbine_name[i]+'.csv'
    df.to_csv(save2csv,index=False)
    print(f'The {i}th file is turbine {turbine_name[i]}')
print('/n/n')
print(turbine)
# data_percentile = np.round(data_percentile, decimals=2)
# turbine = np.array(turbine_name).reshape(-1,1)
# data_df = pd.DataFrame(data=np.concatenate((turbine,data_percentile),axis=1), columns=['Turbine','mpc2original','mpcr1','mpcr2','mpcr3','mpc2lof','lofr1','lofr2','lofr3'])
# data_df.to_csv('/Users/jhao2/Desktop/NWDB/WTK_array/recordings.csv',index=False)
    