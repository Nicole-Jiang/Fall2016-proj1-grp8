# Environment
import numpy as np
import pandas as pd
import os
os.chdir('/Users/pengfeiwang/Desktop/Fall2016-proj1-grp8/data/')

# Function
def Divorce_by_state(part_a, part_b, output_path):
    a = pd.read_csv(part_a,usecols=['MARHD','MARHM','ST'])
    b = pd.read_csv(part_b,usecols=['MARHD','MARHM','ST'])
    state = pd.read_csv('/Users/pengfeiwang/Desktop/Fall2016-proj1-grp8/data/statenames.csv', usecols=['code','abbr'])
    df = pd.concat([a,b])
    df_na = df.dropna(subset=['MARHM','MARHD','ST'])
    df = pd.merge(df_na,state,left_on='ST',right_on='code')
    df = df.drop(['code','ST'],1)
    df['MARHD'] = df['MARHD'].map({2: 0, 1: 1})
    df['MARHM'] = df['MARHM'].map({2: 0, 1: 1})
    grouped = df.groupby('abbr')
    MARHM_state = grouped.MARHM.sum().to_frame()
    MARHD_state = grouped.MARHD.sum().to_frame()
    by_state = pd.merge(MARHM_state, MARHD_state, left_index=True, right_index=True)
    by_state['Divorce_ratio'] = by_state['MARHD'] / by_state['MARHM']
    by_state = by_state.drop(['MARHD','MARHM'],1)
    output = int(filter(str.isdigit, part_a))
    by_state.to_csv(output_path + str(output) + '.csv')


# test
part_a = 'ss13pusa.csv'
part_b = 'ss13pusb.csv'
output_path = '/Users/pengfeiwang/Desktop/'
Divorce_by_state(part_a, part_b, output_path = '/Users/pengfeiwang/Desktop/')