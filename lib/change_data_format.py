import pandas as pd
import os

working_dir = '/Users/pengfeiwang/Desktop/Fall2016-proj1-grp8/output/'
os.chdir(working_dir)
documents = os.listdir(working_dir)

total = pd.DataFrame(columns = ['abbr', 'Divorce_ratio', 'year'])
for document in documents:
    if os.path.splitext(document)[1] == '.csv':
        df = pd.read_csv(document)
        total = pd.concat([total, df])

total = total[['year','abbr','Divorce_ratio']]
total.columns = ['Year','State','Divorce_Ratio']
total = total.sort_values(by = ['State','Year'], ascending=[True, True])
# total.to_csv('Cleaned_data.csv', index = False)