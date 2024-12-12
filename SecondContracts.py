import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import Counter

file_path = 'OTC_Contracts.csv'
df = pd.read_csv(file_path)

nfl_salary_cap = {
    2011: 120375000,
    2012: 120600000,
    2013: 123000000,
    2014: 133000000,
    2015: 143280000,
    2016: 155270000,
    2017: 167000000,
    2018: 177200000,
    2019: 188200000,
    2020: 198200000,
    2021: 182500000, 
    2022: 208200000,
    2023: 224800000,
    2024: 255400000
}

non_rookie = ['Extension', 'UFA', 'Other', 'SFA', 'Veteran', 'Franchise', 'RFA', 'ERFA', 'Transition']

df = df[df['start_year'] > 2010]
df = df[df['start_year'] < 2025]
df = df[df['Years'].notna()]
df = df[df['APY'].notna()]

def calc_cap_perc(start_year, length, APY):
    total = 0
    for year in range(start_year, start_year+length):
        if year > 2024:
            length = length -1 
            continue
        total += nfl_salary_cap[year]
    if length == 0:
        return 0
    av = total / length
    
    return APY / av

df['start_year'] = df['start_year'].astype(int)
df['Years'] = df['Years'].astype(int)
df['APY'] = df['APY'].astype(float)

df['APY_cp'] = df.apply(
    lambda row: calc_cap_perc(row['start_year'], row['Years'], row['APY']),
    axis=1
)

non_rookies = df[df['contract_type'].isin(['Franchise', 'UFA', 'Extension', 'RFA', 'Transition'])]
rookies = df[df['contract_type'] == 'Drafted']

non_rookies_percentiles = non_rookies['APY_cp'].describe(percentiles=[0.25, 0.5, 0.75])[['25%', '50%', '75%']]

# Calculate percentiles for `APY_cp` in rookies
rookies_percentiles = rookies['APY_cp'].describe(percentiles=[0.25, 0.5, 0.75])[['25%', '50%', '75%']]

# Combine into a single DataFrame for comparison
percentiles_df = pd.DataFrame({
    "Non-Rookies": non_rookies_percentiles,
    "Rookies": rookies_percentiles
}).T
percentiles_df.columns = ['25th Percentile', 'Median (50th)', '75th Percentile']

bin_edges = np.linspace(0, 0.1, 20)  
plt.hist(non_rookies['APY_cp'], bins=bin_edges, alpha=0.5, label='Non-Rookies', color='blue', density=True)
plt.hist(rookies['APY_cp'], bins=bin_edges, alpha=0.5, label='Rookies', color='orange', density=True)
plt.xlabel('APY Cap %')
plt.ylabel('Frequency')
plt.title('Distribution of APY Cap % for Non-Rookies and Rookies')
plt.legend()
plt.xlim(0, 0.1)
plt.show()
