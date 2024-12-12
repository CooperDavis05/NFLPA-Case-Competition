import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import math
from collections import Counter

# importing datafiles
directory = "."
data = {}
for filename in os.listdir(directory):
    f = os.path.join(directory, filename)
    # checking if it is a file
    if os.path.isfile(f) and f.endswith(".csv"):
        year = int(f[-8:-4])
        data[year] = pd.read_csv(f)

for year in range(2014, 2025):
    data[year]["Player"] = data[year]["Player"].apply(lambda x: ' '.join(x.split()[1:]))

# generating histogram of a year's cap hit
def gen_histogram(year):
  plt.hist(data[year]["Cap Hit"], bins=100)

# generating histogram (log scale) of cap hit
def gen_log_histogram(year):
  plt.hist(data[year]["Cap Hit"], bins=100, log=True)
  plt.xlabel("Value (log scale)")
  plt.ylabel("Frequency")
  plt.title("Histogram with Logarithmic Scale")
  plt.show()

# plotting 25th, 50th, 75th percentile
def histogram_with_markers(year):
  data_column = data[year]["Cap Hit"]
  # Calculate stats
  median = np.median(data_column)
  percentile_25 = np.percentile(data_column, 25)
  percentile_75 = np.percentile(data_column, 75)
  
  # Plot histogram
  plt.hist(data_column, bins=100, alpha=0.7)
  plt.xlabel("Value")
  plt.ylabel("Frequency")
  plt.title("2024 Histogram with Median and 25th/75th Percentiles")
  
  # Add vertical lines for the median and percentiles
  plt.axvline(median, color='red', linestyle='--', linewidth=1.5, label=f'Median: {median:.2f}')
  plt.axvline(percentile_25, color='green', linestyle='--', linewidth=1.5, label=f'25th Percentile: {percentile_25:.2f}')
  plt.axvline(percentile_75, color='blue', linestyle='--', linewidth=1.5, label=f'75th Percentile: {percentile_75:.2f}')
  
  plt.legend()
  plt.show()

def log_histogram_with_markers(year):
  data_column = data[year]["Cap Hit"]
  # Calculate stats
  median = np.median(data_column)
  percentile_25 = np.percentile(data_column, 25)
  percentile_75 = np.percentile(data_column, 75)
  
  # Plot histogram
  plt.hist(data_column, bins=100, alpha=0.7, log=True)
  plt.xlabel("Value")
  plt.ylabel("Frequency (Log Scale)")
  plt.title("2024 Histogram with Median and 25th/75th Percentiles")
  
  # Add vertical lines for the median and percentiles
  plt.axvline(median, color='red', linestyle='--', linewidth=1.5, label=f'Median: {median:.2f}')
  plt.axvline(percentile_25, color='green', linestyle='--', linewidth=1.5, label=f'25th Percentile: {percentile_25:.2f}')
  plt.axvline(percentile_75, color='blue', linestyle='--', linewidth=1.5, label=f'75th Percentile: {percentile_75:.2f}')
  
  plt.legend()
  plt.show()

def histogram_by_pos(year):
  positions = data[2024]["Pos"].unique()
  positions = [pos for pos in positions if pos not in ['P', 'K', 'LS', 'SS', 'FS', 'FB', 'T', 'LB', 'OL']]
  
  for pos in positions:
      pos_data = data[year][data[year]["Pos"] == pos]
      
      plt.figure()
      plt.hist(pos_data["Cap Hit"], bins=40, alpha=0.7)
      plt.xlabel("Cap Hit") 
      plt.ylabel("Frequency")
      plt.title(f"Histogram for Position: {pos}")
      
      median = np.median(pos_data["Cap Hit"])
      percentile_25 = np.percentile(pos_data["Cap Hit"], 25)
      percentile_75 = np.percentile(pos_data["Cap Hit"], 75)
      plt.axvline(median, color='red', linestyle='--', linewidth=1.5, label=f'Median: {median:.2f}')
      plt.axvline(percentile_25, color='green', linestyle='--', linewidth=1.5, label=f'25th Percentile: {percentile_25:.2f}')
      plt.axvline(percentile_75, color='blue', linestyle='--', linewidth=1.5, label=f'75th Percentile: {percentile_75:.2f}')
      plt.legend()
      plt.show()

def gen_chart_mean_salary_by_percentile():
    top_25 = []
    mid_50 = []
    bot_25 = []
    
    for year in range(2014,2025):
        curr_data = data[year]
        num_players = len(curr_data)
        q_size = math.floor(num_players/4)
        start = num_players - 4*q_size
        bot_25.append(curr_data[start : start+q_size]['Base P5 Salary'].mean())
        mid_50.append(curr_data[start+q_size : start+3*q_size]['Base P5 Salary'].mean())
        top_25.append(curr_data[start+3*q_size : ]['Base P5 Salary'].mean())
    X = range(2014, 2025)
    plt.plot(X, top_25, label='Top 25%')
    plt.plot(X, mid_50, label='Middle 50%')
    plt.plot(X, bot_25, label='Bottom 25%')
    plt.title("Means of Bottom 25%, Middle 50%, and Top 25% Salaries")

def graph_salary_groups(n, domain=[0,1]):
    segments = []
    for _ in range(n):
        segments.append([])

    for year in range(2014,2025):
        year_data = data[year]
        total_players = len(year_data)
        curr_data = data[year][math.floor(total_players*domain[0]) : math.floor(total_players*domain[1])]
        num_players = len(curr_data)
        
        g_size = math.floor(num_players/n)
        print('-----')
        start = num_players - n*g_size
        for i in range(n):
            segments[i].append(curr_data[start+i*g_size : start+(i+1)*g_size]['Base P5 Salary'].mean())
            print(min(curr_data[start+i*g_size : start+(i+1)*g_size]['Base P5 Salary']), max(curr_data[start+i*g_size : start+(i+1)*g_size]['Base P5 Salary']))

    X = range(2014, 2025)
    fig, ax = plt.subplots(figsize=(10,6))
    for sub_data in segments:
        ax.plot(X, sub_data)
    
    ax.set_title(f"NFL Base Salaries Divided into {n} Equal Tiers By Base Salary (Median of groups)")
    if domain != [0,1]:
        ax.set_title(f"NFL Base Salaries Divided into {n} Equal Tiers By Base Salary ({domain[0]}% to {domain[1]*100}%)")

def calculate_pct_change(data):
    return np.diff(data) / data[:-1] * 100

def graph_salary_groups_change(n, domain=[0,1]):

    segments = []
    for _ in range(n):
        segments.append([])

    for year in range(2014,2025):
        
        year_data = data[year]
        total_players = len(year_data)
        curr_data = data[year][math.floor(total_players*domain[0]) : math.floor(total_players*domain[1])]
        num_players = len(curr_data)
        
        g_size = math.floor(num_players/n)
        
        start = num_players - n*g_size
        for i in range(n):
            segments[i].append(curr_data[start+i*g_size : start+(i+1)*g_size]['Base P5 Salary'].median())

    X = range(2014, 2025)
    
    fig, ax = plt.subplots(figsize=(10,6))
    for i, sub_data in enumerate(segments):
        ax.plot(X[1:], calculate_pct_change(sub_data), label=str(i))
    fig.legend()
    ax.set_title(f"NFL Base Salaries Divided into {n} Equal Tiers By Base Salary (Median of groups)")
    if domain != [0,1]:
        ax.set_title(f"NFL Base Salaries Divided into {n} Equal Tiers By Base Salary ({domain[0]}% to {domain[1]*100}%)")

def analyze_rb_salaries():
    def calculate_metrics(data):
        age_group = data.groupby('Age')['Base P5 Salary']
        mean_salary = age_group.mean()
        median_salary = age_group.median()
        top_25_median = age_group.apply(lambda x: x.quantile(0.75))
        bot_25_median = age_group.apply(lambda x: x.quantile(0.25))
        return mean_salary, median_salary, top_25_median, bot_25_median
    
    for year in [2014, 2024]:
        data[year]['Age'] = pd.to_numeric(data[year]['Age'], errors='coerce')
        data[year]['Base P5 Salary'] = pd.to_numeric(data[year]['Base P5 Salary'], errors='coerce')
    
    rb_2014 = data[2014][data[2014]['Pos'] == 'RB'].dropna(subset=['Age', 'Base P5 Salary'])
    rb_2024 = data[2024][data[2024]['Pos'] == 'RB'].dropna(subset=['Age', 'Base P5 Salary'])
    
    mean_2014, median_2014, top25_2014, bot25_2014 = calculate_metrics(rb_2014)
    mean_2024, median_2024, top25_2024, bot25_2024 = calculate_metrics(rb_2024)
    
    plt.figure(figsize=(10, 6))
    plt.plot(mean_2014, label='2014 Mean Salary', color='blue', marker='o')
    plt.plot(mean_2024, label='2024 Mean Salary', color='orange', marker='o')
    plt.title('Mean Base P5 Salary by Age (RBs)')
    plt.xlabel('Age')
    plt.ylabel('Salary ($)')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.show()
    
    plt.figure(figsize=(10, 6))
    plt.plot(median_2014, label='2014 Median Salary', color='blue', marker='o')
    plt.plot(median_2024, label='2024 Median Salary', color='orange', marker='o')
    plt.title('Median Base P5 Salary by Age (RBs)')
    plt.xlabel('Age')
    plt.ylabel('Salary ($)')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.show()
    
    plt.figure(figsize=(10, 6))
    plt.plot(top25_2014, label='2014 Top 25% Median', color='blue', marker='o')
    plt.plot(top25_2024, label='2024 Top 25% Median', color='orange', marker='o')
    plt.plot(bot25_2014, label='2014 Bottom 25% Median', color='green', marker='o')
    plt.plot(bot25_2024, label='2024 Bottom 25% Median', color='red', marker='o')
    plt.title('Percentile-based Medians of Base P5 Salary by Age (RBs)')
    plt.xlabel('Age')
    plt.ylabel('Salary ($)')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.show()
