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
