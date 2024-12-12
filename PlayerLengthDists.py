import requests
from bs4 import BeautifulSoup, Comment
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def compile_nfl_drafts(start, end):
    all_years = pd.DataFrame()

    for year in range(start, end+1):
        url = 'https://www.pro-football-reference.com/years/'+ str(year) + '/draft.htm'
        r = requests.get(url)

        soup = BeautifulSoup(r.content, 'html.parser')

        table = soup.find('table', {'id' : 'drafts'})

        thead = table.find('thead')
        headers = [el.text.strip() for el in thead.find_all('th')]
        headers = headers[13:-1]
        headers.insert(0, "Year")
        headers.insert(1, "Round")
        headers.insert(5, "PlayerID")


        rows = []
        for i, row in enumerate(table.find_all('tr')[2:]):
            row_data = [el.text.strip() for el in row.find_all('td')]
            if len(row_data) != 28:
                continue
            draft_round = row.find('th', {'data-stat' : 'draft_round'}).text
            td_with_data_append_csv = row.select('td[data-append-csv]')
            try:
                player_id = td_with_data_append_csv[0]['data-append-csv']
            except:
                continue

            row_data.insert(0, year)
            row_data.insert(1, draft_round)
            row_data.insert(5, player_id)
            rows.append(row_data[:-1])


        df = pd.DataFrame(rows, columns=headers)
        all_years = pd.concat([all_years, df], ignore_index=True)
    return all_years

df = compile_nfl_drafts(2005, 2020)
df['CareerLength'] = df.apply(lambda row: 0 if row['To'] == '' else (-1 if int(row['To']) == 2024 else int(row['To']) - row['Year']), axis=1)

def gen_chart_by_pos():
  positions = ['QB', 'RB', 'WR', 'DB', 'LB', 'T', 'DT', 'DE', 'C', 'TE', 'G', 'K',
             'P', 'FB', 'NT', 'OL', 'DL', 'OLB', 'CB', 'S', 'ILB', 'LS']

  for pos in positions:
      pos_dist = df[(df['Pos'] == pos) & (df['CareerLength'] != -1)]
      plt.figure()  # Create a new figure for each position
      plt.hist(pos_dist['CareerLength'], bins=range(0, 20), density=True, color='skyblue', edgecolor='black')
      plt.title(f'Career Length Distribution for {pos}')
      plt.xlabel('Career Length (years)')
      plt.ylabel('Frequency')
      plt.show()

def gen_boxplots(cutoff):
  # Filter the data to exclude some career lengths
  filtered_df = df[(df['CareerLength'] > cutoff) & (df['Pos'].isin(pos_prime))]
  
  # Set up the figure size
  plt.figure(figsize=(15, 10))
  
  # Create the boxplot
  sns.boxplot(data=filtered_df, x='Pos', y='CareerLength', order=pos_prime)
  plt.title('Career Length Distribution by Position')
  plt.xlabel('Position')
  plt.ylabel('Career Length (years)')
  plt.xticks(rotation=45)  # Rotate x labels for readability
  
  plt.show()

def gen_violin_plots():
  fig, ax = plt.subplots(figsize=(18, 10))  # Adjust aspect ratio
  
  pos_prime = ['QB', 'RB', 'WR', 'DB', 'LB', 'T', 'DT', 'DE', 'C', 'TE', 'G',
               'OLB', 'CB', 'S', 'ILB']
  
  filtered_df = df[df['Pos'].isin(pos_prime) & df['CareerLength'] > -1]
  
  median_sorted = (
      filtered_df.groupby('Pos')['CareerLength']
      .median()
      .sort_values(ascending=False)
      .index
  )
  
  sns.violinplot(
      data=filtered_df,
      x='Pos',
      y='CareerLength',
      order=median_sorted,
      inner='quartile',
      palette='coolwarm',  # Better contrast palette
      ax=ax
  )
  
  ax.set_title('Career Length Distribution by Position (2014-2024)', fontsize=18)
  ax.set_xlabel('Position', fontsize=14)
  ax.set_ylabel('Career Length (years)', fontsize=14)
  ax.tick_params(axis='x', labelsize=12, rotation=45)  # Rotate and size x labels
  ax.tick_params(axis='y', labelsize=12)
  ax.grid(axis='y', linestyle='--', alpha=0.7)  # Add gridlines for readability
  
  ax.set_ylim(0, None)
  
  plt.tight_layout() 
  plt.show()
