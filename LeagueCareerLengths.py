import requests
from bs4 import BeautifulSoup, Comment
import pandas as pd
import matplotlib.pyplot as plt

def compile_nhl_drafts(start, end):
    all_years = pd.DataFrame()

    for year in range(start, end+1):

        url = 'https://www.hockey-reference.com/draft/NHL_' + str(year) + '_entry.html'
        r = requests.get(url)

        soup = BeautifulSoup(r.content, 'html.parser')

        table = soup.find('table', {'id' : 'stats'})
        thead = table.find('thead')
        headers = [el.text.strip() for el in thead.find_all('th')][7:]
        headers.insert(3, 'player_id')
        headers.append('CareerLength')

        rows = []
        for i, row in enumerate(table.find_all('tr')[2:]):
            row_data = [el.text.strip() for el in row.find_all('td')]
            if len(row_data) < 6:
                continue
            player = row.find('td', {'data-stat' : 'player'})
            player_id = ""
            if player:
                player_id = player['data-append-csv']
            row_data.insert(2, player_id)
            endYear = row_data[6]
            if endYear == '':
                row_data.append(0)
            elif endYear == '2025':
                row_data.append(-1)
            else:
                row_data.append(int(endYear)-year)
            rows.append(row_data)


        df = pd.DataFrame(rows, columns=headers[1:])
        all_years = pd.concat([all_years, df], ignore_index=True)
    return all_years

def compile_nba_drafts(start, end):
    all_years = pd.DataFrame()

    for year in range(start, end+1):

        url = 'https://www.basketball-reference.com/draft/NBA_' + str(year) + '.html'
        r = requests.get(url)

        soup = BeautifulSoup(r.content, 'html.parser')

        table = soup.find('table', {'id' : 'stats'})
        thead = table.find('thead')
        headers = [el.text.strip() for el in thead.find_all('th')]
        headers = headers[9:]

        rows = []
        for i, row in enumerate(table.find_all('tr')[2:]):
            row_data = [el.text.strip() for el in row.find_all('td')]
            rows.append(row_data)


        df = pd.DataFrame(rows, columns=headers[1:])
        all_years = pd.concat([all_years, df], ignore_index=True)
    return all_years

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

nfl = compile_nfl_drafts(2005, 2015)
nfl_filt = nfl[nfl['CareerLength'] > 0]
nfl_data = nfl_filt['CareerLength']
nfl_data['League'] = 'NFL'

nhl = compile_nhl_drafts(2005, 2015)
nhl = nhl[nhl['CareerLength'] > 0]
nhl_data = nhl['CareerLength']
nhl_data['League'] = 'NHL'

nba = compile_nba_drafts(2005, 2015)
nba = nba[(nba['Yrs'] != "") & (nba['Yrs']) & (nba['Yrs'] != "0")]
nba_data = nba['Yrs']
nba_data.rename(columns={'Yrs': 'CareerLength'}, inplace=True)
nba_data['League'] = 'NBA'
combined_data = pd.concat([nfl_data, nhl_data, nba_data], ignore_index=True)

fig, ax = plt.subplots(figsize=(12, 8))

sns.violinplot(
    data=combined_data,
    x='League',
    y='CareerLength',
    inner='quartile',
    palette='coolwarm',
    ax=ax
)

ax.set_title('Career Length Distribution by League (2005-2015)', fontsize=18)
ax.set_xlabel('League', fontsize=14)
ax.set_ylabel('Career Length (years)', fontsize=14)
ax.tick_params(axis='x', labelsize=12)
ax.tick_params(axis='y', labelsize=12)
ax.grid(axis='y', linestyle='--', alpha=0.7)

ax.set_ylim(0, None)
plt.tight_layout()
plt.show()
