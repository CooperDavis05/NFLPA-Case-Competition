def generate_team_salary_chart(team, year):
    url = 'https://www.spotrac.com/nfl/' + team + '/cap/_/year/' + str(year) + '/sort/cap_total'
    r = requests.get(url)

    soup = BeautifulSoup(r.content, 'html.parser')

    table = soup.find('table', {'id' : 'table_active'})
    
    thead = table.find('thead')
    headers = [el.text.strip() for el in thead.find_all('th')]
    headers = [s.replace('\n', ' ') for s in headers]
    headers[0] = "Player"

    rows = []
    for i, row in enumerate(table.find_all('tr')[1:]):
        row_data = [el.text.strip() for el in row.find_all('td')]
        row_data[0] = row_data[0].split('\n', 1)[-1]
        rows.append(row_data)

    df = pd.DataFrame(rows, columns=headers)
    df = df.applymap(lambda x: x.replace('\n', ' ') if isinstance(x, str) else x)
    df = df[df.iloc[:, 0].notna()]

    return df

teams = [
    "Arizona-Cardinals",
    "Atlanta-Falcons",
    "Baltimore-Ravens",
    "Buffalo-Bills",
    "Carolina-Panthers",
    "Chicago-Bears",
    "Cincinnati-Bengals",
    "Cleveland-Browns",
    "Dallas-Cowboys",
    "Denver-Broncos",
    "Detroit-Lions",
    "Green-Bay-Packers",
    "Houston-Texans",
    "Indianapolis-Colts",
    "Jacksonville-Jaguars",
    "Kansas-City-Chiefs",
    "Las-Vegas-Raiders",
    "Los-Angeles-Chargers",
    "Los-Angeles-Rams",
    "Miami-Dolphins",
    "Minnesota-Vikings",
    "New-England-Patriots",
    "New-Orleans-Saints",
    "New-York-Giants",
    "New-York-Jets",
    "Philadelphia-Eagles",
    "Pittsburgh-Steelers",
    "San-Francisco-49ers",
    "Seattle-Seahawks",
    "Tampa-Bay-Buccaneers",
    "Tennessee-Titans",
    "Washington-Commanders"
]

nfl_salaries_final = []
for year in range(2011,2025):
    team_dfs = []
    for team in teams:
        team_df = generate_team_salary_chart(team, year)
        team_dfs.append(team_df)
    year_df = pd.concat(team_dfs, axis=0,ignore_index=True)
    year_df['Cap Hit'] = year_df['Cap Hit'].replace({'\$': '', ',': '', '-': '0'}, regex=True).astype(float)
    year_df['Base P5 Salary'] = year_df['Base P5 Salary'].replace({'\$': '', ',': '', '-': '0'}, regex=True).astype(float)
    year_df = year_df.sort_values(by='Base P5 Salary')
    nfl_salaries_final.append(year_df)

headers = nfl_salaries_final[0].columns
for i, df in enumerate(nfl_salaries_final):
    year = 2014 + i
    file_name = f'dataframe_{year}.csv'
    df.to_csv(file_name, index=False, header=headers)
