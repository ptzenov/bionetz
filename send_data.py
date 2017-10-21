import requests
import pandas as pd


data = pd.read_csv('dataset.csv')
cols = data.columns
url = "https://rem-analytics.eu-de.mybluemix.net/getdata"

for idx in data.index[0:10]:
    payload = {c: data.ix[idx,c] for c in data.columns}
    # payload_json = [("'"+str(c) + "':'" + str(data.ix[idx,c])+"'") for c in data.columns ]
    # payload_str = '{'
    # for s in payload_json:
    #     payload_str += s+','
    # print payload_str[0:-1] + '}'

    # print payload
    res = requests.post(url, data=payload)


comms = ["A",
         "b",
         "c"]







