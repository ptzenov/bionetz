import quandl
import pandas as pd

import numpy as np
import matplotlib.pylab as plt

quandl.ApiConfig.api_key = "cJLyYLEHL1Pbi9v7As34"

datasets = {
            "BTC_USD":"BCHAIN/MKPRU",  # usd2bitcoin price history
            "BTC_TRANS_VOL":"BCHAIN/TRVOU", # transaction volume bitcoin
            "BTC_TRANS_VOL_EST":"BCHAIN/ETRAV", # bitcoin estimated transaction volume
            "BTC_DIFF": "BCHAIN/DIFF", # bitcoin difficulty
            "BTC_BLOCKSIZE":"BCHAIN/AVBLS", # bitcoin average blocksize
            "BTC_UNIQUE_ADDRESS":"BCHAIN/NADDU" # number of unique bitcoin addresses used per day
}

period = {"start":"2014-01-01","end":"2017-09-25"}
data_df = pd.DataFrame()

for key in datasets:
    dummy = quandl.get(datasets[key], start_date = period["start"],   end_date = period["end"], paginate= True)
    dummy.rename(columns= {"Value":key},inplace = True)
    data_df = dummy.join(data_df)

print (data_df.columns)
print ("Data gathering DONE!")

data_df.to_csv("data/BTC_data_{}_{}.csv".format(period['start'],period['end']))


