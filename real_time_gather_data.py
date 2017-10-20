import pandas as pd
import requests
import numpy as np
import matplotlib.pylab as plt
import time as t

import atexit

def nstring(s):
    return s.replace(" ","_")

class MeasurementRecord:

    def  __init__(self,data_record):
        self.raw_data = data_record
        self.type = self.raw_data['type']
        self.geometry = self.raw_data['geometry']
        self.properites = self.raw_data['properties']


        self.id = self.properites['id']
        self.timestamp = self.properites['time']
        self.sensor = self.properites['sensor']

        self.track = self.properites['track']
        self.phenomenons = self.properites['phenomenons']

    def unwrap(self,dictofdicts):

        res = {}
        for key1, val1 in dictofdicts.iteritems():
            if type(val1) == dict:
                for key2,val2 in val1.iteritems():
                    res[nstring(key1+"_"+key2)] = val2
            else:
                res[nstring(key1)] = val1
        return res


    def get_sensor_data(self):
        self.sensor_data = self.unwrap(self.sensor)
    def get_phenomenon_data(self):
        self.phenomenon_data = self.unwrap(self.phenomenons)

    def get_geometry_data(self):
        self.geometry_data = self.unwrap(self.geometry)

    def concat_dicts(self):
        self.all_data = {'id':self.id,  'time': self.timestamp, 'track':self.track}
        self.get_sensor_data()
        self.get_phenomenon_data()
        self.get_geometry_data()

        self.all_data.update(self.phenomenon_data)
        self.all_data.update(self.sensor_data)
        self.all_data.update(self.geometry_data)








# these are the possible requests

api_key = "https://envirocar.org/api/stable/"

_sets =requests.get(api_key)
_sets_urls = pd.Series(_sets.json())

phenomenons = requests.get(api_key + "phenomenons")
schema = requests.get(api_key+"schema")
sensors = requests.get(api_key + "sensors")
statistics = requests.get(api_key + "statistics")
tracks = requests.get(api_key + "trackes")


f = open('measurement_records.csv','a')

for page in range(1,50):
    measurements = requests.get(api_key + "measurements",{"page":page})
    all_records = []
    for raw_record in measurements.json()['features']:
        record = MeasurementRecord(raw_record)
        record.concat_dicts()
        all_records.append(record.all_data)

    record_df = pd.DataFrame(all_records)
    record_df.to_csv(f, index= False, header =  False)
f.close()




