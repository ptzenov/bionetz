import obd
import time

connection = obd.OBD("/dev/rfcomm1")

# # a callback that prints every new value to the console
# comms = [
#         obd.commands.FUEL_PRESSURE          ,
#         obd.commands.INTAKE_PRESSURE        ,
#         obd.commands.RPM                    ,
#         obd.commands.SPEED                  ,
#         obd.commands.INTAKE_TEMP            ,
#         obd.commands.MAF                    ,
#         obd.commands.THROTTLE_POS           ,
#         obd.commands.RUN_TIME               ,
#         obd.commands.DISTANCE_W_MIL         ,
#         obd.commands.FUEL_RAIL_PRESSURE_VAC ,
#         obd.commands.EGR_ERROR              ,
#         obd.commands.FUEL_LEVEL             ,
#         obd.commands.BAROMETRIC_PRESSURE    ,
#         obd.commands.ABSOLUTE_LOAD          ,
#         obd.commands.RELATIVE_THROTTLE_POS  ,
#         obd.commands.ACCELERATOR_POS_D      ,
#         obd.commands.ACCELERATOR_POS_E      ,
#         obd.commands.ACCELERATOR_POS_F      ,
#         obd.commands.RUN_TIME_MIL           ,
#         obd.commands.ETHANOL_PERCENT        ,
#         obd.commands.OIL_TEMP               ,
#         obd.commands.FUEL_INJECT_TIMING     ,
#         obd.commands.FUEL_RATE              ,
#         obd.commands.AMBIANT_AIR_TEMP]


t = 0
fc = open("Record_1.csv", "a")

import pandas as pd


cmd_names = "FUEL_PRESSURE, INTAKE_PRESSURE, RPM, SPEED, INTAKE_TEMP, MAF, THROTTLE_POS, RUN_TIME, DISTANCE_W_MIL, FUEL_RAIL_PRESSURE_VAC, EGR_ERROR, FUEL_LEVEL, BAROMETRIC_PRESSURE, ABSOLUTE_LOAD, RELATIVE_THROTTLE_POS, ACCELERATOR_POS_D, ACCELERATOR_POS_E, ACCELERATOR_POS_F, RUN_TIME_MIL, ETHANOL_PERCENT, OIL_TEMP, FUEL_INJECT_TIMING, FUEL_RATE, AMBIANT_AIR_TEMP"
cmd_names = cmd_names.split(",")
cmd_names = [c.strip() for c in cmd_names]

list_dic = []
time_start = time.time()
start_0 = time_start

while True:
    dic = {}
    dic['time'] = time.time()
    for cmd in cmd_names:
        response = connection.query(obd.commands[cmd])
        dic[cmd] = response.value.magnitude
    list_dic.append(dic)
    if time.time()-time_start >= 10*60:
        df = pd.DataFrame(list_dic)
        df.to_csv('record_01.csv', index=False, header=False)
        break
