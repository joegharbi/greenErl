import json
import matplotlib.pyplot as plt
import subprocess

# "scaphandre json -t 10 -s 0 -n 100000000 -f report.json"
# def start():
#     subprocess.run(['scaphandre json -t 10 -s 0 -n 100000000 -f report.json'],shell= True)
#     subprocess.run(['energy_consumption:measure({filter_map, [recursive], [10,30]},10000).'], shell=True)

# Load the data from the JSON object
with open("C:\erlab\lab_II\greenErl\green_erlang\measurements\\test_new_farm\M_map_F_recursive_I_20_C_1000.json", 'r') as f:
    data = json.load(f)

# Extract the timestamps and erl.exe consumption values
total_val = 0
total_num = 0
for snapshot in data:
    for consumer in snapshot['consumers']:
        if consumer['exe'] == 'C:\\Program Files\\erl-23.3.4.11\\bin\\erl.exe':
            total_val+=consumer['consumption']
            total_num+=1
            # timestamps.append(consumer['timestamp'])
            # erl_consumption.append(consumer['consumption']/1000000)
            # erl_consumption.append(consumer['consumption'])
res_avg=total_val/total_num
print (res_avg/1000000)
# Convert the timestamps to seconds since the first snapshot
# base_time = timestamps[0]
# timestamps = [(t - base_time) for t in timestamps]



# # Create the plot
# plt.plot(timestamps, erl_consumption)
# plt.xlabel('Time (s)')
# plt.ylabel('erl.exe consumption')
# plt.title('erl.exe consumption over time')
# plt.show()
