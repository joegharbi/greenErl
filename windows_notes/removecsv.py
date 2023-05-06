import csv
import os

temp_file = 'temp.csv'
f="C:\erlab\lab_II\greenErl\green_erlang\measurements\map_list\\results\youssef_delete\map_delete_remove_e6.csv"
n=1000000
with open(f, 'r') as input_file:
     with open(temp_file, 'w', newline='') as output_file:
        writer = csv.writer(output_file, delimiter=';')
        for row in csv.reader(input_file, delimiter=';'):
            if int(row[2]) <= n:
                writer.writerow(row)

os.remove(f)
os.rename(temp_file, f)