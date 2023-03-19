import os
import re
import csv
import json
import matplotlib.pyplot as plt

folder_path = 'C:\\erlab\\lab_II\\greenErl\\green_erlang\\measurements\\higher_order_functions\\results\\res_p12f'
for filename in os.listdir(folder_path):
    if filename.endswith('.csv'):
        module_function = filename.split('.')[0]
        module, function = module_function.split('_', 1)
        with open(os.path.join(folder_path, filename), 'a') as f:
            writer = csv.writer(f)
            for json_file in os.listdir(folder_path):
                if json_file.endswith('.json') and json_file.startswith(module_function):
                    # input_name = json_file.split('.')[0].split('_')[-1]
                    with open(os.path.join(folder_path, json_file), 'r') as f:
                        if f.readable() and f.read(1):
                            f.seek(0)
                            data = json.load(f)
                            total_val = 0
                            total_num = 0
                            for snapshot in data:
                                for consumer in snapshot['consumers']:
                                    if consumer['exe'] == 'C:\\Program Files\\erl-23.3.4.11\\bin\\erl.exe':
                                        total_val+=consumer['consumption']
                                        total_num+=1
                            res_avg=total_val/total_num/(1000000)
                            input_name = json_file.rsplit('_', 1)[1].split('.')[0]
                            # writer.writerow([module, function,input_name,'energy-cores',res_avg])
                            row = [module, function,input_name,'energy-cores',res_avg]
                            row_str = ';'.join(map(str, row))
                            with open(os.path.join(folder_path, filename), 'a') as f:
                                f.write(row_str + '\n')
                        else:
                            print(f"{json_file} is empty")