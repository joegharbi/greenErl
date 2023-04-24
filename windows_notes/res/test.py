import json

file = "C:\\Users\\joegh\\report.json"

with open(file, 'r') as f:
    if f.readable() and f.seek(0, 2) > 0:
        f.seek(0)
        data = json.load(f)
        total_val = 0
        total_num = 0
        for snapshot in data:
            for consumer in snapshot['consumers']:
                if consumer['exe'] == 'C:\\Program Files\\erl-23.3.4.11\\bin\\erl.exe':
                    if consumer['pid'] == 14960:
                        total_val+=consumer['consumption']
                        total_num+=1
print(total_val)
print(total_num)