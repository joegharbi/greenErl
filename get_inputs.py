import csv

with open("C:\erlab\lab_II\greenErl\green_erlang\measurements\higher_order_functions\\results\\aron_map\\map_list_comprehension_fun_log.csv",newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=';')
    unique_elements = set()
    for row in reader:
        unique_elements.add(int(row[2]))
    sorted_elements = sorted(unique_elements)
    print(sorted_elements)