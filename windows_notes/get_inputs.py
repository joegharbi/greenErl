import csv

with open("C:\erlab\lab_II\greenErl\green_erlang\measurements\skeletons\\task_farm\\fibonacci\\30\\results\\aron_big\\fibonacci_parmap_log.csv",newline='') as csvfile:
# with open("C:\erlab\lab_II\greenErl\green_erlang\measurements\skeletons\\task_farm\identity\\results\\aron_laptop\\farm_worker_named_function_log.csv",newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=';')
    unique_elements = set()
    for row in reader:
        unique_elements.add(int(row[2]))
    sorted_elements = sorted(unique_elements)
    print(sorted_elements)
    print(len(sorted_elements))