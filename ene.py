import pandas as pd
import os

file_path = 'C:/PhD/greenErl/measurements/higher_order_functions/results/youssef_filter/filter_filter_named.csv'
# Assuming the data is in a CSV file named 'filter_filter_fun.csv'
data = pd.read_csv(file_path, sep=';', header=None, names=['col1', 'col2', 'col3', 'col4', 'col5', 'col6'])

# Create a dictionary to map 'col3' values to 'col6' values for 'time' rows
time_dict = data[data['col4'] == 'time'].set_index('col3')['col6'].to_dict()

# Define a function to apply to 'msr' rows
def multiply_msr(row):
    if row['col4'] == 'msr' and row['col3'] in time_dict:
        return row['col6'] * time_dict[row['col3']]
    else:
        return row['col6']

# Apply the function to 'msr' rows
data['col6'] = data.apply(multiply_msr, axis=1)

file_name = os.path.basename(file_path)
# Write the modified data to a new CSV file
data.to_csv(file_name, sep=';', header=False, index=False)
