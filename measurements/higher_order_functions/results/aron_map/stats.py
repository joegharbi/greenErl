import csv
import sys
import statistics

filenames = ["map_" + suf + "_result.csv" for suf in ["recursive", "list_comprehension", "list_comprehension_fun", "list_comprehension_named", "map_named", "map_fun", "my_map_named", "my_map_fun"]]
methods = ["sysfs", "time"]
domains = ["energy-pkg", "time"]


results = {}

for filename in filenames:
	with open(filename) as csv_file:
		csv_reader = csv.reader(csv_file, delimiter=';')
		line_count = 0
		for row in csv_reader:
			mod = row[0]
			fun = row[1]
			inp = int(row[2])
			method = row[3]
			domain = row[4]
			value = float(row[5])
			if (mod, fun, method, domain, inp) in results:
				results[(mod, fun, method, domain, inp)].append(value)
			else:
				results[(mod, fun, method, domain, inp)] = [value]

timeEnergy = {}
powerResults = [["Module", "Function", "TotalEnergy", "TotalTime", "Power"]]
finalResults = [["Module", "Function", "Mehtod", "Domain", "Input", "Average", "StandDev", "Min", "Max", "Median", "Range"]]
for (mod, fun, method, domain, inp),  values in results.items():
	values.sort()
	# values = values[1:-1]
	if (method in methods or methods == []) and (domain in domains or domains == []):
		values.sort()
		avg = round(statistics.mean(values),4)
		sd = round(statistics.stdev(values),4)
		mini = round(min(values),4)
		maxi = round(max(values),4)
		medi = round(statistics.median(values),4)
		ran  = round(maxi - mini,4)
		finalResults.append([mod, fun, method, domain, inp, avg, sd, mini, maxi, medi, ran])
	
	if (method == "sysfs") and (domain == "energy-pkg"):
		if (mod, fun) in timeEnergy:
			timeEnergy[(mod, fun)][0] += sum(values)
		else:
			timeEnergy[(mod, fun)] = [sum(values), 0]
	if (method == "time") and (domain == "time"):
		if (mod, fun) in timeEnergy:
			timeEnergy[(mod, fun)][1] += sum(values)
		else:
			timeEnergy[(mod, fun)] = [0, sum(values)]

finalResults.sort()

for (mod, fun), [energy, time] in timeEnergy.items():
	power = round(energy / time, 4)
	powerResults.append([mod, fun, round(energy, 4), round(time, 4), power])

powerResults.sort()

with open('statistics.csv', mode='w') as resultFile:
	csv_writer = csv.writer(resultFile, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	for row in finalResults:
		csv_writer.writerow(row)

with open('powers.csv', mode='w') as resultFile:
	csv_writer = csv.writer(resultFile, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	for row in powerResults:
		csv_writer.writerow(row)

print(results)
