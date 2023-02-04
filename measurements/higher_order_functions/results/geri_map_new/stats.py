import csv
import sys
import statistics

filenames = ["map_" + suf + "_result.csv" for suf in ["list_comprehension"]]
methods = ["sysfs"]
domains = ["energy-pkg"]


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


finalResults = [["Module", "Function", "Mehtod", "Domain", "Input", "Average", "StandDev", "Min", "Max", "Median", "Range"]]
for (mod, fun, method, domain, inp),  values in results.items():
	if (method in methods or methods == []) and (domain in domains or domains == []):
		avg = round(statistics.mean(values),4)
		sd = round(statistics.stdev(values),4)
		mini = round(min(values),4)
		maxi = round(max(values),4)
		medi = round(statistics.median(values),4)
		ran  = round(maxi - mini,4)
		finalResults.append([mod, fun, method, domain, inp, avg, sd, mini, maxi, medi, ran])

finalResults.sort()

with open('statistics.csv', mode='w') as resultFile:
	csv_writer = csv.writer(resultFile, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	for row in finalResults:
		csv_writer.writerow(row)

print(results)
