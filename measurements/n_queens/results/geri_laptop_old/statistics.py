import csv
import sys
import statistics

filenames = ["queens_res_" + suf + ".csv" for suf in ["array", "fix", "list", "nohof", "par", "sleep"]]

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
	avg = statistics.mean(values)
	sd = statistics.stdev(values)
	mini = min(values)
	maxi = max(values)
	medi = statistics.median(values)
	ran  = maxi - mini
	finalResults.append([mod, fun, method, domain, inp, avg, sd, mini, maxi, medi, ran])

finalResults.sort()

with open('statistics.csv', mode='w') as resultFile:
	csv_writer = csv.writer(resultFile, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	for row in finalResults:
		csv_writer.writerow(row)

print(results)
