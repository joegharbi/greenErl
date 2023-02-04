import argparse
import matplotlib.pyplot as plt
from matplotlib import rc #for latex
import numpy as np
import ast #"[12]" convert to [12]
from decimal import *
import os
from scipy.stats.stats import pearsonr   #correlation
from collections import defaultdict

import pdb


#Overkill class for measurements
class Measurement:
    def __init__(self):
        self.ram = Decimal(0)
        self.gpu = Decimal(0)
        self.cores = Decimal(0)
        self.pkg = Decimal(0)
        self.time = Decimal(0)
    def sum(self,tpye):
        if(str(tpye) == "ram"):
            return self.ram
        elif(str(tpye) == "core"):
            return self.cores
        elif(str(tpye) == "gpu"):
            return self.gpu
        elif(str(tpye) == "pkg"):
            return self.pkg
        elif(str(tpye) == "time"):
            return self.time
        else:
            return self.ram + self.pkg
    def add(self, valtype, val):
        if(valtype == 'energy-ram'):
            self.ram += val
        elif (valtype == 'energy-gpu'):
            self.gpu += val
        elif (valtype == 'energy-cores'):
            self.cores += val
        elif (valtype == 'energy-pkg'):
            self.pkg += val
        elif (valtype == 'time'):
            self.time += val
        else:
            pass



#Argument parsing
parser = argparse.ArgumentParser(description='Measurement visualization', prog='vis.py')
parser.add_argument('-m',
                    '--method',
                    choices=['sysfs', 'msr', 'perf_event','time'],
                    default=['msr'],
                    help='Method: sysfs/msr/perf_event',
                    nargs='+')
parser.add_argument('-t',
                    '--type',
                    choices=['ram', 'gpu', 'core', 'pkg', 'all'],
                    default='all',
                    help='Type: ram/gpu/core/all. all=pkg+ram',
                    nargs='?')
parser.add_argument('-f',
                    '--files', 
                    nargs='+', 
                    type=argparse.FileType('r'),
                    help='<Required> List of data files', 
                    required=True)
parser.add_argument('-o',
                    '--output', 
                    nargs='?', 
                    help='Output data and latex file for Latex visualization')
parser.add_argument('-c',
                    '--correlation', 
                    nargs='?', 
                    help='Output file for correlation and avg joule/time')
#parser.add_argument('-om',
#                    '--outputmode',
#                    choices=['sysfs', 'msr', 'perf_event'],
#                    default='msr',
#                    nargs='?')
parser.add_argument('-l',
                    '--logscale',
                    action='store_true')
args = parser.parse_args()

#File processing
measurements = {}
for files in args.files:
    splitChar = ' '
    if files.name[-4:] == '.csv':
        splitChar = ';'
    for rows in files:
        row = rows.split(splitChar)
        key = (row[0],row[1],row[2],row[3])
        valuetype = (row[4])
        value = Decimal(row[5])
        if(key in measurements):
            measurements[key].add(valuetype,value)
        else:
            measurements[key] = Measurement()
            measurements[key].add(valuetype,value)

if(args.correlation):
    logfilename = str(args.correlation)
    directory = os.path.dirname(logfilename)
    if not os.path.exists(directory):
        os.makedirs(directory)
    logfile = open(logfilename, 'w')
#Matplotlib visualization
def makefigure(name):
    functions = {}
    correlation = {}
    currentType = args.type
    if (name == 'time'):
        currentType = 'time'
    for key, value  in measurements.items():
        if(key[3] == 'time'):
            functionname = key[0] + '/' + key[1]
            if(functionname in correlation):
                correlation[functionname][1].append((ast.literal_eval(key[2]),value.sum('time')))
            else:
                correlation[functionname] = [[],[]]
                correlation[functionname][1].append((ast.literal_eval(key[2]),value.sum('time')))
        if(key[3] == name):
            functionname = key[0] + '/' + key[1]
            if(functionname in functions):
                functions[functionname].append((ast.literal_eval(key[2]),value.sum(currentType)))
            else:
                functions[functionname] = [(ast.literal_eval(key[2]),value.sum(currentType))]
            if(functionname in correlation):
                correlation[functionname][0].append((ast.literal_eval(key[2]),value.sum(currentType)))
            else:
                correlation[functionname] = [[],[]]
                correlation[functionname][0].append((ast.literal_eval(key[2]),value.sum(currentType)))
    if(args.correlation):
        for key, value in correlation.items():
            value[0].sort()
            value[1].sort()
            times = []
            values = []
            unity = []
            for i in range(1,len(value[0])):
                unity.append((value[0][i][0],float(value[0][i][1] / value[1][i][1])))
            for i,j in value[0]:
                values.append(float(j))
            for i,j in value[1]:
                times.append(float(j))
            logfile.write(key.upper() + ":\n")
            logfile.write("correlation: \n")
            logfile.write(str(pearsonr(values,times)[0])+ '\n')
            logfile.write("Joule/time: "+ '\n')
            logfile.write('\n'.join('{}: {}'.format(x[0],x[1]) for x in unity) + '\n')
            logfile.write("AVG joule/time: \n")
            logfile.write(str(sum(j for i, j in unity)/float(len(unity))) + '\n')
            logfile.write("SUM joule/ SUM time: \n")
            logfile.write(str(sum(i for i in values)/sum(i for i in times)) + '\n\n')
        #print(np.corrcoef(values,times))

    fig = plt.figure(name)
    fig.canvas.set_window_title(name + ' - ' + str(currentType))
    for key, value  in functions.items():
        value.sort()
        plt.plot(*zip(*value),'--o', label=key)
        plt.legend()
        if(args.logscale):
            plt.yscale('log')

if ('msr' in args.method):
    makefigure('msr')
if ('sysfs' in args.method):
    makefigure('sysfs')
if ('perf_event' in args.method):
    makefigure('perf_event')
if ('time' in args.method):
    makefigure('time')

plt.show()

#out to latex data file
outputmethod = args.method[0]
if(args.output):
    datafilename = '{}.dat'.format(str(args.output))
    directory = os.path.dirname(datafilename)
    if not os.path.exists(directory):
        os.makedirs(directory)
    datafile = open(datafilename, 'w')
    latexfile = open('{}.tex'.format(str(args.output)), 'w')
    xvalues = []
    functions = {}
    outputtype = args.type
    if(outputmethod == 'time'):
        outputtype = 'time'
    for key, value  in measurements.items():
        if(key[3] == outputmethod):
            xvalue = ast.literal_eval(key[2]) #x tengely egy erteke pl. [12] -> 12
            if(not xvalue in xvalues):
                xvalues.append(xvalue)
            functionname = key[0] + '/' + key[1]
            if(functionname in functions):
                functions[functionname].append((xvalue,value.sum(outputtype)))
            else:
                functions[functionname] = [(xvalue,value.sum(outputtype))]
    for key, value  in functions.items():
        value.sort()
    xvalues.sort()

    #tenyleges kiiras
    datafile.write("N ") #x tengely neve
    for key, value  in functions.items(): 
        datafile.write("{} ".format(key).replace('_','-')) #fv nevek kiirasa
    datafile.write("\n") #uj sor

    for xvalue in xvalues:
        datafile.write(str(xvalue) + " ") #X tengely kiírása

        for key, value  in functions.items(): 
            l = False
            for (v1,v2) in value:
                if(v1==xvalue):
                    datafile.write("{} ".format(str(v2))) #fv nevek kiirasa
                    l=True
            if(not l):
                datafile.write("nan ")
        datafile.write("\n")
    datafile.close()

    #LATEX FILE generálása ronda módon
    latexRawStart = [r"\begin{tikzpicture}",r"\begin{axis}[legend style={at={(0,1)},anchor=north west}, ylabel=Consumption (J), xlabel=N Values, width=\textwidth, height=0.4\textheight]"]
    #TODO: lehetne szebb 
    latexRawStartLog = [r"\begin{tikzpicture}",r"\begin{axis}[legend style={at={(0,1)},anchor=north west}, ylabel=Consumption (J), xlabel=N Values, ymode=log, width=\textwidth, height=0.4\textheight]"]
    latexRawEnd = [r"\end{axis}",r"\end{tikzpicture}"]
    if(args.logscale): #LEHETNE SZEBB
        latexfile.writelines('\n'.join(latexRawStartLog) + '\n')
    else:
        latexfile.writelines('\n'.join(latexRawStart) + '\n')
    for key, value  in functions.items(): 
        latexfile.write("\\addplot table [x=N, y={}]{{{}}};".format((str)(key).replace('_','-'),datafilename) + "\n")
        latexfile.write("\\addlegendentry{{{}}}".format(key).replace('_','-') + "\n")
    latexfile.writelines('\n'.join(latexRawEnd) + '\n')
    latexfile.close()
