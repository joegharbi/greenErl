import argparse
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
from matplotlib import rc #for latex
import numpy as np
import ast #"[12]" convert to [12]
from decimal import *
import os
from scipy.stats.stats import pearsonr   #correlation
from collections import defaultdict
from tkinter import *
from tkinter import filedialog
import pprint
from enum import Enum, unique
import pdb

@unique
class Domain(Enum):
    # RAM = "ram"
    CORE = "core"
    # GPU = "gpu"
    # PKG = "pkg"
    TIME = "time"
    # ALL = "all"

@unique
class Method(Enum):
    # SYSFS = "sysfs"
    # PERF_EVENT = 'perf_event'
    MSR = "msr"

class Measurement:
    def __init__(self):
        # self.ram = Decimal(0)
        # self.gpu = Decimal(0)
        self.cores = Decimal(0)
        # self.pkg = Decimal(0)
        self.time = Decimal(0)
    def Get(self,domain):
        # if(str(domain) == str(Domain.RAM)):
        #     return self.ram
        if(str(domain) == str(Domain.CORE)):
            return self.cores
        # elif(str(domain) == str(Domain.CORE)):
        #     return self.cores
        # elif(str(domain) == str(Domain.GPU)):
        #     return self.gpu
        # elif(str(domain) == str(Domain.PKG)):
        #     return self.pkg
        elif(str(domain) == str(Domain.TIME)):
            return self.time
        else:
            return self.ram + self.pkg
    def Set(self, valtype, val):
        # if(valtype == 'energy-ram'):
        #     if(self.ram != Decimal(0)):
        #         raise ValueError('self.ram was not 0')
        #     self.ram = val
        # elif (valtype == 'energy-gpu'):
        #     if(self.gpu != Decimal(0)):
        #         raise ValueError('self.gpu was not 0')
        #     self.gpu = val
        if (valtype == 'energy-cores'):
            if(self.cores != Decimal(0)):
                raise ValueError('self.cores was not 0')
            self.cores = val
        # elif (valtype == 'energy-cores'):
        #     if(self.cores != Decimal(0)):
        #         raise ValueError('self.cores was not 0')
        #     self.cores = val
        # elif (valtype == 'energy-pkg'):
        #     if(self.pkg != Decimal(0)):
        #         raise ValueError('self.pkg was not 0')
        #     self.pkg = val
        elif (valtype == 'time'):
            if(self.time != Decimal(0)):
                raise ValueError('self.time was not 0')
            self.time = val
        else:
            pass
    def __repr__(self):
        return "Measurement()"
    def __str__(self):
        return " CORES " + str(self.cores) + " TIME " + str(self.time) + '\n'
        # return "RAM: " + str(self.ram) + " GPU "  + str(self.gpu) + " CORES " + str(self.cores) + " PKG " + str(self.pkg) + " TIME " + str(self.time) + '\n'


class GUI:
    def __init__(self):
        self.window = Tk()
        self.window.title('GreenErlang Visualization')
        self.window.geometry('800x500')
        r = 0
        self.visualizationPathButton = Button(self.window,
                                        text = 'Select files',
                                        command = self.GetFilesDialog,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r,padx=10,pady=(20,5), sticky=W)
        r += 1
        self.visualizationPathButton = Button(self.window,
                                        text = 'Select a directory',
                                        command = self.GetDirectoryDialog,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r,padx=10,pady=5,sticky=W)

        self.csvIsChecked = BooleanVar(value=True)
        self.csvCheckBox = Checkbutton(
            self.window, text="CSV files",
            variable=self.csvIsChecked, offvalue=False,onvalue=True)
        self.csvCheckBox.grid(column=1, row=r, padx=5, pady=5, sticky=W)

        self.txtIsChecked = BooleanVar()
        self.txtCheckBox = Checkbutton(
            self.window, text="TXT files",
            variable=self.txtIsChecked, offvalue=False,onvalue=True)
        self.txtCheckBox.grid(column=2, row=r,padx=10,pady=10,sticky=W)
        r += 1
        self.visualizationPathButton = Button(self.window,
                                        text = 'Clear files',
                                        command = self.ClearFiles,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r, padx=10, pady=(5,20), sticky=W)

        # r += 1
        # self.visualisationOptionLabel = Label(self.window,
        #                                            text = 'Method:')
        # self.visualisationOptionLabel.grid(column=0, row=r, padx=10, pady=10, sticky=W)

        self.methodOptions = ('msr','sysfs','perf_event')
        # self.methodOptions = ('msr','sysfs','perf_event')
        self.selectedOption = StringVar()
        self.selectedOption.set(self.methodOptions[0])
        # self.visualisationOptionMenu = OptionMenu(self.window, self.selectedOption, *self.methodOptions)
        # self.visualisationOptionMenu.grid(column=1, row=r, sticky=W)

        r += 1
        self.visualisationDomainLabel = Label(self.window,
                                                   text = 'Domains:')
        self.visualisationDomainLabel.grid(column = 0, row=r, sticky=W,padx=10,pady=10,rowspan = 3)
        # self.ramIsChecked = BooleanVar(value=False)
        self.coreIsChecked = BooleanVar(value=True)
        # self.gpuIsChecked = BooleanVar(value=False)
        # self.pkgIsChecked = BooleanVar(value=False)
        self.timeIsChecked = BooleanVar(value=True)
        # self.allIsChecked = BooleanVar(value=True)
        # self.ramCheckBox = Checkbutton(
        # self.window, text="RAM",
        #     variable=self.ramIsChecked, offvalue=False,onvalue=True)
        # self.ramCheckBox.grid(column=1, row=r,sticky=W)

        self.coreCheckBox = Checkbutton(
        self.window, text="core",
            variable=self.coreIsChecked, offvalue=False,onvalue=True)
        self.coreCheckBox.grid(column=2, row=r,sticky=W)
        # r += 1
        # self.gpuCheckBox = Checkbutton(
        # self.window, text="gpu",
        #     variable=self.gpuIsChecked, offvalue=False,onvalue=True)
        # self.gpuCheckBox.grid(column=1, row=r,sticky=W)

        # self.pkgCheckBox = Checkbutton(
        # self.window, text="pkg",
        #     variable=self.pkgIsChecked, offvalue=False,onvalue=True)
        # self.pkgCheckBox.grid(column=2, row=r,sticky=W)
        # r += 1
        self.timeCheckBox = Checkbutton(
        self.window, text="time",
            variable=self.timeIsChecked, offvalue=False,onvalue=True)
        self.timeCheckBox.grid(column=1, row=r,sticky=W)

        # self.allCheckBox = Checkbutton(
        # self.window, text="all",
        #     variable=self.allIsChecked, offvalue=False,onvalue=True)
        # self.allCheckBox.grid(column=2, row=r,sticky=W)

        r += 1
        self.visualisationLogScaleLabel = Label(self.window,
                                                   text = 'LogScale:')
        self.visualisationLogScaleLabel.grid(column = 0, row=r, sticky=W, padx=10, pady=10)
        self.logScaleIsChecked = BooleanVar(value=False)
        self.logScaleCheckBox = Checkbutton(
            self.window,
            variable=self.logScaleIsChecked, offvalue=False,onvalue=True)
        self.logScaleCheckBox.grid(column=1, row=r, sticky=W)

        r += 1
        self.visualizationPathButton = Button(self.window,
                                        text = 'Draw Selected Figures',
                                        command = self.DrawSelected,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r,padx=10,pady=5, sticky=W)

        r += 1
        self.visualizationPathButton = Button(self.window,
                                        text = 'Draw All Figures',
                                        command = self.QuickVis,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r,padx=10,pady=5, sticky=W)

        r += 1
        self.visualizationPathButton = Button(self.window,
                                        text = 'Latex Export',
                                        command = self.LatexExport,
                                        padx=5)
        self.visualizationPathButton.grid(column=0, row=r,padx=10,pady=5, sticky=W)

        r += 1
        self.errorLabel = Label(self.window,
                                text = '',
                                justify = 'left',
                                anchor  = 'w',
                                width = 25,
                                wraplength = 220)
        self.errorLabel.grid(column=0, row=r,sticky=W)

        self.exporter = LatexExporter()
        self.visualizer = MeasurementVisualizer()
        self.resultHandler = ResultHandler()
        self.ClearFiles()
        self.window.mainloop()
    def ClearFiles(self):
        self.measurementFiles = set()
    def GetFilesDialog(self):
        newFiles = self.window.tk.splitlist(filedialog.askopenfilenames(parent=self.window,title='Choose files',filetypes=[("CSV Files",".csv"),("Text Files",".txt")]))
        self.measurementFiles.update(newFiles)
    def GetDirectoryDialog(self):
        directory = filedialog.askdirectory(parent=self.window,title='Choose Directory')
        print(directory)
        for file in os.listdir(directory):
            filename = os.fsdecode(file)
            if filename.endswith(".txt") and self.txtIsChecked.get(): 
                self.measurementFiles.add(os.path.join(directory, filename))
                continue
            if filename.endswith(".csv") and self.csvIsChecked.get(): 
                self.measurementFiles.add(os.path.join(directory, filename))
                continue
            else:
                continue
        #print(self.measurementFiles)
    def GetMeasurements(self):
        if(len(self.measurementFiles) == 0):
            return
        #print(self.measurementFiles)
        for filename in self.measurementFiles:
            if filename.lower().endswith(".txt"):
                self.resultHandler.AppendFromTXT(filename)
                continue
            if filename.lower().endswith(".csv"):
                self.resultHandler.AppendFromCSV(filename)
                continue
    def DrawSelected(self):
        self.resultHandler.Clear()
        self.GetMeasurements()
        if self.resultHandler.IsEmpty():
            self.errorLabel.configure(text='Give me some files first', fg='red')
            return
        self.visualizer.AttachResultHandler(self.resultHandler)
        if self.ramIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.RAM, self.logScaleIsChecked.get())
        if self.coreIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.CORE, self.logScaleIsChecked.get())
        if self.gpuIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.GPU, self.logScaleIsChecked.get())
        if self.pkgIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.PKG, self.logScaleIsChecked.get())
        if self.timeIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.TIME, self.logScaleIsChecked.get())
        if self.allIsChecked.get():
            self.visualizer.DrawFigure(self.selectedOption.get(), Domain.ALL, self.logScaleIsChecked.get())
        self.errorLabel.configure(text='OK', fg='green')
        return
    def QuickVis(self):
        self.resultHandler.Clear()
        self.GetMeasurements()
        if self.resultHandler.IsEmpty():
            self.errorLabel.configure(text='Give me some files first', fg='red')
            return
        self.visualizer.AttachResultHandler(self.resultHandler)
        self.visualizer.DrawAllFigures(self.selectedOption.get(),self.logScaleIsChecked.get())
        self.errorLabel.configure(text='OK', fg='green')
        return
    def LatexExport(self):
        self.resultHandler.Clear()
        self.GetMeasurements()
        if self.resultHandler.IsEmpty():
            self.errorLabel.configure(text='Give me some files first', fg='red')
            return
        self.exporter.AttachResultHandler(self.resultHandler)
        filename = filedialog.asksaveasfilename(parent=self.window,title = "Select file",filetypes = (("tex files","*.tex"),("all files","*.*")))
        # if self.ramIsChecked.get():
        #     self.exporter.ExportToFile(filename + '_ram',self.selectedOption.get(), Domain.RAM, self.logScaleIsChecked.get())
        if self.coreIsChecked.get():
            self.exporter.ExportToFile(filename + '_ram',self.selectedOption.get(), Domain.CORE, self.logScaleIsChecked.get())
        # if self.gpuIsChecked.get():
        #     self.exporter.ExportToFile(filename + '_gpu',self.selectedOption.get(), Domain.GPU, self.logScaleIsChecked.get())
        # if self.pkgIsChecked.get():
        #     self.exporter.ExportToFile(filename + '_pkg',self.selectedOption.get(), Domain.PKG, self.logScaleIsChecked.get())
        if self.timeIsChecked.get():
            self.exporter.ExportToFile(filename + '_time',self.selectedOption.get(), Domain.TIME, self.logScaleIsChecked.get())
        # if self.allIsChecked.get():
        #     self.exporter.ExportToFile(filename + '_all',self.selectedOption.get(), Domain.ALL, self.logScaleIsChecked.get())
        self.errorLabel.configure(text='OK', fg='green')
        return

class ResultHandler:
    def __init__(self):
        self.Clear()
        self.resultsCalced = False
        self.isEmpty = True
        return
    def IsEmpty(self):
        return self.isEmpty
    def Clear(self):
        self.measurements = {}
        self.results = {
            'msr':  {
                    # Domain.RAM: {},
                    # Domain.GPU: {},
                    Domain.CORE: {},
                    # Domain.PKG: {},
                    Domain.TIME: {}
                    # ,
                    # Domain.ALL: {}
                    }
                    # ,
            # 'sysfs':  {
            #         Domain.RAM: {},
            #         Domain.GPU: {},
            #         Domain.CORE: {},                 
            #         Domain.PKG: {},
            #         Domain.TIME: {},
            #         Domain.ALL: {}
            #         },
            # 'perf_event':  {
            #         Domain.RAM: {},
            #         Domain.GPU: {},
            #         Domain.CORE: {},
            #         Domain.PKG: {},
            #         Domain.TIME: {},
            #         Domain.ALL: {}
                    # }
        }
        self.isEmpty = True
    def GetResults(self):
        if not self.resultsCalced:
            self.CalculateResults()
        return self.results
    def AppendFromCSV(self,filename):
        self.AppendFromFile(filename,';')
    def AppendFromTXT(self,filename):
        self.AppendFromFile(filename,' ')
    def AppendFromFile(self,filename,delimeter=';'):
        currentFile = open(filename, 'r') 
        self.resultsCalced = False
        for rows in currentFile:
            row = rows.strip().split(delimeter)
            if(len(row) != 6):
                raise Exception('Unexpected row length in file:'+row)
            #print(row)
            key = (row[0],row[1],row[2],row[3])
            valuetype = (row[4])
            value = Decimal(row[5])
            if(key in self.measurements):
                self.measurements[key].Set(valuetype,value)
            else:
                self.measurements[key] = Measurement()
                self.measurements[key].Set(valuetype,value)
        self.isEmpty = False
        return
    def CalculateResults(self):
        #pp = pprint.PrettyPrinter(indent=4)
        #pp.pprint(self.measurements)
        self.results.clear()
        self.results = {
            'msr':  {
                    # Domain.RAM: {},
                    # Domain.GPU: {},
                    Domain.CORE: {},
                    # Domain.PKG: {},
                    Domain.TIME: {}
                    # Domain.ALL: {}
                    }
                    # ,
            # 'sysfs':  {
            #         Domain.RAM: {},
            #         Domain.GPU: {},
            #         Domain.CORE: {},                 
            #         Domain.PKG: {},
            #         Domain.TIME: {},
            #         Domain.ALL: {}
            #         },
            # 'perf_event':  {
            #         Domain.RAM: {},
            #         Domain.GPU: {},
            #         Domain.CORE: {},
            #         Domain.PKG: {},
            #         Domain.TIME: {},
            #         Domain.ALL: {}
            #         }
        }

        for key, value  in self.measurements.items():
            functionname = key[0] + '/' + key[1]
            if(key[3] == 'time'):
                for method in self.results:
                    if(functionname in self.results[method][Domain.TIME]):
                        self.results[method][Domain.TIME][functionname].append((ast.literal_eval(key[2]),value.Get(Domain.TIME)))
                    else:
                        self.results[method][Domain.TIME][functionname] = [(ast.literal_eval(key[2]),value.Get(Domain.TIME))]
            else:
                for domain in Domain:
                    if(str(domain) == str(Domain.TIME)):
                        continue
                    else:
                        if(functionname in self.results[key[3]][domain]):
                            self.results[key[3]][domain][functionname].append((ast.literal_eval(key[2]),value.Get(domain)))
                        else:
                            self.results[key[3]][domain][functionname] = [(ast.literal_eval(key[2]),value.Get(domain))]
        for key,method  in self.results.items():
            for key, domain in method.items():
                for function, results in domain.items():
                    results.sort()
        #pp.pprint(self.results['msr'])
        self.resultsCalced = True

class MeasurementVisualizer:
    def __init__(self):
        self.resultHandler = None
        return
    def AttachResultHandler(self, resultHandler):
        self.resultHandler = resultHandler
    def DrawAllFigures(self,method,logscale = False):
        if(self.resultHandler is None):
            raise Exception('Resulthandler is required')
        window = Tk()
        counter = 0
        results = self.resultHandler.GetResults()
        for key, domain in results[method].items():
            fig = Figure(figsize=(5,5), tight_layout= True)
            if(logscale):
                a = fig.add_subplot(111, yscale='log') 
            else:
                a = fig.add_subplot(111) 
            for function, results in domain.items():
                a.plot(*zip(*results),'--o', label=function)
                a.legend()
            canvas = FigureCanvasTkAgg(fig, master=window)
            w = Label(window, text=str(key))
            w.grid(column=int(counter % 3), row = int(counter/3)*2 + 1)
            canvas.get_tk_widget().grid(column=int(counter % 3), row = int(counter/3)*2,padx=10,pady=10)
            canvas.draw()
            counter += 1
    def DrawFigure(self,method,domain,logscale = False):
        if(self.resultHandler is None):
            raise Exception('Resulthandler is required')
        logstr = 'log' if logscale else ''
        fig = plt.figure(method + domain.value + logstr)
        fig.canvas.set_window_title(method + ' - ' + str(domain.value))
        results = self.resultHandler.GetResults()
        keys = list(results[method][domain].keys())
        keys.sort()
        for key in keys:
            value = results[method][domain][key]
            plt.plot(*zip(*value),'--o', label=key)
            plt.legend(loc='best')
            if(logscale):
                plt.yscale('log')
        fig.show()

class LatexExporter:
    def __init__(self):
        self.resultHandler = None
        return
    def AttachResultHandler(self, resultHandler):
        self.resultHandler = resultHandler
    def ExportToFile(self,filename, method, domain, logscale = False):
        datafilename = '{}.dat'.format(str(filename))
        directory = os.path.dirname(datafilename)
        if not os.path.exists(directory):
            os.makedirs(directory)
        if(self.resultHandler is None):
            raise Exception('Resulthandler is required')
        results = self.resultHandler.GetResults()

        xvalues = set()
        keys = list(results[method][domain].keys())
        keys.sort()
        for key in keys:
            for values in results[method][domain][key]:
                xvalues.add(values[0])
        xvalues = list(xvalues)
        xvalues.sort()
        datafile = open(datafilename, 'w')
        latexfile = open('{}.tex'.format(str(filename)), 'w')

        datafile.write("N ") #x tengely neve
        for key, value  in results[method][domain].items(): 
            datafile.write("{} ".format(key).replace('_','-')) #fv nevek kiirasa
        datafile.write("\n") #uj sor

        for xvalue in xvalues:
            datafile.write(str(xvalue) + " ") #X tengely kiírása

            for key, value  in results[method][domain].items(): 
                l = False
                for (v1,v2) in value:
                    if(v1==xvalue):
                        datafile.write("{} ".format(str(v2))) #fv nevek kiirasa
                        l=True
                        break
                if(not l):
                    datafile.write("nan ")
            datafile.write("\n")
        datafile.close()
        latexRawStart = [r"\begin{tikzpicture}",r"\begin{axis}[legend style={at={(0,1)},anchor=north west}, ylabel=Consumption (J), xlabel=N Values, width=\textwidth, height=0.4\textheight]"]
        latexRawStartLog = [r"\begin{tikzpicture}",r"\begin{axis}[legend style={at={(0,1)},anchor=north west}, ylabel=Consumption (J), xlabel=N Values, ymode=log, width=\textwidth, height=0.4\textheight]"]
        latexRawEnd = [r"\end{axis}",r"\end{tikzpicture}"]
        if(logscale): 
            latexfile.writelines('\n'.join(latexRawStartLog) + '\n')
        else:
            latexfile.writelines('\n'.join(latexRawStart) + '\n')
        for key, value  in results[method][domain].items(): 
            latexfile.write("\\addplot table [x=N, y={}]{{{}}};".format((str)(key).replace('_','-'),datafilename) + "\n")
            latexfile.write("\\addlegendentry{{{}}}".format(key).replace('_','-') + "\n")
        latexfile.writelines('\n'.join(latexRawEnd) + '\n')
        latexfile.close()

def main():
    # Parsing args here
    gui = GUI()


if __name__ == '__main__':
    main()