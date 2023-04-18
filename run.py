# Summary:
#  - run this script and then select what you want to measure
#  - compile measuring erlang program
#  - generate erlang commands for measurement
#  - select file to measure
#  - select function(s) to measure or measure all exported functions
#  - specify result file names or generate based on something

# HOW TO USE
#  - MUST BE RUN WITH SUDO FOR MEASUREMENTS!
#  - Select Erlang file that contains the functions you want to measure
#  - Rapl program and Erlang measure module already selected,
#    but modify if you want to use different path
#  - Set number of measurements. This means the number of times a single function
#    will be run for a single input
#  - Functions to measure
#    Comma separated name of functions, without module name!
#    If left empty all exported functions (except generate_input) are measured
#  - Input desctiptions
#    Comma separated input descriptions. If it consists of a single number, etc.
#    no need to write it as lists, otherwise lists must be written.
#    If generate_input function is available (with correct arity) in the given module:
#     pass input descriptions to generate_input, and get inputs that way.
#    Otherwise treat the input description as the actual input.
#  - Result folder name
#    This folder will be created inside the results folder next to the erlang module being measured.
#    All results will be logged there.


#  Multiple measurements supported, you need to add measurements using the Add measurement button
#  When starting the measurements, the current(!) input will be validated and if all fields are valid
#  it will also be added to the measurements.
#  CAREFUL: if current input is not valid when starting measurements, no warning is given, it is simply omitted
#  SUGGESTION: add all measurements before starting to measure, to make sure everything entered is valid!

import argparse
import copy
import json
from tkinter import *
from tkinter import filedialog
from tkinter import ttk
import subprocess
import sys
import os
import vis
import time
import pdb
import os
import re
import csv
import json
import matplotlib.pyplot as plt

# if os.getuid() != 0:
#     print("\033[1m\033[91mERROR: YOU MUST BE ROOT TO RUN THIS PROGRAM\033[0m")
#     exit()

# Class for storing measurement data
class Measurement:
    def __init__(self):
        self.setDefaultValues()

    def setDefaultValues(self):
        self.erlangFile = ''
        # self.erlangMeasureFile = 'C:/erlab/lab_II/greenErl/green_erlang/rapl_erlang/energy_consumption_res.erl'
        self.erlangMeasureFile = os.getcwd().replace("\\", "/") + '/rapl_erlang/energy_consumption_res.erl'
        self.numberOfMeasurements = 10
        self.erlangMeasureModule = 'energy_consumption_res'
        self.moduleName = ''
        self.functionsToMeasure = ''
        self.resultPath = ''
        self.inputDescs = '[]'

    def clear(self):
        self.setDefaultValues()

# Class for creating GUI
class GUI:
    def __init__(self):
        self.measurement = Measurement()
        self.measurementList = []
        self.window = Tk()  # Create window
        self.window.title('Measure Erlang')  # Set window title
        self.window.geometry('800x600')  # Resize window to 800x600px

        # Input Erlang file
        r = 0
        self.erlangFileLabel = Label(self.window,
                                     justify='right',
                                     anchor='e',
                                     width=25,
                                     text='Erlang file to measure')
        self.erlangFileLabel.grid(column=0, row=r, sticky=E)
        self.erlangFileButton = Button(self.window,
                                       text='Select file',
                                       width=8,
                                       command=self.getErlangFilePath)
        self.erlangFileButton.grid(column=1, row=r)
        self.currentErlangFileLabel = Label(self.window,
                                            justify='left',
                                            text='No file selected')
        self.currentErlangFileLabel.grid(column=2, row=r, sticky=W)


        # Input measuring erlang
        r += 1
        self.erlangMeasureFileLabel = Label(self.window,
                                            justify='right',
                                            anchor='e',
                                            width=25,
                                            text='Erlang module to measure with')
        self.erlangMeasureFileLabel.grid(column=0, row=r, sticky=E)
        self.erlangMeasureFileButton = Button(self.window,
                                              text='Select file',
                                              width=8,
                                              command=self.getErlangMeasureFilePath)
        self.erlangMeasureFileButton.grid(column=1, row=r)
        self.currentErlangMeasureFileLabel = Label(self.window,
                                                   justify='left',
                                                   text=self.measurement.erlangMeasureFile)
        self.currentErlangMeasureFileLabel.grid(column=2, row=r, sticky=W)

        # Number of measurements
        r += 1
        self.erlangMeasureFileLabel = Label(self.window,
                                            justify='right',
                                            anchor='e',
                                            width=25,
                                            text='Number of measurements')
        self.erlangMeasureFileLabel.grid(column=0, row=r, sticky=E)
        self.numberOfMeasurements = IntVar(self.window)
        self.numberOfMeasurements.set(self.measurement.numberOfMeasurements)
        self.numberOfMeasurementsSpinbox = Spinbox(self.window,
                                                   from_=1,
                                                   to=100,
                                                   width=8,
                                                   textvariable=self.numberOfMeasurements)
        self.numberOfMeasurementsSpinbox.grid(column=1, row=r)

        # Functions to measure, comma separated, empty for all
        r += 1
        self.erlangFunctionsToMeasureLabel = Label(self.window,
                                                   justify='right',
                                                   anchor='e',
                                                   width=25,
                                                   text='Functions to measure')
        self.erlangFunctionsToMeasureLabel.grid(column=0, row=r, sticky=E)
        self.functionsToMeasureEntry = Entry(self.window,
                                             width=10)
        self.functionsToMeasureEntry.grid(column=1, row=r)

        # Input descriptors used to generate input
        r += 1
        self.inputDescsLabel = Label(self.window,
                                     justify='right',
                                     anchor='e',
                                     width=25,
                                     text='Input descriptions')
        self.inputDescsLabel.grid(column=0, row=r, sticky=E)
        self.inputDescsEntry = Entry(self.window,
                                     width=10)
        self.inputDescsEntry.grid(column=1, row=r)

        # Result folder
        r += 1
        self.resultFolderLabel = Label(self.window,
                                       justify='right',
                                       anchor='e',
                                       width=25,
                                       text='Result folder name')
        self.resultFolderLabel.grid(column=0, row=r, sticky=E)
        self.resultFolderEntry = Entry(self.window,
                                       width=10)
        self.resultFolderEntry.grid(column=1, row=r)

        # Add measurement to list
        r += 1
        self.addMeasurementButton = Button(self.window,
                                           text='Add measurement',
                                           command=self.addMeasurement)
        self.addMeasurementButton.grid(column=0, row=r, sticky=W)

        # Measure
        r += 1
        self.erlangMeasureFileButton = Button(self.window,
                                              text='Start measurement',
                                              command=self.startMeasurement)
        self.erlangMeasureFileButton.grid(column=0, row=r, sticky=W)
      

        # Error display
        r += 1
        self.errorLabel = Label(self.window,
                                text='',
                                justify='left',
                                anchor='w',
                                width=25,
                                wraplength=220)
        self.errorLabel.grid(column=0, row=r, sticky=W)

        self.window.mainloop()  # Start the main loop for window

    def getErlangFilePath(self):
        erlangFile = filedialog.askopenfilename()
        if erlangFile == ():
            return
        else:
            self.measurement.erlangFile = erlangFile
            self.measurement.moduleName = getModuleNameFromPath(erlangFile)
            self.currentErlangFileLabel.configure(
                text=getRelativePath(erlangFile))

    def getErlangMeasureFilePath(self):
        erlangMeasureFile = filedialog.askopenfilename()
        if erlangMeasureFile == ():
            return
        else:
            self.measurement.erlangMeasureFile = erlangMeasureFile
            self.measurement.erlangMeasureModule = getModuleNameFromPath(
                erlangMeasureFile)
            self.currentErlangMeasureFileLabel.configure(
                text=getRelativePath(erlangMeasureFile))
            
    def populateAndValidateMeasurementData(self):
        self.measurement.numberOfMeasurements = self.numberOfMeasurements.get()
        errorStr = ''
        if self.measurement.erlangFile == '':
            errorStr += 'No Erlang file selected for measurement!\n'

        functionsToMeasureStr = self.functionsToMeasureEntry.get().strip()
        self.measurement.functionsToMeasure = '[{0}]'.format(
            functionsToMeasureStr)
        if functionsToMeasureStr == '':
            self.measurement.functionsToMeasure = 'all'

        resultRelativePath = self.resultFolderEntry.get()
        self.measurement.resultPath = removeFilenameFromPath(
            self.measurement.erlangFile) + '\\\\results\\\\' + resultRelativePath +'\\\\'
        if resultRelativePath == '':
            errorStr += 'No folder for results specified\n'
        else:
            if not os.path.exists(self.measurement.resultPath):
                os.makedirs(self.measurement.resultPath)

        self.measurement.inputDescs = '[{0}]'.format(
            self.inputDescsEntry.get())
        if self.measurement.inputDescs == '[]':
            errorStr += 'No inputs specified\n'

        if errorStr == '':
            self.errorLabel.configure(text='OK', fg='green')
        else:
            self.errorLabel.configure(text=errorStr, fg='red')
            return False
        return True

    def resetGUIData(self):
        self.currentErlangFileLabel.configure(
            text=getRelativePath(self.measurement.erlangFile))
        self.functionsToMeasureEntry.delete(0, END)
        self.inputDescsEntry.delete(0, END)
        self.resultFolderEntry.delete(0, END)
        self.numberOfMeasurements.set(self.measurement.numberOfMeasurements)

    def addMeasurement(self):
        if not self.populateAndValidateMeasurementData():
            return False
        self.measurementList.append(copy.deepcopy(self.measurement))
        print("------------Measurement added----------------")
        print("Measure module:", self.measurement.erlangMeasureModule)
        print("Module name:", self.measurement.moduleName)
        print("Input description:", self.measurement.inputDescs)
        print("Result path:", self.measurement.resultPath)
        print("---------------------------------------------")
        self.measurement.clear()
        self.resetGUIData()
        return True

    def startMeasurement(self):
        self.errorLabel.configure(text='Measurement started', fg='yellow')
        self.addMeasurement()
        for measurement in self.measurementList:
            measure(measurement)
            # time.sleep(90)
        self.measurementList = []
        self.errorLabel.configure(text='Measurement done', fg='green')

    # def visualize(self):
    #     self.measurement.numberOfMeasurements = self.numberOfMeasurements.get()
    #     errorStr = ''
    #     if self.measurement.erlangFile == '':
    #         errorStr += 'No Erlang file selected for measurement!\n'

    #     functionsToMeasureStr = self.functionsToMeasureEntry.get().strip()
    #     self.measurement.functionsToMeasure = '[{0}]'.format(
    #         functionsToMeasureStr)
    #     if functionsToMeasureStr == '':
    #         self.measurement.functionsToMeasure = 'all'

    #     resultRelativePath = self.resultFolderEntry.get()
    #     self.measurement.resultPath = removeFilenameFromPath(
    #         self.measurement.erlangFile) + '/results/' + resultRelativePath
    #     if resultRelativePath == '':
    #         errorStr += 'No folder for results specified\n'
    #     else:
    #         if not os.path.exists(self.measurement.resultPath):
    #             errorStr += 'Results folder does not exist\n'

    #     if errorStr == '':
    #         self.errorLabel.configure(text='OK', fg='green')
    #     else:
    #         self.errorLabel.configure(text=errorStr, fg='red')
    #         return

    #     resultFiles = [fileName for fileName in os.listdir(
    #         self.measurement.resultPath) if fileName[-4:] == '.csv']
    #     if self.measurement.functionsToMeasure != 'all':
    #         filteredResults = []
    #         functionNames = [s.strip(
    #         ) + "_log.csv" for s in self.measurement.functionsToMeasure[1:-1].split(',')]
    #         for eachFunctionName in functionNames:
    #             for eachFileName in resultFiles:
    #                 if eachFunctionName in eachFileName:
    #                     filteredResults.append(eachFileName)
    #                     break
    #         resultFiles = filteredResults

    #     completeResultPaths = [self.measurement.resultPath +
    #                            '/' + resultFile for resultFile in resultFiles]
    #     visualizer = vis.MeasurementVisualizer()
    #     resultHandler = vis.ResultHandler()
    #     for filename in completeResultPaths:
    #         resultHandler.AppendFromCSV(filename)
    #     visualizer.AttachResultHandler(resultHandler)
    #     if self.ramIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.RAM, self.logScaleIsChecked.get())
    #     if self.coreIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.CORE, self.logScaleIsChecked.get())
    #     if self.gpuIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.GPU, self.logScaleIsChecked.get())
    #     if self.pkgIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.PKG, self.logScaleIsChecked.get())
    #     if self.timeIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.TIME, self.logScaleIsChecked.get())
    #     if self.allIsChecked.get():
    #         visualizer.DrawFigure(self.selectedOption.get(
    #         ), vis.Domain.ALL, self.logScaleIsChecked.get())

    # def quickVis(self):
    #     self.measurement.numberOfMeasurements = self.numberOfMeasurements.get()
    #     errorStr = ''
    #     if self.measurement.erlangFile == '':
    #         errorStr += 'No Erlang file selected for measurement!\n'

    #     functionsToMeasureStr = self.functionsToMeasureEntry.get().strip()
    #     self.measurement.functionsToMeasure = '[{0}]'.format(
    #         functionsToMeasureStr)
    #     if functionsToMeasureStr == '':
    #         self.measurement.functionsToMeasure = 'all'

    #     resultRelativePath = self.resultFolderEntry.get()
    #     self.measurement.resultPath = removeFilenameFromPath(
    #         self.measurement.erlangFile) + '/results/' + resultRelativePath
    #     if resultRelativePath == '':
    #         errorStr += 'No folder for results specified\n'
    #     else:
    #         if not os.path.exists(self.measurement.resultPath):
    #             errorStr += 'Results folder does not exist\n'

    #     if errorStr == '':
    #         self.errorLabel.configure(text='OK', fg='green')
    #     else:
    #         self.errorLabel.configure(text=errorStr, fg='red')
    #         return

    #     resultFiles = [fileName for fileName in os.listdir(
    #         self.measurement.resultPath) if fileName[-4:] == '.csv']
    #     if self.measurement.functionsToMeasure != 'all':
    #         filteredResults = []
    #         functionNames = [s.strip(
    #         ) + "_log.csv" for s in self.measurement.functionsToMeasure[1:-1].split(',')]
    #         for eachFunctionName in functionNames:
    #             for eachFileName in resultFiles:
    #                 if eachFunctionName in eachFileName:
    #                     filteredResults.append(eachFileName)
    #                     break
    #         resultFiles = filteredResults

    #     completeResultPaths = [self.measurement.resultPath +
    #                            '/' + resultFile for resultFile in resultFiles]
    #     visualizer = vis.MeasurementVisualizer()
    #     resultHandler = vis.ResultHandler()
    #     for filename in completeResultPaths:
    #         resultHandler.AppendFromCSV(filename)
    #     visualizer.AttachResultHandler(resultHandler)
    #     visualizer.DrawAllFigures(
    #         self.selectedOption.get(), self.logScaleIsChecked.get())


def removeFilenameFromPath(fullPath):
    path = '\\\\'.join(fullPath.split('/')[:-1])
    return path


def getRelativePath(fullPath):
    if fullPath == '':
        return ''
    currentDir = os.getcwd().split('\\')
    filePath = fullPath.split('/')
    i = 0
    while i < len(currentDir) and i < len(filePath) and currentDir[i] == filePath[i]:
        i += 1
    relativePath = '/'*(len(currentDir) - i) + '/'.join(filePath[i:])
    return relativePath


def getModuleNameFromPath(fullPath):
    fileName = fullPath.split('/')[-1]
    moduleName = '.'.join(fileName.split('.')[:-1])
    return moduleName

# def restartFunction(filename):
#     module_function = filename.split('.')[0]
#     module, function = module_function.split('_', 1)
#     print ("Module: ", module," with function: " ,function)


def getNames(file_path):
    with open(file_path, 'r') as f:
        reader = csv.reader(f)
        row1 = next(reader)
        row1="".join(row1[0])
        module_function = row1.split(';')[:2]
        module_function = ';'.join(module_function)
        # print (module_function)
        return module_function

# def getPairs(pattern):
#     # Check if pattern contains optional count tuple
#     # if re.search(r'{\d+,\d+}', pattern):
#     #     # print("Pattern contains optional count tuple")
#     # else:
#         # print("Pattern does not contain optional count tuple")
    
#     # Extract values from pattern
#     values = re.findall(r'\d+', pattern)
#     # print("Values extracted from pattern:", values)

#     # Extract pairs from count tuple
#     pairs = re.findall(r'{(\d+),(\d+)}', pattern)
#     return pairs
    


def dumpAvg(folder_path,count,input,pid):
    if not os.path.exists(f"{folder_path}\\logs"):
        os.makedirs(f"{folder_path}\\logs")
    if re.search(r'{\d+,\d+}', input):
        pairs = re.findall(r'{(\d+),(\d+)}', input)
        left = [int(pair[0]) for pair in pairs]
        right = [int(pair[1]) for pair in pairs]
        # print("Left values:", left)
        # print("Right values:", right)
    for filename in os.listdir(folder_path):
        if filename.endswith('.csv'):
            module_function = filename.split('.')[0]
            module_function_json = getNames(os.path.join(folder_path, filename))
            module, function = module_function_json.split(';', 1)
            # print (module)
            # print (function)
            for json_file in os.listdir(folder_path):
                if json_file.endswith('.json') and json_file.startswith(module_function):
                    # act_inp = json_file.split('.')[0].split('_')[-1]
                    parts = json_file.split("_")
                    act_inp = int(parts[-1].split(".")[0])
                    # act_inp = json_file.rsplit('.', 1)[0].rsplit('_', 1)[-1]
                    act_inpv = int(act_inp)
                    # print("actual input values:", act_inpv)
                    with open(os.path.join(folder_path, json_file), 'r') as f:
                        # if f.readable() and f.read(1):
                        #     f.seek(0)
                        if f.readable() and f.seek(0, 2) > 0:
                            f.seek(0)
                            data = json.load(f)
                            total_val = 0
                            total_num = 0
                            for snapshot in data:
                                for consumer in snapshot['consumers']:
                                    if consumer['exe'] == 'C:\\Program Files\\erl-23.3.4.11\\bin\\erl.exe':
                                        if consumer['pid'] == pid:
                                            total_val+=consumer['consumption']
                                            total_num+=1
                            if total_num == 0:
                                # f1 = open(logFile, "a")
                                # f1.write("This file has no erl.exe: ",json_file)
                                # f1.close()
                                res_avg = 0
                                f = open(f"{folder_path}\\logs\\empty_logs.txt", "a")
                                f.write(f"This file has no erl.exe: {json_file}\n")
                                f.close()
                                print ("This file has no erl.exe: ",json_file)
                            else:
                                # res_avg=total_val/total_num/(1000000)
                                # res_avg=total_val/total_num
                                # print ("count", count)
                                if re.search(r'{\d+,\d+}', input):
                                    if act_inpv in left:
                                        count = right[left.index(act_inpv)]
                                        res_avg= total_val / count
                                else:
                                    res_avg= total_val / count                                
                            input_name = json_file.rsplit('_', 1)[1].split('.')[0]
                            row = [module, function,input_name,'msr','energy-cores',res_avg]
                            with open(os.path.join(folder_path, filename), 'r+') as f:
                                reader = csv.reader(f)
                                rows = list(reader)
                                new_row = ';'.join(row[:-1])
                                if new_row not in [r[0].rsplit(';', 1)[0] for r in rows]:
                                    row_str = ';'.join(map(str, row))
                                    f.write(row_str + '\n')
                                else: 
                                    continue
                        else:
                            f = open(f"{folder_path}\\logs\\empty_files.txt", "a")
                            f.write(f"{json_file} is empty\n")
                            f.close()
                            print(f"{json_file} is empty")

# def cleanCSV(file_csv):
#     with open(file_csv, 'r') as input_file:
#         reader = csv.reader(input_file)
#         rows = [row for row in reader if 'time;time' in row]

#     with open(file_csv, 'w') as output_file:
#         writer = csv.writer(output_file)
#         writer.writerows(rows)

def measure(measurement):
    compileTemplate = 'c("{0}"). '
    measureTemplate = '{erlangMeasureModule}:measure({{{moduleName}, {functionsToMeasure}, {inputDescs}}},{numberOfMeasurements},"{resultFolder}"). '
    erlangCommand = ''
    # Compile measuring program
    erlangCommand += compileTemplate.format(measurement.erlangMeasureFile)
    # Compile measured program
    erlangCommand += compileTemplate.format(measurement.erlangFile)
    # Call measureFunctions
    erlangCommand += measureTemplate.format(erlangMeasureModule=measurement.erlangMeasureModule,
                                            moduleName=measurement.moduleName,
                                            functionsToMeasure=measurement.functionsToMeasure,
                                            inputDescs=measurement.inputDescs,
                                            numberOfMeasurements=measurement.numberOfMeasurements,
                                            resultFolder = measurement.resultPath)
    print(erlangCommand)
    # Create and run subprocess for Erlang
    erlangProc = subprocess.Popen(
        ['erl', '+P', '134217727'], stdin=subprocess.PIPE, stdout=sys.stdout)
    out, _ = erlangProc.communicate(input=erlangCommand.encode())
    erl_pid=erlangProc.pid
    print(erl_pid)
    dumpAvg(measurement.resultPath,measurement.numberOfMeasurements,measurement.inputDescs,erl_pid)
    # jsonFileTemplate = '{resultFolder}\\{moduleName}_{functionsToMeasure}'
    # jsonFile = jsonFileTemplate.format( resultFolder = measurement.resultPath,
    #                                     moduleName=measurement.moduleName,
    #                                     functionsToMeasure=measurement.functionsToMeasure)
    
    # print(out.decode())
    # print (jsonFile)


# def measure(measurement):
#     # hipeCompile = False
#     # compileTemplate = 'c("{0}", [native,{{hipe, {{verbose, true}}}}]). ' if hipeCompile else 'c("{0}"). '
#     compileTemplate = 'c("{0}"). '
#     measureTemplate = '{erlangMeasureModule}:measure({{{moduleName}, {functionsToMeasure}, {inputDescs}}},{numberOfMeasurements}). '
#     erlangCommand = ''
#     # Compile measuring program
#     erlangCommand += compileTemplate.format(measurement.erlangMeasureFile)
#     # Compile measured program
#     erlangCommand += compileTemplate.format(measurement.erlangFile)
#     # Call measureFunctions
#     erlangCommand += measureTemplate.format(erlangMeasureModule=measurement.erlangMeasureModule,
#                                             moduleName=measurement.moduleName,
#                                             functionsToMeasure=measurement.functionsToMeasure,
#                                             inputDescs=measurement.inputDescs,
#                                             numberOfMeasurements=measurement.numberOfMeasurements)
#     # Create and run subprocess for Erlang
#     echoErlCmdProc = subprocess.Popen(
#         ['echo', erlangCommand], stdout=subprocess.PIPE, shell=True)
#     erlangProc = subprocess.Popen(
#         ['erl', '+P', '134217727 '], stdin=echoErlCmdProc.stdout)
#     echoErlCmdProc.stdout.close()
#     erlangProc.communicate()


def main():
    gui = GUI()


if __name__ == '__main__':
    main()
