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
from tkinter import *
from tkinter import filedialog
from tkinter import ttk
import subprocess
import sys
import os
import vis
import time
import pdb

# if os.getuid() != 0:
#     print("\033[1m\033[91mERROR: YOU MUST BE ROOT TO RUN THIS PROGRAM\033[0m")
#     exit()

# Class for storing measurement data


class Measurement:
    def __init__(self):
        self.setDefaultValues()

    def setDefaultValues(self):
        self.erlangFile = ''
        self.raplFile = os.getcwd() + '/rapl-read.out'
        self.erlangMeasureFile = os.getcwd() + '/rapl_erlang/energy_consumption.erl'
        self.numberOfMeasurements = 10
        self.makeNeeded = True
        self.setupNeeded = True
        self.raplFileRelativePath = './rapl-read.out'
        self.erlangMeasureModule = 'energy_consumption'
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

        # Make
        r += 1
        self.makeLabel = Label(self.window,
                               justify='right',
                               anchor='e',
                               width=25,
                               text='Build measuring program')
        self.makeLabel.grid(column=0, row=r, sticky=E)
        self.makeButton = Button(self.window,
                                 text='Make',
                                 width=8,
                                 command=self.make)
        self.makeButton.grid(column=1, row=r)

        # Setup
        r += 1
        self.setupLabel = Label(self.window,
                                justify='right',
                                anchor='e',
                                width=25,
                                text='Setup measuring environment')
        self.setupLabel.grid(column=0, row=r, sticky=E)
        self.setupButton = Button(self.window,
                                  text='Setup',
                                  width=8,
                                  command=self.setup)
        self.setupButton.grid(column=1, row=r)
        self.setupErrorLabel = Label(self.window,
                                     justify='left',
                                     anchor='w',
                                     text='')
        self.setupErrorLabel.grid(column=2, row=r, sticky=W)
        if os.getpid() != 0:
            self.setupButton['state'] = 'disabled'
            self.setupErrorLabel.configure(
                text='You must be root to run setup', fg='red')

        # Input measuring program
        r += 1
        self.raplFileLabel = Label(self.window,
                                   justify='right',
                                   anchor='e',
                                   width=25,
                                   text='Rapl program to measure with')
        self.raplFileLabel.grid(column=0, row=r, sticky=E)
        self.raplFileButton = Button(self.window,
                                     text='Select file',
                                     width=8,
                                     command=self.getRaplFilePath)
        self.raplFileButton.grid(column=1, row=r)
        self.currentRaplFileLabel = Label(self.window,
                                          justify='left',
                                          text=getRelativePath(self.measurement.raplFile))
        self.currentRaplFileLabel.grid(column=2, row=r, sticky=W)

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
                                                   text=getRelativePath(self.measurement.erlangMeasureFile))
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

        # Visualize
        r += 1
        self.visualisationOptionLabel = Label(self.window,
                                              justify='right',
                                              anchor='e',
                                              width=25,
                                              text='Method:')
        self.visualisationOptionLabel.grid(column=0, row=r, sticky=E)

        self.methodOptions = ('msr', 'perf_event', 'sysfs')
        self.selectedOption = StringVar()
        self.selectedOption.set(self.methodOptions[0])
        self.visualisationOptionMenu = OptionMenu(
            self.window, self.selectedOption, *self.methodOptions)
        self.visualisationOptionMenu.grid(column=1, row=r, sticky=W)

        self.visualisationLogScaleLabel = Label(self.window,
                                                justify='right',
                                                anchor='w',
                                                width=25,
                                                text='LogScale:')
        self.visualisationLogScaleLabel.grid(column=2, row=r, sticky=W)
        self.logScaleIsChecked = BooleanVar(value=False)
        self.logScaleCheckBox = Checkbutton(
            self.window,
            variable=self.logScaleIsChecked, offvalue=False, onvalue=True)
        self.logScaleCheckBox.grid(column=2, row=r)

        r += 1
        self.visualisationDomainLabel = Label(self.window,
                                              justify='right',
                                              anchor='e',
                                              width=25,
                                              text='Domains:')
        self.visualisationDomainLabel.grid(
            column=0, row=r, sticky=E, rowspan=3)
        self.ramIsChecked = BooleanVar(value=False)
        self.coreIsChecked = BooleanVar(value=False)
        self.gpuIsChecked = BooleanVar(value=False)
        self.pkgIsChecked = BooleanVar(value=False)
        self.timeIsChecked = BooleanVar(value=True)
        self.allIsChecked = BooleanVar(value=True)
        self.ramCheckBox = Checkbutton(
            self.window, text="RAM",
            variable=self.ramIsChecked, offvalue=False, onvalue=True)
        self.ramCheckBox.grid(column=1, row=r, sticky=W)

        self.coreCheckBox = Checkbutton(
            self.window, text="core",
            variable=self.coreIsChecked, offvalue=False, onvalue=True)
        self.coreCheckBox.grid(column=2, row=r, sticky=W)
        r += 1
        self.gpuCheckBox = Checkbutton(
            self.window, text="gpu",
            variable=self.gpuIsChecked, offvalue=False, onvalue=True)
        self.gpuCheckBox.grid(column=1, row=r, sticky=W)

        self.pkgCheckBox = Checkbutton(
            self.window, text="pkg",
            variable=self.pkgIsChecked, offvalue=False, onvalue=True)
        self.pkgCheckBox.grid(column=2, row=r, sticky=W)
        r += 1
        self.timeCheckBox = Checkbutton(
            self.window, text="time",
            variable=self.timeIsChecked, offvalue=False, onvalue=True)
        self.timeCheckBox.grid(column=1, row=r, sticky=W)

        self.allCheckBox = Checkbutton(
            self.window, text="all",
            variable=self.allIsChecked, offvalue=False, onvalue=True)
        self.allCheckBox.grid(column=2, row=r, sticky=W)

        r += 1
        self.visualizeButton = Button(self.window,
                                      text='Visualize',
                                      command=self.visualize)
        self.visualizeButton.grid(column=0, row=r, sticky=W, padx=6, pady=4)
        r += 1
        self.quickvisButton = Button(self.window,
                                     text='QuickVis',
                                     command=self.quickVis)
        self.quickvisButton.grid(column=0, row=r, sticky=W, padx=6, pady=4)

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

    def getRaplFilePath(self):
        raplFile = filedialog.askopenfilename()
        if raplFile == ():
            return
        else:
            self.measurement.raplFile = raplFile
            self.measurement.raplFileRelativePath = getRelativePath(raplFile)
            if self.measurement.raplFileRelativePath[0] != '.':
                self.measurement.raplFileRelativePath = '.' + \
                    self.measurement.raplFileRelativePath
            self.currentRaplFileLabel.configure(text=getRelativePath(raplFile))

    def getErlangMeasureFilePath(self):
        erlangMeasureFile = filedialog.askopenfilename()
        if erlangMeasureFile == ():
            return
        else:
            self.measurement.erlangMeasureFile = erlangMeasureFile
            self.measurement.erlangMeasureModule = getModuleNameFromPath(
                erlangMeasureFile)
            self.currentRaplFileLabel.configure(
                text=getRelativePath(erlangMeasureFile))

    def make(self):
        self.measurement.makeNeeded = False
        make()

    def setup(self):
        if os.getuid() != 0:
            errorStr = "You must be root to run setup!"
            self.errorLabel.configure(text=errorStr, fg='red')
        else:
            self.measurement.setupNeeded = False
            setup()

    def populateAndValidateMeasurementData(self):
        if len(self.measurementList) > 0:
            self.measurement.makeNeeded = False
            self.measurement.setupNeeded = False

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
            self.measurement.erlangFile) + '/results/' + resultRelativePath
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
        self.currentRaplFileLabel.configure(
            text=getRelativePath(self.measurement.erlangMeasureFile))
        self.currentRaplFileLabel.configure(
            text=getRelativePath(self.measurement.raplFile))
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

    def visualize(self):
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
            self.measurement.erlangFile) + '/results/' + resultRelativePath
        if resultRelativePath == '':
            errorStr += 'No folder for results specified\n'
        else:
            if not os.path.exists(self.measurement.resultPath):
                errorStr += 'Results folder does not exist\n'

        if errorStr == '':
            self.errorLabel.configure(text='OK', fg='green')
        else:
            self.errorLabel.configure(text=errorStr, fg='red')
            return

        resultFiles = [fileName for fileName in os.listdir(
            self.measurement.resultPath) if fileName[-4:] == '.csv']
        if self.measurement.functionsToMeasure != 'all':
            filteredResults = []
            functionNames = [s.strip(
            ) + "_log.csv" for s in self.measurement.functionsToMeasure[1:-1].split(',')]
            for eachFunctionName in functionNames:
                for eachFileName in resultFiles:
                    if eachFunctionName in eachFileName:
                        filteredResults.append(eachFileName)
                        break
            resultFiles = filteredResults

        completeResultPaths = [self.measurement.resultPath +
                               '/' + resultFile for resultFile in resultFiles]
        visualizer = vis.MeasurementVisualizer()
        resultHandler = vis.ResultHandler()
        for filename in completeResultPaths:
            resultHandler.AppendFromCSV(filename)
        visualizer.AttachResultHandler(resultHandler)
        if self.ramIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.RAM, self.logScaleIsChecked.get())
        if self.coreIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.CORE, self.logScaleIsChecked.get())
        if self.gpuIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.GPU, self.logScaleIsChecked.get())
        if self.pkgIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.PKG, self.logScaleIsChecked.get())
        if self.timeIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.TIME, self.logScaleIsChecked.get())
        if self.allIsChecked.get():
            visualizer.DrawFigure(self.selectedOption.get(
            ), vis.Domain.ALL, self.logScaleIsChecked.get())

    def quickVis(self):
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
            self.measurement.erlangFile) + '/results/' + resultRelativePath
        if resultRelativePath == '':
            errorStr += 'No folder for results specified\n'
        else:
            if not os.path.exists(self.measurement.resultPath):
                errorStr += 'Results folder does not exist\n'

        if errorStr == '':
            self.errorLabel.configure(text='OK', fg='green')
        else:
            self.errorLabel.configure(text=errorStr, fg='red')
            return

        resultFiles = [fileName for fileName in os.listdir(
            self.measurement.resultPath) if fileName[-4:] == '.csv']
        if self.measurement.functionsToMeasure != 'all':
            filteredResults = []
            functionNames = [s.strip(
            ) + "_log.csv" for s in self.measurement.functionsToMeasure[1:-1].split(',')]
            for eachFunctionName in functionNames:
                for eachFileName in resultFiles:
                    if eachFunctionName in eachFileName:
                        filteredResults.append(eachFileName)
                        break
            resultFiles = filteredResults

        completeResultPaths = [self.measurement.resultPath +
                               '/' + resultFile for resultFile in resultFiles]
        visualizer = vis.MeasurementVisualizer()
        resultHandler = vis.ResultHandler()
        for filename in completeResultPaths:
            resultHandler.AppendFromCSV(filename)
        visualizer.AttachResultHandler(resultHandler)
        visualizer.DrawAllFigures(
            self.selectedOption.get(), self.logScaleIsChecked.get())


def removeFilenameFromPath(fullPath):
    path = '/'.join(fullPath.split('/')[:-1])
    return path


def getRelativePath(fullPath):
    if fullPath == '':
        return ''

    currentDir = os.getcwd().split('/')
    filePath = fullPath.split('/')
    i = 0
    while i < len(currentDir) and i < len(filePath) and currentDir[i] == filePath[i]:
        i += 1
    relativePath = '../'*(len(currentDir) - i) + '/'.join(filePath[i:])
    return relativePath


def getModuleNameFromPath(fullPath):
    fileName = fullPath.split('/')[-1]
    moduleName = '.'.join(fileName.split('.')[:-1])
    return moduleName


def setup():
    subprocess.call(['sudo', 'make', 'setup'])


def make():
    subprocess.call(['make'])


def measure(measurement):
    # Make if needed
    if measurement.makeNeeded:
        make()
        measurement.makeNeeded = False

    # Setup if needed
    if measurement.setupNeeded and os.getuid() == 0:
        setup()
        measurement.setupNeeded = False

    hipeCompile = False
    compileTemplate = 'c("{0}", [native,{{hipe, {{verbose, true}}}}]). ' if hipeCompile else 'c("{0}"). '
    measureTemplate = '{erlangMeasureModule}:measure("{raplFile}",{{{moduleName}, {functionsToMeasure}, {inputDescs}}},{numberOfMeasurements},"{resultFolder}"). '
    erlangCommand = ''
    # Compile measuring program
    erlangCommand += compileTemplate.format(measurement.erlangMeasureFile)
    # Compile measured program
    erlangCommand += compileTemplate.format(measurement.erlangFile)
    # Call measureFunctions
    erlangCommand += measureTemplate.format(erlangMeasureModule=measurement.erlangMeasureModule,
                                            raplFile=measurement.raplFile,
                                            moduleName=measurement.moduleName,
                                            functionsToMeasure=measurement.functionsToMeasure,
                                            inputDescs=measurement.inputDescs,
                                            numberOfMeasurements=measurement.numberOfMeasurements,
                                            resultFolder=measurement.resultPath)
    # Create and run subprocess for Erlang
    echoErlCmdProc = subprocess.Popen(
        ['echo', erlangCommand], stdout=subprocess.PIPE)
    erlangProc = subprocess.Popen(
        ['erl', '+P', '134217727'], stdin=echoErlCmdProc.stdout)
    echoErlCmdProc.stdout.close()
    erlangProc.communicate()

    # Change owner and group for all created files
    user = subprocess.check_output("who").split()[0].decode("utf-8")
    subprocess.call(['chown', '-R', user, measurement.resultPath])
    subprocess.call(['chgrp', '-R', user, measurement.resultPath])


def main():
    gui = GUI()


if __name__ == '__main__':
    main()
