*******************************
*******************************
*****ON CAMPUS HOUSING LOG*****
*******************************
*******************************

clear all
set more off
set excelxlsxlargefile on

global path = "/Users/matthew/Desktop/Research/On Campus Housing/Analysis - GitHub"
global raw = "$path/RawData"
global intermed = "$path/IntermediateData"
global output = "$path/Output"

do "$path/Code/On Campus Housing Regressions.do"
