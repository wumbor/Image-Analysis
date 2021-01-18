//This macro performs a nuclei count and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
print("\\Clear");
print("COUNT NUCLEI MACRO");

//INITIALISE KEY ANALYSIS PARAMETERS
//Initialize global variables
var IJMacrosDir;
var RScriptsDir;
var NucleiThreshold;
var countNuclei;
var saveImages;
var plotData;
var optimiseThreshold;


//Request working directory from user
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format
experimentId = split(workingDir, "/"); 
experimentId = experimentId[(lengthOf(experimentId)-1)]; //Extract the experimentId from the directory path
sourceImagesDir = workingDir + "TIFFs/"; //define source images directory



//Load key parameters from file
nucleiCountParametersFile = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/NucleiCountParameters.txt";
loadAllParameters(nucleiCountParametersFile);



//Initialise AnalysisLog file
analysisLogFile = workingDir +  experimentId + "_AnalysisLog.txt";
timeStampMacroPath = IJMacrosDir + "GetTimeStamp.ijm";
timeStamp = runMacro(timeStampMacroPath);

if (File.exists(analysisLogFile)) {
	garbage = File.delete(analysisLogFile); // delete any existing logfile
} 

f = File.open(analysisLogFile); //Create a new logfile 
print(f, "Parameter, Value");
print(f, "Analysis Date, " + timeStamp);
File.close(f);



//ERROR CHECK


//If optimiseThreshold is toggled on, call the optimise threshold macro 
OptimiseThresholdMacroPath = IJMacrosDir + "OptimiseThreshold.ijm"; //specify path to macro 
OptimiseThresholdArgs = workingDir + "&&" + RScriptsDir + "&&" + IJMacrosDir; //combine required macro arguments into one string

if (optimiseThreshold) {

		//Throw an error if the TIFF images folder cannot be found
	if (!File.exists(sourceImagesDir)) { 
		print(" ");
		print("Error: TIFF images folder not found");
		print(sourceImagesDir);
		exit("TIFF images folder not found");
	}

	AutoThreshold = runMacro(OptimiseThresholdMacroPath, OptimiseThresholdArgs);
	AutoThreshold = parseInt(AutoThreshold);
	NucleiThreshold = AutoThreshold; //change the NucleiThreshold to the one calculated automatically
	File.append("AutoThreshold, TRUE", analysisLogFile);
} else {
	File.append("AutoThreshold, FALSE", analysisLogFile);
}




//Call the appropriate Nuclei Count macro and pass the working directory, nuclei threshold and experimentId as arguments
CountWithImgSaveMacroPath = IJMacrosDir + "CountNucleiWithImageSave.ijm"; //specify path to macro for nuclei count with image saving on
CountWithoutImgSaveMacroPath = IJMacrosDir + "CountNucleiWithoutImageSave.ijm"; //specify path to macro for nuclei count with image saving off
NucleiCountArgs = workingDir + "&&" + NucleiThreshold + "&&" + experimentId; //combine macro arguments into one string

//sourceImagesDir = workingDir + "TIFFs/";
if (countNuclei) { //perform a nuclei count if countNuclei is set to true
	if (saveImages){//use the macro which saves images 
			feedback = runMacro(CountWithImgSaveMacroPath, NucleiCountArgs);
			if (feedback != "true") { //exit the macro if the nuclei count was not performed
				exit("Count Nuclei Master macro terminated");
			}
			File.append("Nuclei Count Macro, CountNucleiWithImageSave.ijm", analysisLogFile);
		} else {//use the macro which does not save images
			feedback = runMacro(CountWithoutImgSaveMacroPath, NucleiCountArgs);
			if (feedback != "true") { //exit the macro if the nuclei count was not performed
				exit("Count Nuclei Master macro terminated");
			}
			File.append("Nuclei Count Macro, CountNucleiWithoutImageSave.ijm", analysisLogFile);
		}
	File.append("Nuclei Count Threshold, " + NucleiThreshold, analysisLogFile);
}



//Run the SummariseNucleiCount R script to summarise the data and plot graphs
callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "SummariseNucleiCount.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript; //combine macro arguments into one string

nucleiCountResultsFile = workingDir + experimentId +  "_Nuclei_Count.csv";
imageSequenceFile = workingDir + experimentId + "_Image_Capture.txt";


if (plotData){
		File.append("Analysis Script, SummariseNucleiCount.R", analysisLogFile);
		if (!File.exists(nucleiCountResultsFile)) { //check to ensure a nuclei count results file is present
			print(" ");
			print("Error: Nuclei Count Results file not found");
			print(nucleiCountResultsFile);
			exit("Nuclei Count Results file not found");
		}

		if (!File.exists(imageSequenceFile)) { //check to ensure an image sequence file is present
			print(" ");
			print("Error: Image Sequence file not found");
			print(imageSequenceFile);
			exit("Image Sequence file not found");
		}
		
		print(" ");
		print("Summarising and plotting data...");
		runMacro(callRScriptMacroPath, CallRScriptArgs);			
		print("Data Summary and Plot Complete");
}


garbage = File.delete(analysisLogFile); //delete the analysis log file
garbage = File.delete(nucleiCountResultsFile); //delete the preliminary nuclei count results file


//Functions used in this macro
function loadAllParameters(parametersFilePath) { 
// This function loads all parameters from a given parameters txt file

	parametersFile = File.openAsString(parametersFilePath);
	parametersFileLines = split(parametersFile, "\n");
	
	for (i = 0; i < parametersFileLines.length; i++){
	// Load directory paths
		if (parametersFileLines[i].contains("IJMacrosDir")) { 
			IJMacrosDir = retrieveParameter(parametersFileLines[i]); //Retrieve the IJMacrosDir 
		} else if (parametersFileLines[i].contains("RScriptsDir")) { 
			RScriptsDir = retrieveParameter(parametersFileLines[i]); //Retrieve the RScriptsDir 
		} 
	
	//Load nuclei count parameters
		if (parametersFileLines[i].contains("NucleiThreshold")) {
			NucleiThreshold = parseInt(retrieveParameter(parametersFileLines[i])); //Retrieve the NucleiThreshold
		} else if (parametersFileLines[i].contains("countNuclei?")) {
			countNuclei = retrieveParameter(parametersFileLines[i]); //Retrieve the countNuclei option
		} else if (parametersFileLines[i].contains("saveImages?")) {
			saveImages = retrieveParameter(parametersFileLines[i]); //Retrieve the saveImages option
		} else if (parametersFileLines[i].contains("plotData?")) {
			plotData = retrieveParameter(parametersFileLines[i]); //Retrieve the plotData option
		} else if (parametersFileLines[i].contains("optimiseThreshold?")) {
			optimiseThreshold = retrieveParameter(parametersFileLines[i]); //Retrieve the optimiseThreshold option
		}
		}
}
	
	
	
	
function retrieveParameter(parameterString) {
	//This function returns the value assigned to a parameter as a string
	parameterString = split(parameterString, "=");
	parameterVal = parameterString[1].trim();
	parameterVal = parameterVal.replace('"', "");
	
	if ((parameterVal.contains("\\"))||(parameterVal.contains("/"))) { //convert any directory paths to universal format
		parameterVal = parameterVal.replace("\\", "/"); 
		return parameterVal;
	} else if (parameterVal.toLowerCase =="true") {
		return true
	} else if (parameterVal.toLowerCase =="false") {
		return false
	} else {
		return parameterVal
	}	
}
