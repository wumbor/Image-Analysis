//This macro measures the fluorescence intensity of images and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, January 2021
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
print("\\Clear");
print("MEASURE FLUORESCENCE MACRO");

//INITIALISE KEY ANALYSIS PARAMETERS
//Initialize global variables
var IJMacrosDir;
var RScriptsDir;
var channelOfInterest;
var fluoParameter;
var measureFluoresence;
var plotData;


//Request working directory from user
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format
experimentId = split(workingDir, "/"); 
experimentId = experimentId[(lengthOf(experimentId)-1)]; //Extract the experimentId from the directory path
sourceImagesDir = workingDir + "TIFFs/"; //define source images directory


//Load key parameters from file
fluoIntensityParametersFile = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/MeasureFluorescenceParameters.txt";
loadAllParameters(fluoIntensityParametersFile);



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



//If optimiseThreshold is toggled on, call the optimise threshold macro 
optimiseThreshold = false;
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
	Threshold = AutoThreshold; //change the NucleiThreshold to the one calculated automatically
	File.append("AutoThreshold, TRUE", analysisLogFile);
} else {
	File.append("AutoThreshold, FALSE", analysisLogFile);
	Threshold = 150; 
}




//Call the MeasureFluorescence macro and pass the required parameters as arguments
//MeasureFluorescenceMacroPath = IJMacrosDir + "MeasureFluorescence.ijm"; //specify path to macro 
MeasureFluorescenceMacroPath = IJMacrosDir + "MeasureThresholdedFluorescence2.ijm"; //specify path to macro 
MeasureFluorescenceArgs = workingDir + "&&" + channelOfInterest + "&&" + experimentId+ "&&" + Threshold; //combine macro arguments into one string


if (measureFluoresence) { //perform a nuclei count if measureFluoresence is set to true
		
	feedback = runMacro(MeasureFluorescenceMacroPath, MeasureFluorescenceArgs);
	if (feedback != "true") { //exit the macro if the nuclei count was not performed
		exit("Measure Fluorescence Master macro terminated");
	}
	File.append("Measure Fluorescence Macro, MeasureFluorescence.ijm", analysisLogFile);	
	File.append("Image Channel, " + channelOfInterest, analysisLogFile);
	File.append("Parameter Analyzed, " + fluoParameter, analysisLogFile);
}



//Run the SummariseNucleiCount R script to summarise the data and plot graphs
callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "SummariseFluoIntensity.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript; //combine macro arguments into one string

fluoIntensityResultsFile = workingDir + experimentId +  "_Fluo_Intensity.csv";
imageSequenceFile = workingDir + experimentId + "_Image_Capture.txt";


if (plotData){
		File.append("Analysis Script, SummariseFluoIntensity.R", analysisLogFile);
		if (!File.exists(fluoIntensityResultsFile)) { //check to ensure a results file exists
			print(" ");
			print("Error: Nuclei Count Results file not found");
			print(fluoIntensityResultsFile);
			exit("Nuclei Count Results file not found");
		}

		if (!File.exists(imageSequenceFile)) { //check to ensure an image sequence file is present
			print(" ");
			print("Error: Image Sequence file not found");
			print(imageSequenceFile);
			exit("Image Sequence file not found");
		}
		
		print("\nSummarising and plotting data...");
		runMacro(callRScriptMacroPath, CallRScriptArgs);
		print("Data Summary and Plot Complete");
			
}


garbage = File.delete(analysisLogFile); //delete the analysis log file
garbage = File.delete(fluoIntensityResultsFile); //delete the preliminary nuclei count results file

close("Results");
RplotsFile = workingDir + "Rplots.pdf";
if (File.exists(RplotsFile)) {
	garbage = File.delete(RplotsFile); // delete the annoying Rplots file
} 


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
		if (parametersFileLines[i].contains("channelOfInterest")) {
			channelOfInterest = retrieveParameter(parametersFileLines[i]); //Retrieve the channelOfInterest
		} else if (parametersFileLines[i].contains("fluoParameter")) {
			fluoParameter = retrieveParameter(parametersFileLines[i]); //Retrieve the fluoParameter option
		} else if (parametersFileLines[i].contains("measureFluoresence?")) {
			measureFluoresence = retrieveParameter(parametersFileLines[i]); //Retrieve the measureFluoresence option
		} else if (parametersFileLines[i].contains("plotData?")) {
			plotData = retrieveParameter(parametersFileLines[i]); //Retrieve the plotData option
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
		return toString(parameterVal);
	}	
}


