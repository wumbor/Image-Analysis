//This macro performs a nuclei count and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
print("\\Clear");

//Initialize global variables
var IJMacrosDir;
var RScriptsDir;
var NucleiThreshold;
var countNuclei;
var saveImages;
var plotData;

//Request working directory from user
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format

//Load key parameters from file
nucleiCountParametersFile = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/NucleiCountParameters.txt";
loadAllParameters(nucleiCountParametersFile);

//Initialise AnalysisLog file
analysisLogFile = workingDir + "AnalysisLog.txt";
timeStampMacroPath = IJMacrosDir + "GetTimeStamp.ijm";
timeStamp = runMacro(timeStampMacroPath);

if (!File.exists(analysisLogFile)) {
	f = File.open(analysisLogFile); //Create a log file if none already exists
	print(f, "NUCLEI COUNT PARAMETERS");
	print(f, timeStamp);
	File.close(f);
} else {
	File.append("", analysisLogFile);
	File.append("NUCLEI COUNT PARAMETERS", analysisLogFile); //Write to an existing file if it already exists
	File.append(timeStamp, analysisLogFile);
}

//Extract the experimentId from the directory path
experimentId = split(workingDir, "/");
experimentId = experimentId[(lengthOf(experimentId)-1)];


//Call the appropriate Nuclei Count macro and pass the working directory, nuclei threshold and experimentId as arguments
CountWithImgSaveMacroPath = IJMacrosDir + "CountNucleiWithImageSave.ijm"; //specify path to macro for nuclei count with image saving on
CountWithoutImgSaveMacroPath = IJMacrosDir + "CountNucleiWithoutImageSave.ijm"; //specify path to macro for nuclei count with image saving off
NucleiCountArgs = workingDir + "&&" + NucleiThreshold + "&&" + experimentId; //combine macro arguments into one string

sourceImagesDir = workingDir + "TIFFs/";

if (countNuclei) { //perform a nuclei count if countNuclei is set to true

	if (!File.exists(sourceImagesDir)) {
		print(" ");
		print("Error: TIFF images folder not found");
		print(sourceImagesDir);
		exit("TIFF images folder not found");
	}

	if (saveImages){
			File.append("Nuclei Count Macro: CountNucleiWithImageSave.ijm", analysisLogFile);
			runMacro(CountWithImgSaveMacroPath, NucleiCountArgs);
		} else {
			File.append("Nuclei Count Macro: CountNucleiWithoutImageSave.ijm", analysisLogFile);
			runMacro(CountWithoutImgSaveMacroPath, NucleiCountArgs);
		}
	File.append("Nuclei Count Threshold: " + NucleiThreshold, analysisLogFile);
}



//Run the SummariseNucleiCount R script to summarise the data and plot graphs
callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "SummariseNucleiCount.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript + "&&" + experimentId; //combine macro arguments into one string

nucleiCountResultsFile = workingDir + experimentId +  "_Nuclei_Count.csv";
imageSequenceFile = workingDir + experimentId + "_Image_Capture.txt";


if (plotData){
	
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
		File.append("Analysis Script: SummariseNucleiCount.R", analysisLogFile);
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
		if (parametersFileLines[i].contains("NucleiThreshold")) {
			NucleiThreshold = parseInt(retrieveParameter(parametersFileLines[i])); //Retrieve the NucleiThreshold
		} else if (parametersFileLines[i].contains("countNuclei")) {
			countNuclei = retrieveParameter(parametersFileLines[i]); //Retrieve the countNuclei option
		} else if (parametersFileLines[i].contains("saveImages")) {
			saveImages = retrieveParameter(parametersFileLines[i]); //Retrieve the saveImages option
		} else if (parametersFileLines[i].contains("plotData")) {
			plotData = retrieveParameter(parametersFileLines[i]); //Retrieve the saveImages option
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
