
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Neurphology macro
////
//// This macro quantifies neurite length, neurite area, neurite endpoints, neurite attachment points, and soma size
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Created by Eric Huang, June 2011 https://hwangeric5.wixsite.com/erichwanglab/neurphologyj
//Modified by Victor Kumbol, August 2021


requires("1.42k");
setBatchMode(true);
run("Set Measurements...", "area mean standard modal shape limit redirect=None decimal=3");
setForegroundColor(0, 0, 0);

//Initialize global variables
var IJMacrosDir;
var RScriptsDir;
var NucleiThreshold;
var countNuclei;
var saveImages;
var plotData;
var optimiseThreshold;
var inVivo;
lowc = 2; 		//contrast level
lowi = 10;		//soma intensity
nwidth = 5;		//width of neurites in pixel
cleanup = 15;	//particle cleanup value

	
//Select source folder and retrieve file list 
userDir = getDirectory("Choose Source Folder");
workingDir = userDir.replace("\\", "/"); //convert directory path to universal format
experimentId = split(workingDir, "/"); 
experimentId = experimentId[(lengthOf(experimentId)-1)]; //Extract the experimentId from the directory path

//Create an output subfolder
outputDir = userDir + "Morphology Analysis" + File.separator;
sourceDir = userDir + "Morphology" + File.separator;
fileList = getFileList(sourceDir);

if (!File.exists(outputDir)) {
	File.makeDirectory(outputDir);
	print("");
	print("Output folder created: " + outputDir);
	print("");
}

//Initialise Results file
resultsFile = userDir + "MorphologyAnalysis.csv";
if (File.exists(resultsFile)) {
	garbage = File.delete(resultsFile); // delete any existing results file
} 

f = File.open(resultsFile); //Create a new results file
print(f, "Filename, SomaCount, TotalSomaArea, AverageSomaArea, NeuriteCount, TotalNeuriteArea, TotalNeuriteLength, AttachmentPointsCount, EndPointsCount");
File.close(f);



//Load key parameters from file
nucleiCountParametersFile = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/NucleiCountParameters.txt";
loadAllParameters(nucleiCountParametersFile);


//Initialise AnalysisLog file
analysisLogFile = userDir + "AnalysisLog.txt";
timeStampMacroPath = IJMacrosDir + "GetTimeStamp.ijm";
timeStamp = runMacro(timeStampMacroPath);

if (File.exists(analysisLogFile)) {
	garbage = File.delete(analysisLogFile); // delete any existing logfile
} 

f = File.open(analysisLogFile); //Create a new logfile 
print(f, "Parameter, Value");
print(f, "Analysis Date, " + timeStamp);
File.close(f);










//Perform morphology analysis on images in folder
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tif files
		sourceFilePath = sourceDir + fileList[i];
		open(sourceFilePath);
		analyseMorphology(outputDir, getTitle(), File.nameWithoutExtension, lowc, lowi, nwidth, cleanup);	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}

//File.append("Analysis Script, " + RScript, analysisLogFile);
File.append("Morphology Analysis Macro, NeurphologyJHT.ijm", analysisLogFile);
File.append("Contrast Level, " + lowc, analysisLogFile);
File.append("Soma Intensity, " + lowi, analysisLogFile);
File.append("Neurite Width, " + nwidth, analysisLogFile);
File.append("Particle Cleanup Value, " + cleanup, analysisLogFile);
print("Morphology Analysis Completed");
print("Total Images Analysed: " + counter);









function analyseMorphology(outputDir, title, name, lowc, lowi, nwidth, cleanup) {
	
	//showMessage("select image","select the image you want to analyze (16 bit or less)");
	//open();
	run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
	rename("original");
	run("Duplicate...", "title=flatten");
	run("Subtract Background...", "rolling=50");
	
	if(bitDepth()!=16)
	{
	 run("16-bit");
	}
	else
	
	////Background cleanup process
	
	////detecting low contrast pixels
	selectWindow("original");
	run("Duplicate...", "title=blurred");
	selectWindow("blurred");
	run("Gaussian Blur...", "sigma=1");
	imageCalculator("Subtract create", "original","blurred");
	rename("sub_blurred");
	selectWindow("blurred");
	close();
	
	////create a mask for low contrast pixels
	selectWindow("sub_blurred");
	setThreshold(0, lowc);
	run("Create Mask");
	rename("low_contrast");
	
	////create a mask for low intensity pixels
	selectWindow("flatten");
	setThreshold(0, lowi);
	run("Create Mask");
	rename("low_intensity");
	run("Clear Results");
	selectWindow("flatten");
	close();
	
	////create a mask to remove low intensity and low contrast pixels
	imageCalculator("AND create", "low_contrast","low_intensity");
	rename("zero_intensity");
	run("Invert");
	run("16-bit");
	run("Subtract...", "value=254");
	
	selectWindow("low_intensity");
	close();
	selectWindow("low_contrast");
	close();
	selectWindow("original");
	resetThreshold();
	imageCalculator("Multiply create", "original","zero_intensity");
	rename("new_original");
	
	////soma detection
	selectWindow("new_original");
	run("Duplicate...", "title=open");
	run("Minimum...", "radius="+nwidth+"");
	run("Maximum...", "radius="+nwidth+"");
	//setAutoThreshold("Mean dark");
	setThreshold(lowc, 65535);
	run("Create Mask");
	rename("soma");
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	
	////image clean up (remove small particles and debris) and soma removal
	selectWindow("new_original");
	run("Duplicate...", "title=neuritesoma");
	//setAutoThreshold("Mean dark");
	setThreshold(lowc, 65535);
	run("Create Mask");
	rename("neuritesoma_mask");
	setBackgroundColor(0, 0, 0);
	setAutoThreshold();
	run("Particle Remover", "size=0-"+cleanup+" circularity=0.00-1.00 show=Nothing");
	rename("neuritesoma_clean");
	run("Duplicate...", "title=neurite");
	selectWindow("soma");
	run("Create Selection");
	selectWindow("neurite");
	run("Restore Selection");
	run("Clear");
	run("Select None");
	
	////neurite detection
	selectWindow("neurite");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	
	////neurite length detection
	selectWindow("neuritesoma_clean");
	run("Duplicate...", "title=neuritesoma_skeleton");
	run("Skeletonize");
	imageCalculator("Subtract create", "neuritesoma_skeleton","soma");
	rename("neurite_length");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	selectWindow("neuritesoma_clean");
	close();
	
	////attachment point detection
	selectWindow("soma");
	run("Select None");
	run("Duplicate...", "title=soma_dilate");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	imageCalculator("AND create", "soma_dilate","neuritesoma_skeleton");
	rename("stem");
	run("Duplicate...", "title=stem_erode");
	run("Options...", "iterations=1 black count=7");
	run("Erode");
	imageCalculator("Subtract create", "stem","stem_erode");
	rename("stem_points");
	imageCalculator("Subtract create", "stem_points","soma");
	rename("attachment_points");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	selectWindow("stem");
	close();
	selectWindow("stem_erode");
	close();
	selectWindow("neuritesoma_skeleton");
	close();
	selectWindow("stem_points");
	close();
	
	////endpoint detection
	selectWindow("neurite_length");
	run("Duplicate...", "title=neurite_erode");
	run("Options...", "iterations=1 black count=7");
	run("Erode");
	imageCalculator("Subtract create", "neurite_length","neurite_erode");
	rename("tip");
	imageCalculator("Subtract create", "tip","soma_dilate");
	rename("endpoints");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	selectWindow("neurite_erode");
	close();
	selectWindow("tip");
	close();
	selectWindow("soma_dilate");
	close();
	
	////create and save color-combined image for user evaluation
	selectWindow("soma");
	run("Select None");
	selectWindow("original");
	run("8-bit");
	run("Merge Channels...", "red=neurite_length green=attachment_points blue=soma gray=original create keep");
	rename("red=neurite green=attachment-points blue=soma gray=original");
	saveAs("Tiff", outputDir + name + "_analysed");


	//save the analysis results to a csv file
	selectWindow("Summary"); 
	filename = title;
	somaCount = Table.get("Count", 0);
	totalSomaArea = Table.get("Total Area", 0);
	averageSomaArea = Table.get("Average Size", 0);
	neuriteCount = Table.get("Count", 1);
	totalNeuriteArea = Table.get("Total Area", 1);
	totalNeuriteLength = Table.get("Total Area", 2);
	attachmentPointsCount = Table.get("Count", 3);
	endPointsCount = Table.get("Count", 4);
	
	File.append(filename +","+ somaCount +","+ totalSomaArea +","+ averageSomaArea +","+ neuriteCount +","+ totalNeuriteArea +","+ totalNeuriteLength +","+ attachmentPointsCount +","+ endPointsCount, resultsFile);
	
	////close all windows that are not needed anymore
	run("Close");

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
		} else if (parametersFileLines[i].contains("countNuclei?")) {
			countNuclei = retrieveParameter(parametersFileLines[i]); //Retrieve the countNuclei option
		} else if (parametersFileLines[i].contains("saveImages?")) {
			saveImages = retrieveParameter(parametersFileLines[i]); //Retrieve the saveImages option
		} else if (parametersFileLines[i].contains("plotData?")) {
			plotData = retrieveParameter(parametersFileLines[i]); //Retrieve the plotData option
		} else if (parametersFileLines[i].contains("optimiseThreshold?")) {
			optimiseThreshold = retrieveParameter(parametersFileLines[i]); //Retrieve the optimiseThreshold option
		}else if (parametersFileLines[i].contains("inVivo?")) {
			inVivo = retrieveParameter(parametersFileLines[i]); //Retrieve the inVivo option
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
