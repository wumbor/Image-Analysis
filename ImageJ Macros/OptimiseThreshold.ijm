//This macro runs the autothreshold function on a batch of images and returns the optimal threshold for nuclei count
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);

run("Clear Results");
//Retrieve parameters from parent macro
args = getArgument();
args = split(args, "&&");
workingDir = args[0]; 
RScriptsDir = args[1];
IJMacrosDir = args[2];

experimentId = split(workingDir, "/"); 
experimentId = experimentId[(lengthOf(experimentId)-1)]; //Extract the experimentId from the directory path

//Define key variables
sourceImagesDir = workingDir + "TIFFs/";
fileList = getFileList(sourceImagesDir);
ThresholdAnalysisFile = workingDir + experimentId + "_Threshold_Analysis.csv";


//Prepare the ThresholdAnalysis file
if (File.exists(ThresholdAnalysisFile)) {	////delete any existing Threshold analysis file 
	garbage = File.delete(ThresholdAnalysisFile);	
} 
f = File.open(ThresholdAnalysisFile); 
print(f, "Nuclei Count Threshold Optimisation: " + experimentId);
print(f, "Filename, Lower Threshold ");	
File.close(f);



//Run the analyse threshold on all images in the selected folder
print("\nFinding the optimum threshold for nuclei count...");
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tiff images
		filePath = sourceImagesDir + fileList[i];
		open(filePath);
		if (is("composite")) { //process only composite images
			analyseThreshold(getTitle(), File.nameWithoutExtension, ThresholdAnalysisFile);
		counter ++;
		}
	}
    close("*");
   	showProgress(i, fileList.length);	  	
}




callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "DetermineOptimalThreshold.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript; //combine macro arguments into one string
//runMacro(callRScriptMacroPath, CallRScriptArgs);	
optimisedThreshold = runMacro(callRScriptMacroPath, CallRScriptArgs);	
print("Optimum threshold determined");
//garbage = File.delete(ThresholdAnalysisFile); //delete the threshold analysis file
return optimisedThreshold;






function analyseThreshold(title, name, outputfile) { 





	//Extract the red channel from image
	run("Split Channels");
	selectWindow("C1-" + title);
	run("8-bit");
	close("\\Others");
	
	//Write the lower threshold to file
	setAutoThreshold("Otsu dark");
	getThreshold(lowerThreshold, upperThreshold);
	File.append(name + ", " + lowerThreshold , outputfile);
	resetThreshold;
	close("*");

	/*
	//Extract the red channel from image
	run("Split Channels");
	selectWindow("C1-" + title);
	run("8-bit");
	//run("Subtract Background...", "rolling=50 sliding");
	close("\\Others");
	
	//Write the lower threshold to file
	setAutoThreshold("Otsu dark");
	//setAutoThreshold("Triangle dark");
	getThreshold(lowerThreshold, upperThreshold);
	File.append(name + ", " + lowerThreshold , outputfile);
	resetThreshold;
	close("*");
	*/
}



