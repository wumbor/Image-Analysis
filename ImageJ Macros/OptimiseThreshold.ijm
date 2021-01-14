//This macro runs the autothreshold function on a batch of images and returns the optimal threshold for nuclei count
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);


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
timeStampMacroPath = IJMacrosDir + "GetTimeStamp.ijm";
timeStamp = runMacro(timeStampMacroPath);


//Create the ThresholdAnalysis file
if (File.exists(ThresholdAnalysisFile)) {	////delete the existing file if one exists 
	if (File.delete(ThresholdAnalysisFile)) {
		print("\nPrevious thresholdAnalysis file deleted");
	}	
} 

f = File.open(ThresholdAnalysisFile); 
print("\nNew thresholdAnalysis file created: ");
print(ThresholdAnalysisFile);
print(f, "Nuclei Count Threshold Optimisation: " + experimentId);
print(f, timeStamp);	
print(f, "Filename, Lower Threshold ");	
File.close(f);



//Run the analyse threshold on all images in the selected folder
print("\nFinding the optimum threshold for analysis...");
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



//Run the DetermineOptimalThreshold.R script
/*
a = exec("cmd", "/c", "start", "cmd", "/k", 'Rscript "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/DetermineOptimalThreshold.R" "D:/OneDrive - Charité - Universitätsmedizin Berlin/My PhD Project/mMORPH/R Scripts/2020_08_3_VK"'); 
print(a);
*/

callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "DetermineOptimalThreshold.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript; //combine macro arguments into one string
//runMacro(callRScriptMacroPath, CallRScriptArgs);	
optimisedThreshold = runMacro(callRScriptMacroPath, CallRScriptArgs);	
print("Optimum threshold determined");
return optimisedThreshold;
//print(optimisedThreshold);






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
}



