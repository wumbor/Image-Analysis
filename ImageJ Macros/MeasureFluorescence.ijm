//This macro measures the average fluorescence intensity in a composite image
//Written by Victor Kumbol, September 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
run("Clear Results");
run("Set Measurements...", "area mean modal min median display redirect=None decimal=3");

//Retrieve parameters from parent macro
args = getArgument();
args = split(args, "&&");
workingDir = args[0] 
channelOfInterest = args[1];
experimentId = args[2];


//Define key variables
sourceImagesDir = workingDir + "TIFFs/";
resultsFile = workingDir + experimentId + "_Fluo_Intensity.csv";
fileList = getFileList(sourceImagesDir);

/*
 FOR TROUBLESHOOTING
//Select source folder and retrieve file list 
sourceDir = getDirectory("Choose Source Folder");
sourceDir = sourceDir.replace("\\", "/"); //convert directory path to universal format
fileList = getFileList(sourceDir);
channelOfInterest = "C1";
experimentId = "2021_01_5_VK";
resultsFile = sourceDir + experimentId + "_Fluo_Intensity.csv";
*/

//Run the measure fluorescence function on all images in the selected folder
print("\nMeasuring Fluorescence Intensity...");
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tiff images
		filePath = sourceImagesDir + fileList[i];
		open(filePath);
		if (is("composite")) { //process only composite images
			measureFluoIntensity(getTitle(), channelOfInterest);
		counter ++;
		}
	}
    close("*");
   	showProgress(i, fileList.length);	
}

print("Fluoresence Measurement Complete");
print("Total Files Processed: " + counter);

//selectWindow("Summary"); 
saveAs("Results", resultsFile);
return "true";



function measureFluoIntensity(title, channel) { 
	
	//Extract a channel from image
	run("Split Channels");
	selectWindow(channel + "-" + title);
	//run("8-bit");
	close("\\Others");
	
	//Measure Fluorescence Intensity
	run("Measure");
	close("*");
}


