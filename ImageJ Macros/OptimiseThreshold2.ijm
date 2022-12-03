//This macro runs the autothreshold function on a batch of images and returns the optimal threshold for nuclei count
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, November 2021

setBatchMode(true);
run("Clear Results");
//Retrieve macro arguments
args = getArgument();
delimiter = "&&";
args=split(args, delimiter);
workingDirectory = args[0];
channelOfInterest = args[1];


//Define key variables
sourceImagesDir = workingDirectory + File.separator + "TIFFs" + File.separator;
ThresholdSelectionFile = workingDirectory  + File.separator + "ThresholdSelection.csv";
ThresholdAnalysisFile = workingDirectory  + File.separator + "ThresholdAnalysis.csv";

fileList = File.openAsString(ThresholdSelectionFile);
fileList = split(fileList,"\n"); 


//Prepare the ThresholdAnalysis file 
garbage = File.delete(ThresholdAnalysisFile);	////delete any existing Threshold analysis file
f = File.open(ThresholdAnalysisFile); 
print(f, "Filename, Lower Threshold ");	
File.close(f);



//Run the analyse threshold on all images in the selected folder
print("\n\nFinding the optimum threshold for nuclei count...");
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tiff images
		filePath = sourceImagesDir + fileList[i];
		open(filePath);
		if (is("composite")) { //process only composite images
			analyseThreshold(getTitle(), File.nameWithoutExtension, channelOfInterest, ThresholdAnalysisFile);
		counter ++;
		}
	}
    close("*");  	
}
print("Threshold analysis complete");





function analyseThreshold(title, name, channel, outputfile) { 
	//Extract the appropriate channel from image
	run("Split Channels");
	selectWindow(channel + title);
	//run("8-bit");
	close("\\Others");
	
	//Write the lower threshold to file
	setAutoThreshold("Otsu dark");
	getThreshold(lowerThreshold, upperThreshold);
	File.append(name + ", " + lowerThreshold , outputfile);
	resetThreshold;
	close("*");
}



