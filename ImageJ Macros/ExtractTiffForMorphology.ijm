//This macro extracts and merges desired channels from a composite tiff
//Written by Victor Kumbol, August 2021


setBatchMode(true);

//Select source folder and retrieve file list 
userDir = getDirectory("Choose Source Folder");

//Create an output subfolder
outputDir = userDir + "Morphology" + File.separator;
sourceDir = userDir + "TIFFs" + File.separator;
fileList = getFileList(sourceDir);


if (!File.exists(outputDir)) {
	File.makeDirectory(outputDir);
	print("");
	print("Output folder created: " + outputDir);
	print("");
}


//Extract NeuN+MAP2 Channels from tiff images
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tif files
		sourceFilePath = sourceDir + fileList[i];
		open(sourceFilePath);
		extractChannels(outputDir, getTitle(), File.nameWithoutExtension);	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}


print("Extraction Completed");
print("Total Files Converted: " + counter);







function extractChannels(outputDir, title, name) {

	//Extract the desired channels from image
	run("Split Channels");
	red_channel = "C1-" + title;
	green_channel = "C2-" + title;

	//process images
	/*
	selectWindow(red_channel);
	//run("Subtract Background...", "rolling=15 sliding");
	//run("Enhance Contrast", "saturated=0.35");
	run("Grays");
	run("Apply LUT");


	selectWindow(green_channel);
	run("Grays");
	//run("Enhance Contrast", "saturated=0.35");
	run("Apply LUT");
	
	*/
	//Merge the extracted channels and save as a tiff
	run("Merge Channels...", "c1=&red_channel c2=&green_channel ignore");
	run("8-bit");
	run("Apply LUT");
	saveAs("Tiff", outputDir + name + "_morph");
}


