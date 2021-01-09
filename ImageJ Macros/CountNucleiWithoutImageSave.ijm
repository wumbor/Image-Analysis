//This macro counts the number of nuclei in images and saves the processed images
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
run("Clear Results");

//Retrieve parameters from parent macro
args = getArgument();
args = split(args, "&&");
workingDir = args[0] 
nucleiThreshold = parseInt(args[1]);
experimentId = args[2];

//Define key variables
sourceImagesDir = workingDir + "TIFFs/";
resultsFile = workingDir + experimentId + "_Nuclei_Count.csv";
fileList = getFileList(sourceImagesDir);


//Return an error if the source folder does not exist
if (!File.exists(sourceImagesDir)) {
	print("Error: TIFF images folder not found");
	print(sourceImagesDir);
	return "failed";
	exit("TIFF images folder not found");
}


//Run the nuclei count function on all images in the selected folder
print("Counting Nuclei...");
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tiff images
		filePath = sourceImagesDir + fileList[i];
		open(filePath);
		if (is("composite")) { //process only composite images
			countNuclei(getTitle(), File.nameWithoutExtension);
		counter ++;
		}
	}
    close("*");
   	showProgress(i, fileList.length);	
}

print("Nuclei Count Complete");
print("Total Files Processed: " + counter);



function countNuclei(title, name) { 
	
	//Extract the red channel from image
	run("Split Channels");
	selectWindow("C1-" + title);
	run("8-bit");
	close("\\Others");
	
	//Process image
	setThreshold(nucleiThreshold, 255);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	run("Fill Holes");
	run("Convert to Mask");
	run("Watershed");
	
	
	//Analyze image for nuclei
	run("Analyze Particles...", "size=10-Infinity show=Outlines display exclude summarize");
	close("*");
}

selectWindow("Summary"); 
saveAs("Results", resultsFile);
return "counted";