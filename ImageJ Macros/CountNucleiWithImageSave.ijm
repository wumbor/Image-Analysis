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
countedImagesDir = workingDir + "Nuclei Count/";
resultsFile = workingDir + experimentId + "_Nuclei_Count.csv";
fileList = getFileList(sourceImagesDir);


//Create a results subfolder
if (!File.exists(countedImagesDir)) {
	File.makeDirectory(countedImagesDir);
	print("");
	print("Nuclei Count folder created: ");
	print(countedImagesDir);
	print("");
}



//Run the nuclei count function on all images in the selected folder
print("Counting Nuclei...");
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tiff images
		filePath = sourceImagesDir + fileList[i];
		open(filePath);
		if (is("composite")) { //process only composite images
			countNuclei(countedImagesDir, getTitle(), File.nameWithoutExtension);
		counter ++;
		}
	}
    close("*");
   	showProgress(i, fileList.length);	
}

print("Nuclei Count Complete");
print("Total Files Processed: " + counter);



function countNuclei(outputDir, title, name) { 
	
	//Extract the red channel from image
	run("Split Channels");
	selectWindow("C1-" + title);
	run("8-bit");
	saveAs("tiff", outputDir + name + "_red");
	original_image = name + "_red.tif";
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
	
	//Save masked image with counted cells
	
	selectWindow("Drawing of " + name + "_red.tif");
	count_mask_image = "Drawing of " + name + "_red.tif";
	run("Magenta");
	run("Invert LUT");
	close("\\Others");
	open(outputDir + original_image);
	run("8-bit");
	run("Merge Channels...", "c4=&original_image c6=&count_mask_image");
	saveAs("tiff", outputDir + name + "_nuclei_count_mask");

}

selectWindow("Summary"); 
saveAs("Results", resultsFile);
run("Close All");