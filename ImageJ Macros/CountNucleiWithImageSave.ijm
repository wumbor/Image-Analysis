//This macro counts the number of nuclei in images and saves the processed images
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021

setBatchMode(true);
run("Clear Results");
run("Set Measurements...", "area mean modal median limit display redirect=None decimal=3");

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

/*
//Create a results subfolder
if (!File.exists(countedImagesDir)) {
	File.makeDirectory(countedImagesDir);
	print("");
	print("Nuclei Count folder created: ");
	print(countedImagesDir);
	print("");
}
*/

//If a Nuclei Count subfolder already exists, ask the user for permission to delete the current folder and create a new one, else quit the program
if (File.exists(countedImagesDir)) {
	//feedback = getBoolean("WARNING: Delete existing Nuclei Count folder?");
	feedback = true;
	if (feedback) {
		if (deleteFolder(countedImagesDir)) {
			File.makeDirectory(countedImagesDir);
		} else {
			exit("Folder was not deleted");
		}
	} else {
		exit("Nuclei Count Macro terminated");
	}
	} else {
		File.makeDirectory(countedImagesDir);
	}
	



//Run the nuclei count function on all images in the selected folder
print("\nCounting Nuclei...");
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

selectWindow("Summary"); 
saveAs("Results", resultsFile);
run("Close All");
return "true";


function countNuclei(outputDir, title, name) { 
	
	//Extract the red channel from image
	run("Split Channels");
	selectWindow("C1-" + title);
	run("8-bit");
	close("\\Others");
	run("Duplicate...", "title=original_image");
	selectWindow("original_image");
	run("Grays");
	run("Apply LUT");

	
	//Process image
	selectWindow("C1-" + title);
	
	//run("Mean...", "radius=5");
	run("Median...", "radius=5");
	run("Subtract Background...", "rolling=50");
	//setAutoThreshold("Otsu dark");
	setThreshold(nucleiThreshold, 255);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	run("Fill Holes");
	run("Convert to Mask");
	run("Watershed");
	

	

	/* ORIGINAL SCRIPT
	setThreshold(nucleiThreshold, 255);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	run("Fill Holes");
	run("Convert to Mask");
	run("Watershed");
	*/

		
	//Analyze image for nuclei
	run("Analyze Particles...", "size=20-Infinity show=Outlines display exclude summarize");
	
	//Save masked image with counted cells
	selectWindow("Drawing of C1-" + title);
	rename("count_mask_image");
	run("Magenta");
	run("Invert LUT");
	run("Merge Channels...", "c4=&original_image c6=&count_mask_image");
	saveAs("tiff", outputDir + name + "_nuclei_count_mask");
	close("*");
}


function deleteFolder(folderpath) { 
	//first delete all files in the folder
	deleteFileList = getFileList(folderpath);
		for (i = 0; i < deleteFileList.length; i++){
			deletefilePath = folderpath +deleteFileList[i];
			garbage = File.delete(deletefilePath);
			}	

	//delete the folder itself
	deletecompleted = File.delete(folderpath);
	if (deletecompleted) {
		//print("Delete completed");
		return true;
	} else {
		//print("Delete failed");
		return false;
	}
}