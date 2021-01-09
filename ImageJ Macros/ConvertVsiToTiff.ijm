setBatchMode(true);


//Retrieve parameters from parent macro
args = getArgument();
args = split(args, "&&");
workingDir = args[0] 
//nucleiThreshold = parseInt(args[1]);
//experimentId = args[2];



//Select source folder and retrieve file list 
rawImagesDir = workingDir + "Raw Data" + File.separator;
fileList = getFileList(rawImagesDir);

//Create an output subfolder
convertedImagesDir = workingDir + "TIFFs" + File.separator;
if (!File.exists(convertedImagesDir)) {
	File.makeDirectory(convertedImagesDir);
	print("");
	print("Output folder created: " + convertedImagesDir);
	print("");
}


//Convert all vsi files to tiffs
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".vsi")) { //process only vsi files
		sourceFilePath = rawImagesDir + fileList[i];
		run("Viewer", "open=[sourceFilePath]");
	    saveAs("Tiff", convertedImagesDir + File.nameWithoutExtension + ".tif");	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}


print("Conversion Completed");
print("Total Files Converted: " + counter);