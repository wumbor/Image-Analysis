setBatchMode(true);

//Select source folder and retrieve file list 
userDir = getDirectory("Choose Source Folder");

//Create an output subfolder
outputDir = userDir + "TIFFs" + File.separator;
sourceDir = userDir + "Raw Data" + File.separator;
fileList = getFileList(sourceDir);


if (!File.exists(outputDir)) {
	File.makeDirectory(outputDir);
	print("");
	print("Output folder created: " + outputDir);
	print("");
}


//Convert all vsi files to tiffs
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".vsi")) { //process only vsi files
		sourceFilePath = sourceDir + fileList[i];
		run("Viewer", "open=[sourceFilePath]");
	    saveAs("Tiff", outputDir + File.nameWithoutExtension + ".tif");	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}


print("Conversion Completed");
print("Total Files Converted: " + counter);