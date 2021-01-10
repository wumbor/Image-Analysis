setBatchMode(true);

//Select source folder and retrieve file list 
sourceDir = getDirectory("Choose Source Folder");
fileList = getFileList(sourceDir);

//Create an output subfolder
outputDir = sourceDir + "TIFFs" + File.separator;
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