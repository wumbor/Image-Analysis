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


//Convert all nd2 files to tiffs
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".nd2")) { //process only nd2 files
		sourceFilePath = sourceDir + fileList[i];
		run("Viewer", "open=[sourceFilePath]");
		run("Bio-Formats Importer", "open=[sourceFilePath] color_mode=Composite rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
	    saveAs("Tiff", outputDir + File.nameWithoutExtension + ".tif");	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}


print("Conversion Completed");
print("Total Files Converted: " + counter);