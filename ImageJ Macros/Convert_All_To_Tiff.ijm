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



//Convert all nd2 and vsi files to tiffs
counter = 0;
for (i = 0; i < fileList.length; i++){
	sourceFilePath = sourceDir + fileList[i];
	ConvertToTiff(sourceFilePath);
    close("*");
    counter ++;	
   	showProgress(i, fileList.length);	
}
print("Conversion Completed");
print("Total Files Converted: " + counter);





function ConvertToTiff(filePath) { 
	//this function converts vsi and nd2 files to tiffs
	if (endsWith(fileList[i], ".nd2")) {
		run("Bio-Formats Importer", "open=[sourceFilePath] color_mode=Composite rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
		saveAs("Tiff", outputDir + File.nameWithoutExtension + ".tif");
	} else if(endsWith(fileList[i], ".vsi")) {
		run("Bio-Formats Importer", "open=[sourceFilePath] color_mode=Composite rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT series_1");
	 	saveAs("Tiff", outputDir + File.nameWithoutExtension + ".tif");
	}
   	
}



