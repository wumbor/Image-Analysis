
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Neurphology macro
////
//// This macro quantifies neurite length, neurite area, neurite endpoints, neurite attachment points, and soma size
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Created by Eric Huang, June 2011 https://hwangeric5.wixsite.com/erichwanglab/neurphologyj
//Modified by Victor Kumbol, August 2021


requires("1.42k");
setBatchMode(true);
run("Set Measurements...", "area mean standard modal shape limit redirect=None decimal=3");
setForegroundColor(0, 0, 0);



//Select source folder and retrieve file list 
userDir = getDirectory("Choose Source Folder");

//Create an output subfolder
outputDir = userDir + "Morphology Analysis" + File.separator;
sourceDir = userDir + "Morphology" + File.separator;
//sourceDir = userDir;
fileList = getFileList(sourceDir);


if (!File.exists(outputDir)) {
	File.makeDirectory(outputDir);
	print("");
	print("Output folder created: " + outputDir);
	print("");
}

//Initialise Results file
resultsFile = userDir + "MorphologyAnalysis.csv";
if (File.exists(resultsFile)) {
	garbage = File.delete(resultsFile); // delete any existing results file
} 

f = File.open(resultsFile); //Create a new results file
print(f, "Filename, SomaCount, TotalSomaArea, AverageSomaArea, NeuriteCount, TotalNeuriteArea, TotalNeuriteLength, AttachmentPointsCount, EndPointsCount");
File.close(f);


//Perform morphology analysis on images in folder
counter = 0;
for (i = 0; i < fileList.length; i++){
	if (endsWith(fileList[i], ".tif")) { //process only tif files
		sourceFilePath = sourceDir + fileList[i];
		open(sourceFilePath);
		analyseMorphology(outputDir, getTitle(), File.nameWithoutExtension);	
	    counter ++;
	}
    close("*");
   	showProgress(i, fileList.length);	
}


print("Morphology Analysis Completed");
print("Total Images Analysed: " + counter);









function analyseMorphology(outputDir, title, name) {
	
	//showMessage("select image","select the image you want to analyze (16 bit or less)");
	//open();
	run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
	rename("original");
	run("Duplicate...", "title=flatten");
	run("Subtract Background...", "rolling=50");
	
	////ask whether a wizard set-up is needed
	/*
	if(getBoolean("Do you want to use the set-up wizard?")==1)
	{
	selectWindow("original");
	run("Duplicate...", "title=blurred");
	selectWindow("blurred");
	run("Gaussian Blur...", "sigma=1");
	imageCalculator("Subtract create", "original","blurred");
	rename("sub_blurred");
	selectWindow("blurred");
	close();
	selectWindow("sub_blurred");
	run("Threshold...");
	waitForUser("Select a proper threshold so that all neurites are shown\nThis is your contrast level. Click ok when done");
	getThreshold(lower1,upper1);
	close();
	selectWindow("flatten");
	run("Threshold...");
	waitForUser("Select a proper threshold so that all cell bodies are shown\nThis is your soma intensity. Click ok when done");
	getThreshold(lower2,upper2);
	print("\\Clear");
	print("contrast level: " + lower1);
	print("soma intensity: " + lower2);
	
	}
	else
	{
	}
	*/

	//Define key analysis parameters
	lowc = 2; 		//contrast level
	lowi = 7;		//soma intensity
	nwidth = 5;		//width of neurites in pixel
	cleanup = 15;	//particle cleanup value

	/*
	lowc=getNumber("Please type in your contrast level, increase it if too many particles are detected", 19);
	
	lowi=getNumber("Please type in your soma intensity, reduce it if too little cell body gets picked up", 245);
	
	nwidth=getNumber("Please type in the width of your neurite in pixel", 6);
	
	cleanup=getNumber("Please enter the particle cleanup value", 15);
	*/
	
	if(bitDepth()!=16)
	{
	 run("16-bit");
	}
	else
	
	////Background cleanup process
	
	////detecting low contrast pixels
	selectWindow("original");
	run("Duplicate...", "title=blurred");
	selectWindow("blurred");
	run("Gaussian Blur...", "sigma=1");
	imageCalculator("Subtract create", "original","blurred");
	rename("sub_blurred");
	selectWindow("blurred");
	close();
	
	////create a mask for low contrast pixels
	selectWindow("sub_blurred");
	setThreshold(0, lowc);
	run("Create Mask");
	rename("low_contrast");
	
	////create a mask for low intensity pixels
	selectWindow("flatten");
	setThreshold(0, lowi);
	run("Create Mask");
	rename("low_intensity");
	run("Clear Results");
	selectWindow("flatten");
	close();
	
	////create a mask to remove low intensity and low contrast pixels
	imageCalculator("AND create", "low_contrast","low_intensity");
	rename("zero_intensity");
	run("Invert");
	run("16-bit");
	run("Subtract...", "value=254");
	
	selectWindow("low_intensity");
	close();
	selectWindow("low_contrast");
	close();
	selectWindow("original");
	resetThreshold();
	imageCalculator("Multiply create", "original","zero_intensity");
	rename("new_original");
	
	////soma detection
	selectWindow("new_original");
	run("Duplicate...", "title=open");
	run("Minimum...", "radius="+nwidth+"");
	run("Maximum...", "radius="+nwidth+"");
	setAutoThreshold("Mean dark");
	run("Create Mask");
	rename("soma");
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	
	////image clean up (remove small particles and debris) and soma removal
	selectWindow("new_original");
	run("Duplicate...", "title=neuritesoma");
	setAutoThreshold("Mean dark");
	run("Create Mask");
	rename("neuritesoma_mask");
	setBackgroundColor(0, 0, 0);
	setAutoThreshold();
	run("Particle Remover", "size=0-"+cleanup+" circularity=0.00-1.00 show=Nothing");
	rename("neuritesoma_clean");
	run("Duplicate...", "title=neurite");
	selectWindow("soma");
	run("Create Selection");
	selectWindow("neurite");
	run("Restore Selection");
	run("Clear");
	run("Select None");
	
	////neurite detection
	selectWindow("neurite");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	
	////neurite length detection
	selectWindow("neuritesoma_clean");
	run("Duplicate...", "title=neuritesoma_skeleton");
	run("Skeletonize");
	imageCalculator("Subtract create", "neuritesoma_skeleton","soma");
	rename("neurite_length");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	selectWindow("neuritesoma_clean");
	close();
	
	////attachment point detection
	selectWindow("soma");
	run("Select None");
	run("Duplicate...", "title=soma_dilate");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	imageCalculator("AND create", "soma_dilate","neuritesoma_skeleton");
	rename("stem");
	run("Duplicate...", "title=stem_erode");
	run("Options...", "iterations=1 black count=7");
	run("Erode");
	imageCalculator("Subtract create", "stem","stem_erode");
	rename("stem_points");
	imageCalculator("Subtract create", "stem_points","soma");
	rename("attachment_points");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	selectWindow("stem");
	close();
	selectWindow("stem_erode");
	close();
	selectWindow("neuritesoma_skeleton");
	close();
	selectWindow("stem_points");
	close();
	
	////endpoint detection
	selectWindow("neurite_length");
	run("Duplicate...", "title=neurite_erode");
	run("Options...", "iterations=1 black count=7");
	run("Erode");
	imageCalculator("Subtract create", "neurite_length","neurite_erode");
	rename("tip");
	imageCalculator("Subtract create", "tip","soma_dilate");
	rename("endpoints");
	setAutoThreshold();
	run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
	run("Options...", "iterations=1 black count=1");
	run("Dilate");
	selectWindow("neurite_erode");
	close();
	selectWindow("tip");
	close();
	selectWindow("soma_dilate");
	close();
	
	////create and save color-combined image for user evaluation
	selectWindow("soma");
	run("Select None");
	selectWindow("original");
	run("8-bit");
	run("Merge Channels...", "red=neurite_length green=attachment_points blue=soma gray=original create keep");
	rename("red=neurite green=attachment-points blue=soma gray=original");
	saveAs("Tiff", outputDir + name + "_analysed");


	//save the analysis results to a csv file
	selectWindow("Summary"); 
	filename = title;
	somaCount = Table.get("Count", 0);
	totalSomaArea = Table.get("Total Area", 0);
	averageSomaArea = Table.get("Average Size", 0);
	neuriteCount = Table.get("Count", 1);
	totalNeuriteArea = Table.get("Total Area", 1);
	totalNeuriteLength = Table.get("Total Area", 2);
	attachmentPointsCount = Table.get("Count", 3);
	endPointsCount = Table.get("Count", 4);
	
	File.append(filename +","+ somaCount +","+ totalSomaArea +","+ averageSomaArea +","+ neuriteCount +","+ totalNeuriteArea +","+ totalNeuriteLength +","+ attachmentPointsCount +","+ endPointsCount, resultsFile);
	
	////close all windows that are not needed anymore
	run("Close");

}


  
  