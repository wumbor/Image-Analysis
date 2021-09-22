//This macro performs measures fluorescence intensity in one channel based on ROIs defined another channels of a time-lapse image stack
//Written by Victor Kumbol, August 2021

//Close the ROI Manger if it is already open
if (isOpen("ROI Manager")) {
 selectWindow("ROI Manager");
 run("Close");
}

//Set IJ parameters
setBatchMode(true);
setOption("ExpandableArrays", true);
run("Clear Results");

//Set major image parameters
var C1color = "blue";
var C2color = "green";
var C3color = "red";
var roiList = newArray;


//|Request working directory from user and run analysis on all images in the folder 
userDir = getDirectory("Choose Source Folder");
AnalyzeKinetics(userDir);




function AnalyzeKinetics(workingDir) {
	sourceDir = workingDir + "TIFFs" + File.separator;
	outputDir = workingDir +"Results" + File.separator;
	fileList = getFileList(sourceDir);
	
	//Create an output subfolder
	if (!File.exists(outputDir)) {
		File.makeDirectory(outputDir);
		print("");
		print("Output folder created: " + outputDir);
		print("");
	}
	
	
	//Analyse all images
	counter = 0;
	for (i = 0; i < fileList.length; i++){
		if (endsWith(fileList[i], ".tif")) { //process only tif images
			sourceFilePath = sourceDir + fileList[i];
			open(sourceFilePath);
			resultFilePath= outputDir + File.nameWithoutExtension + "_miRKinetics.csv";
			stack = getTitle();
			run("Split Channels");
			C1name = "C1-" + stack;
			C2name = "C2-" + stack;
			C3name = "C3-" + stack;
	
		
			//Measure fluorescence using individual ROI masks from channels
			C3ROIs = CreateROIs(C3name, C3color);
			C1ROIs = newArray; //place holder for other channels if needed
			roiList = Array.concat(C1ROIs, C3ROIs); //merge ROI masks from separate channels  
			MeasureInROIs(C2name, roiList);
			
			selectWindow("Results");
			saveAs("Results", resultFilePath);
			roiManager("Deselect");	
			roiManager("Delete");
		    counter ++;
		}
	    close("*");
	    run("Clear Results");
	   	showProgress(i, fileList.length);	
	}
	
	print("Analysis Completed");
	print("Total Files Analysed: " + counter);

}












function MeasureInROIs(ChannelName, ROIstoMeasure) { 
	selectWindow(ChannelName); //select the channel to perform measurements on (ALexa488)
	close("\\Others");
	
	run("Set Measurements...", "area mean integrated stack display redirect=None decimal=3");

	//First, measure fluorescence using individual ROI masks from channels
	for (i = 0; i < lengthOf(ROIstoMeasure); i++) {
		roiManager("select", i);
		roiManager("measure");
	}
	


	//Create composite masks and measure fluorescence in these composite ROI masks
	CompositeROIs = newArray;
	//CompositeROIIndices = newArray;
	ChannelsCount = 2;
	ROIsPerChannel = (lengthOf(ROIstoMeasure)/ChannelsCount);
	C1ROIsIndex = 0;
	C3ROIsIndex = 0 + ROIsPerChannel;

	//select, combine and measure fluorescence corresponding ROIs
	selectWindow(ChannelName);
	for (i = 1; i <= ROIsPerChannel; i++) {
		roiManager("select", newArray(C1ROIsIndex, C3ROIsIndex));
		roiManager("combine");
		roiManager("deselect");
		roiManager("select", C1ROIsIndex);
		roiManager("measure");
		CompositeROIs = Array.concat(CompositeROIs, "composite" +"-"+ i);
		//CompositeROIIndices = Array.concat(CompositeROIIndices, C1ROIsIndex); 
		C1ROIsIndex = C1ROIsIndex + 1;
		C3ROIsIndex = C3ROIsIndex + 1;
	}
	ROIstoMeasure = Array.concat(ROIstoMeasure, CompositeROIs);


	//Rearrange the results table to display relevant data in desired format
	n = nResults;
  	label = newArray(n);
  	area = newArray(n);
	mean = newArray(n);
	rawintden = newArray(n);
  	slice = newArray(n);

    for (i=0; i<n; i++) {
      label[i] = getResultLabel(i);
      area[i] = getResult('Area', i);
      mean[i] = getResult('Mean', i);
      rawintden[i] = getResult('RawIntDen', i);
      slice[i] = getResult('Slice', i);
    }
   	run("Clear Results");
   	run("Select None");

   for (i=0; i<n; i++) {
      setResult("Channel", i, ROIstoMeasure[i]);
      setResult("Label", i, label[i]);
      setResult("Area", i, area[i]);
      setResult("Mean", i, mean[i]);
      setResult("RawIntDen", i, rawintden[i]);
      setResult("Slice", i, slice[i]);
    }
     updateResults();
    
}








function CreateROIs(ChannelName, ChannelColor) { 
	selectWindow(ChannelName); //select the channel to create masked image
	run("Convert to Mask", "method=Otsu background=Dark calculate black");
	run("Fill Holes", "stack");
	
	ROIs = newArray;	
	for (i = 1; i <= nSlices; i++) { //loop over the slices, create a selection and ROI from each slice
	    setSlice(i);
	    run("Create Selection");
		roiManager("Add");
		ROIs = Array.concat(ROIs, ChannelColor +"-"+ i);

		//create inverse ROI
		roiManager("Deselect");
		selectWindow(ChannelName);
		run("Create Selection");
		run("Make Inverse");
		roiManager("Add");
		ROIs = Array.concat(ROIs, ChannelColor +" inverse -"+ i);
	}
	return ROIs;
}




