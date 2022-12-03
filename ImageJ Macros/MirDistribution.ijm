//This macro measures fluorescence intensity in one channel based on ROIs defined in the other channels of an image stack
//Written by Victor Kumbol, August 2021
//Modified by Victor Kumbol, October 2021

//Close the ROI Manger if it is already open
if (isOpen("ROI Manager")) {
 selectWindow("ROI Manager");
 run("Close");
}

//Set IJ parameters
setBatchMode(true);
setOption("ExpandableArrays", true);
run("Clear Results");
run("Set Measurements...", "area mean integrated stack display redirect=None decimal=3");

//Set major image parameters
var allChannelColors = newArray("blue", "green", "red", "fred");
var channelOfInterest = "green";
var channelsInImage = 4;
var channelsForComposite = newArray("red", "fred");

channelOfInterestIndex = getIndexinArray(allChannelColors, channelOfInterest);
allChannelColors = Array.trim(allChannelColors, channelsInImage);



//Request working directory from user and run analysis on all images in the folder 
userDir = getDirectory("Choose Source Folder");
AnalyzeDistribution(userDir);


function AnalyzeDistribution(workingDir) {
	sourceDir = workingDir + "TIFFs" + File.separator;
	outputDir = workingDir +"Results" + File.separator;
	fileList = getFileList(sourceDir);


	//If a Results folder already exists, ask the user for permission to delete it, else quit the program
if (File.exists(outputDir)) {
	feedback = getBoolean("WARNING: Delete existing results folder?");
	if (feedback) {
		if (deleteFolder(outputDir)) {
			File.makeDirectory(outputDir);
		} else {
			exit("Folder was not deleted");
		}
	} else {
		exit("Macro terminated");
	}
	} else {
		File.makeDirectory(outputDir);
	}
	
	
	//Analyse all images
	counter = 0;
	for (i = 0; i < fileList.length; i++){
		if (endsWith(fileList[i], ".tif")) { //process only tif images
			sourceFilePath = sourceDir + fileList[i];
			open(sourceFilePath);
			resultFilePath= outputDir + File.nameWithoutExtension + "_miRDistribution.csv";
				
			stack = getTitle();
			run("Split Channels");
			
			allChannelNames = newArray("C1-"+stack, "C2-"+stack, "C3-"+stack, "C4-"+stack);
			allChannelNames = Array.trim(allChannelNames, channelsInImage);
				
			roiList = newArray();
			for (j = 0; j < lengthOf(allChannelColors); j++) { //create ROIs for all channels except the channel of interest
				if (allChannelColors[j]!= channelOfInterest) {
					ROI = CreateROIs(allChannelNames[j], allChannelColors[j]);
					roiList = Array.concat(roiList, ROI); //update the ROI list
				}	
			}	

			MeasureInROIs(allChannelNames[channelOfInterestIndex], roiList);	//measure all single ROIs
			compositeROIChannels = channelsForComposite;
			compositeROIList = MeasureInCompositeROIs(allChannelNames[channelOfInterestIndex], roiList, compositeROIChannels); //create composite ROIs and measure in them
			roiList = Array.concat(roiList, compositeROIList);		//update the ROI list

			ReformatResults(roiList); //reformat results table for saving
					
			selectWindow("Results");
			saveAs("Results", resultFilePath);
			roiManager("Deselect");	
			roiManager("Delete");		
			close("*");
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
	//this function performs measurements on a list of ROIs in a defined channel
	selectWindow(ChannelName); //select the channel to perform measurements on 
	close("\\Others");

	//First, measure fluorescence using individual ROI masks from channels
	for (i = 0; i < lengthOf(ROIstoMeasure); i++) {
		roiManager("select", i);
		roiManager("measure");
	}
}


function ReformatResults(roiList) { 
// this function reformats the results table to display relevant columns
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
      setResult("Channel", i, roiList[i]);
      setResult("Label", i, label[i]);
      setResult("Area", i, area[i]);
      setResult("Mean", i, mean[i]);
      setResult("RawIntDen", i, rawintden[i]);
      setResult("Slice", i, slice[i]);
    }
     updateResults();
}


function MeasureInCompositeROIs(ChannelName, roiList, compositeROIChannels) { 
// this function creates composite ROIs and measures defined parameters in them
	selectWindow(ChannelName); //select the channel to perform measurements on 
	
	compositeROIIndices = newArray();
	compositeROIs = newArray();
	//extract the indices of ROIs from channels designated for composite
	for (i = 0; i < roiList.length; i++) {
		if (detectStringinArray(compositeROIChannels, roiList[i])) {
			compositeROIIndices = Array.concat(compositeROIIndices, i);
		}	
	}

	ROIIndex = 0;
	ROIsPerChannel = compositeROIIndices.length/compositeROIChannels.length;
	//loop over the ROIs per channel and create composite ROIs
	for (i = 0; i < ROIsPerChannel; i++) { 
		selectionArray = newArray();
		ROIIndex = i;
		//create a selection array to select the appropriate ROIs
		for (j = 0; j < compositeROIChannels.length; j++) {
			selectionArray = Array.concat(selectionArray, compositeROIIndices[ROIIndex]);
			ROIIndex = ROIIndex + ROIsPerChannel;	
		}

		//create composite ROIs and measure
		if (selectionArray.length>1) {
			roiManager("select", selectionArray);
			roiManager("combine");
			roiManager("deselect");
		} 
		roiManager("select", selectionArray[0]);
		roiManager("measure");
		compositeROIs = Array.concat(compositeROIs, "composite-" + (i+1));
	}

return compositeROIs;
}


function CreateROIs(ChannelName, ChannelColor) { 
	selectWindow(ChannelName); //select the channel to create masked image
	run("Convert to Mask", "method=Otsu background=Dark calculate black");	
	ROIs = newArray;	
	for (i = 1; i <= nSlices; i++) { //loop over the slices, create a selection and ROI from each slice
	    setSlice(i);
	    run("Create Selection");
		roiManager("Add");
		ROIs = Array.concat(ROIs, ChannelColor +"-"+ i);
	}
	return ROIs;
}


function getIndexinArray(array, string) { 
// this function returns the index of a string in an array
	for (i = 0; i < lengthOf(array); i++) {
		if (array[i]==string) {
			index = i;
		}		
	}
	return index;
}


function detectStringinArray(array, string) { 
// this function detects a match of a string in an array
	detected = false;
	for (i = 0; i < lengthOf(array); i++) {
		if (string.startsWith(array[i])) {
			detected = true;
		}		
	}
	return detected;
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
		return true;
	} else {
		return false;
	}
}
