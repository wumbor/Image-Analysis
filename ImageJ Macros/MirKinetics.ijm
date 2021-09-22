//This macro performs measures fluorescence intensity in one channel based on ROIs defined another channel of a time-lapse image stack
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
run("Set Measurements...", "area mean integrated stack display redirect=None decimal=3");

//Request file from user and run analysis on all frames in the hyperstack
filePath = File.openDialog("Choose image file");
AnalyzeKinetics(filePath);



function AnalyzeKinetics(filePath) {
	
	if (endsWith(filePath, ".nd2")) { 
		run("Bio-Formats Importer", "open=[filePath] color_mode=Composite rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
	} else if (endsWith(filePath, ".tif")) {
		open(filePath);
	}

	//Make sure image is a hyperstack
	hyperStack = getTitle();
	if (Stack.isHyperstack) {
		getDimensions(width, height, channels, slices, frames);
	} else {
		exit("The image must be a hyperstack");
	}
	
	//Create an output subfolder
	parentDir = File.getParent(filePath);
	outputDir = parentDir + File.separator + File.nameWithoutExtension + "_Results";
	if (!File.exists(outputDir)) {
		File.makeDirectory(outputDir);
		print("");
		print("Output folder created: " + outputDir);
		print("");
	}
	

	//iterate over all the frames in the hyperstack and perform fluorescence measurements on each
	for (i = 1; i <= frames; i++) {
		selectWindow(hyperStack);
		Stack.setFrame(i);
		run("Reduce Dimensionality...", "channels slices keep");
		currentFrame = getTitle();	
		currentFrameIndex = i;
		MeasureInFrame(hyperStack, currentFrame, currentFrameIndex, outputDir);
		showProgress(i, frames);	
	}

print("Analysis Completed");
print("Total Frames Analysed: " + frames);
}





function MeasureInFrame(hyperStack, frame, frameIndex, outputDir) {
	C1name = "C1-" + frame;
	C2name = "C2-" + frame;
	C3name = "C3-" + frame;
	channelsToMeasure = newArray(C1name, C2name, C3name);
	channelColors = newArray(C1color, C2color, C3color);

	//Duplicate the channel to be used for creating ROIs
	selectWindow(frame);
	run("Split Channels");
	selectWindow(C3name);
	run("Duplicate...", "title=StackforROI duplicate");
	StackForROI = "StackforROI";

	C3ROIs = CreateROIs(StackForROI, C3color); //create ROIs
	roiList = C3ROIs; 
	
	//Measure fluorescence in all channels using ROI mask from desired channel
	for (i = 0; i < lengthOf(channelsToMeasure); i++) {
		MeasureInROIs(channelsToMeasure[i], roiList);
		resultFilePath = outputDir + File.separator + hyperStack +"_miRKinetics_"+ channelColors[i]+"-"+frameIndex+".csv";
		selectWindow("Results");
		saveAs("Results", resultFilePath);
	}
		
	roiManager("Deselect");	
	roiManager("Delete");
	
	selectWindow(hyperStack);
	close("\\Others");
    run("Clear Results");
}




function MeasureInROIs(ChannelName, ROIstoMeasure) { 
	selectWindow(ChannelName); //select the channel to perform measurements on 

	//First, measure fluorescence using individual ROI masks from channels
	for (i = 0; i < lengthOf(ROIstoMeasure); i++) {
		roiManager("select", i);
		roiManager("measure");
	}
	
	
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
	//run("8-bit");
	//run("Auto Local Threshold", "method=Contrast radius=15 parameter_1=0 parameter_2=0 white stack");
	//run("Fill Holes", "stack");
	
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
	selectWindow(ChannelName);
	close();
}




