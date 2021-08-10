//Set IJ parameters
setOption("ExpandableArrays", true);
run("Clear Results");

//Request working directory from user
workingDir = "D:\\OneDrive - Charité - Universitätsmedizin Berlin\\My PhD Project\\mMORPH\\2021_06_12_VK\\";
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format
originalImage = File.nameWithoutExtension;
resultsfile = workingDir + originalImage + "_miRLocation.csv";

//Set major image parameters
C1color = "blue";
C2color = "green";
C3color = "red";
roiList = newArray;

//Manually select the range slices to be analysed


//Create a substack from the original image
run("Make Substack...", "channels=1-3 slices=4-6");

//Select the substack and close the original image
substack = getInfo("window.title");
selectWindow(substack);
close("\\Others");

C1name = "C1-" + substack;
C2name = "C2-" + substack;
C3name = "C3-" + substack;

//Split the substack into component channels
run("Split Channels");


a = CreateROIs(C1name, C1color);
b = CreateROIs(C3name, C3color);
roiList = Array.concat(a, b);


MeasureInROIs(C2name, roiList)

selectWindow("Results");
saveAs("Results", resultsfile);
roiManager("Deselect");
roiManager("Delete");


function MeasureInROIs(ChannelName, ROIstoMeasure) { 
	selectWindow(ChannelName); //select the channel to perform measurements on (ALexa488)
	close("\\Others");
	
	run("Set Measurements...", "area mean stack display redirect=None decimal=3");
	
	for (i = 0; i < lengthOf(ROIstoMeasure); i++) {
		roiManager("select", i);
		roiManager("measure");
	}
	
	n = nResults;
  	label = newArray(n);
  	area = newArray(n);
	mean = newArray(n);
  	slice = newArray(n);

    for (i=0; i<n; i++) {
      label[i] = getResultLabel(i);
      area[i] = getResult('Area', i);
      mean[i] = getResult('Mean', i);
      slice[i] = getResult('Slice', i);
    }
   	run("Clear Results");
   	run("Select None");

   for (i=0; i<n; i++) {
      setResult("Channel", i, ROIstoMeasure[i]);
      setResult("Label", i, label[i]);
      setResult("Area", i, area[i]);
      setResult("Mean", i, mean[i]);
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
	}
	return ROIs;
}









