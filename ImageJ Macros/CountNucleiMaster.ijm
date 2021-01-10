//This macro performs a nuclei count and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021


setBatchMode(true);
print("\\Clear");

//Set key parameters
countNuclei = true;
saveImages = true;
plotData = true;

NucleiThreshold = 35;
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format

//Specify folder containing IJ Macros
IJMacrosDir = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/ImageJ Macros/";
IJMacrosDir = IJMacrosDir.replace("\\", "/"); //convert directory path to universal format 

//Specify folder containing R Scripts
RScriptsDir = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/";
RScriptsDir = RScriptsDir.replace("\\", "/"); //convert directory path to universal format


//Extract the experimentId from the directory path
experimentId = split(workingDir, "/");
experimentId = experimentId[(lengthOf(experimentId)-1)];





//Call the appropriate Nuclei Count macro and pass the working directory, nuclei threshold and experimentId as arguments
CountWithImgSaveMacroPath = IJMacrosDir + "CountNucleiWithImageSave.ijm" //specify path to macro for nuclei count with image saving on
CountWithoutImgSaveMacroPath = IJMacrosDir + "CountNucleiWithoutImageSave.ijm" //specify path to macro for nuclei count with image saving off
NucleiCountArgs = workingDir + "&&" + NucleiThreshold + "&&" + experimentId; //combine macro arguments into one string

sourceImagesDir = workingDir + "TIFFs/";

if (countNuclei) {

	if (!File.exists(sourceImagesDir)) {
		print(" ");
		print("Error: TIFF images folder not found");
		print(sourceImagesDir);
		exit("TIFF images folder not found");
	}

	if (saveImages){
			runMacro(CountWithImgSaveMacroPath, NucleiCountArgs);
		} else {
			runMacro(CountWithoutImgSaveMacroPath, NucleiCountArgs);
		}
}



//Run the SummariseNucleiCount R script to summarise the data and plot graphs
callRScriptMacroPath = IJMacrosDir + "CallRScript.ijm";
pathToRScript = RScriptsDir + "SummariseNucleiCount.R"; //speficy the path to the R script to be run
CallRScriptArgs = workingDir + "&&" + pathToRScript + "&&" + experimentId; //combine macro arguments into one string

nucleiCountResultsFile = workingDir + experimentId +  "_Nuclei_Count.csv";
imageSequenceFile = workingDir + experimentId + "_Image_Capture.txt";


if (plotData){
	
		if (!File.exists(nucleiCountResultsFile)) { //check to ensure a nuclei count results file is present
			print(" ");
			print("Error: Nuclei Count Results file not found");
			print(nucleiCountResultsFile);
			exit("Nuclei Count Results file not found");
		}

		if (!File.exists(imageSequenceFile)) { //check to ensure an image sequence file is present
			print(" ");
			print("Error: Image Sequence file not found");
			print(imageSequenceFile);
			exit("Image Sequence file not found");
		}
		
		print(" ");
		print("Summarising and plotting data...");
		runMacro(callRScriptMacroPath, CallRScriptArgs);			
		print("Data Summary and Plot Complete");
		}
