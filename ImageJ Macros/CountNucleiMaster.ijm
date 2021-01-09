//This macro performs a nuclei count and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021


setBatchMode(true);

//Set key parameters
saveImages = true;
NucleiThreshold = 35;
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/") //convert directory path to universal format

//Specify folder containing IJ Macros
IJMacrosDir = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/ImageJ Macros/";
IJMacrosDir = IJMacrosDir.replace("\\", "/") //convert directory path to universal format 

//Specify folder containing R Scripts
RScriptsDir = "C:/Users/Victor Kumbol/Documents/GitHub/Image-Analysis/R Scripts/"
RScriptsDir = RScriptsDir.replace("\\", "/") //convert directory path to universal format


//Extract the experimentId from the directory path
experimentId = split(workingDir, "/");
experimentId = experimentId[(lengthOf(experimentId)-1)];





//Call the appropriate Nuclei Count macro and pass the working directory, nuclei threshold and experimentId as arguments
macroWithImageSavePath = IJMacrosDir + "CountNucleiWithImageSave.ijm" //specify path to macro for nuclei count with image saving on
macroWithoutImageSavePath = IJMacrosDir + "CountNucleiWithoutImageSave.ijm" //specify path to macro for nuclei count with image saving off
NucleiCountArgs = workingDir + "&&" + NucleiThreshold + "&&" + experimentId; //combine macro arguments into one string

if (saveImages){
		runMacro(macroWithImageSavePath, NucleiCountArgs);
	} else {
		runMacro(macroWithoutImageSavePath, NucleiCountArgs);
	}






//Run the R script to summarise the data and plot graphs

workingDirforR = workingDir.substring(0, (lengthOf(workingDir)-1)); //remove trailing /
workingDirforRString = '"' + workingDirforR + '"'; //enclose path in quotation marks

pathToNucleiCountRscript = RScriptsDir + "SummariseNucleiCount.R";
pathToNucleiCountRscriptString = '"' + pathToNucleiCountRscript + '"'; //enclose path in quotation marks

RCmdArgs = "Rscript.exe" + " " + pathToNucleiCountRscriptString + " " + workingDirforRString; //specify execution parameters for command prompt
exec("cmd", "/c", "start", "cmd", "/k", RCmdArgs);

/*
pathToRscript = 'Rscript.exe "D:/OneDrive - Charité - Universitätsmedizin Berlin/PhD Project-DESKTOP-3SHP9SG/mMORPH/R Scripts/Aggregate_Nuclei_3.R"';
executionPath = pathToRscript + ' ' + RWorkingDir;
exec("cmd", "/c", "start", "cmd", "/k", executionPath);

*/