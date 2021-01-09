//This macro performs a nuclei count and plots the summary data by calling submacros and Rscripts 
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021


setBatchMode(true);

//Set key parameters
convertImages = false;
saveImages = true;
NucleiThreshold = 35;
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/") //convert directory path to universal format



//Extract the experimentId from the directory path
experimentId = split(workingDir, "/");
experimentId = experimentId[(lengthOf(experimentId)-1)];

//Convert the working directory to the universal format
RWorkingDir = split(workingDir, "\\");
RWorkingDir = String.join(RWorkingDir, "/");
RWorkingDir = '"' + RWorkingDir + '"';

//Call the image converter macro
ImageConversionArgs = workingDir;
if (convertImages){
		print("Converting Images...");
		runMacro("ConvertVsiToTiff", ImageConversionArgs);
	}



//Call the appropriate Nuclei Count macro with the source folder and threshold
NucleiCountArgs = workingDir + "&&" + NucleiThreshold + "&&" + experimentId;
if (saveImages){
		print("Counting Nuclei...");
		runMacro("CountNucleiWithImageSave2", NucleiCountArgs);
	} else {
		print("Counting Nuclei...");
		runMacro("CountNucleiWithoutImageSave2", NucleiCountArgs);
	}


//Run the R script to summarise the data and plot graphs
/*
pathToRscript = 'Rscript.exe "D:/OneDrive - Charité - Universitätsmedizin Berlin/PhD Project-DESKTOP-3SHP9SG/mMORPH/R Scripts/Aggregate_Nuclei_3.R"';
executionPath = pathToRscript + ' ' + RWorkingDir;
exec("cmd", "/c", "start", "cmd", "/k", executionPath);

*/