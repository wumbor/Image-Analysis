//This macro count the number of nuclei in a composite image
//Written by Victor Kumbol, June 2020
//Modified by Victor Kumbol, January 2021


setBatchMode(true);

//Set key parameters
convertImages = true;
saveImages = true;
NucleiThreshold = 35;
IJWorkingDir = getDirectory("Choose Source Folder");

//Extract the experimentId from the file path
experimentId = split(IJWorkingDir, "\\");
experimentId = experimentId[(lengthOf(experimentId)-1)];

//Convert the working directory to the universal format
RWorkingDir = split(IJWorkingDir, "\\");
RWorkingDir = String.join(RWorkingDir, "/");
RWorkingDir = '"' + RWorkingDir + '"';

//Call the image converter macro
ImageConversionArgs = IJWorkingDir;
if (convertImages){
		print("Converting Images...");
		runMacro("ConvertVsiToTiff", ImageConversionArgs);
	}



//Call the appropriate Nuclei Count macro with the source folder and threshold
NucleiCountArgs = IJWorkingDir + "&&" + NucleiThreshold + "&&" + experimentId;
if (saveImages){
		print("Counting Nuclei...");
		runMacro("CountNucleiWithImageSave", NucleiCountArgs);
	} else {
		print("Counting Nuclei...");
		runMacro("CountNucleiWithoutImageSave", NucleiCountArgs);
	}


//Run the R script to summarise the data and plot graphs
pathToRscript = 'Rscript.exe "D:/OneDrive - Charité - Universitätsmedizin Berlin/PhD Project-DESKTOP-3SHP9SG/mMORPH/R Scripts/Aggregate_Nuclei_3.R"';
executionPath = pathToRscript + ' ' + RWorkingDir;
exec("cmd", "/c", "start", "cmd", "/k", executionPath);