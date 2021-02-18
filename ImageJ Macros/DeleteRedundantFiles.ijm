//This macro deletes redundant files in selected folder and subfolders
//Written by Victor Kumbol, June 2020

//Request working directory from user
workingDir = getDirectory("Choose Source Folder");
workingDir = workingDir.replace("\\", "/"); //convert directory path to universal format


mainFileList = getFileList(workingDir);
//get list of experiment folders
mainSubFoldersList = newArray();
for (i = 0; i < mainFileList.length; i++){
	mainFilePath = workingDir + mainFileList[i];
	
	if(File.isDirectory(mainFilePath)){
		subFolder = getSubfolders(mainFilePath);
		mainSubFoldersList= Array.concat(mainSubFoldersList, subFolder);
	}
}


//filter subfolders for Nuclei Count
NuceiCountFolders = newArray();
for (i = 0; i < mainSubFoldersList.length; i++){
	if (mainSubFoldersList[i].contains("Nuclei Count")) {
		NuceiCountFolders = Array.concat(NuceiCountFolders, mainSubFoldersList[i]);
	}
}
	


//delete relevant files in Nuclei Count folders
for (i = 0; i < NuceiCountFolders.length; i++){
	deleteRedFiles(NuceiCountFolders[i]);
	}	







function getSubfolders(directory) { 
	subfoldersList = newArray();	// this function retrieves the list of all subfolders from a filepath and returns true if any subfolders were found
	directory = directory;
	listOfFiles = getFileList(directory);
	for (i = 0; i < listOfFiles.length; i++) {
		filePath = directory + listOfFiles[i];
		if (File.isDirectory(filePath)) {
			subfoldersList = Array.concat(subfoldersList, filePath);
		}
	}

	return subfoldersList;
}



function deleteRedFiles(directory) { 
	//this function deletes files in the folder that end with "_red.tif"
	directory = directory;
	listOfFiles = getFileList(directory);
	for (i = 0; i < listOfFiles.length; i++) {
		filePath = directory + listOfFiles[i];
		if (endsWith(filePath, "_red.tif")) {
			print(filePath);
			File.delete(filePath);
		}
	}
	
}

/*
print("\nNucei Count Folders List");
//display the contents of the array
for (i = 0; i < NuceiCountFolders.length; i++){
	print(NuceiCountFolders[i]);
	}	


