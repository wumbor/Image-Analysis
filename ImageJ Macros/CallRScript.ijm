//This macro calls a specified R script and passes an argument for execution
//Written by Victor Kumbol, January 2021

setBatchMode(true);

//Retrieve parameters from parent macro
args = getArgument();
args = split(args, "&&");
workingDir = args[0] 
pathToRscript = args[1];



//Reformat workingDir 
workingDirforR = workingDir.substring(0, (lengthOf(workingDir)-1)); //remove trailing /
workingDirforRString = '"' + workingDirforR + '"'; //enclose path in quotation marks

//Enclose pathToRScript in quotes
pathToRscriptString = '"' + pathToRscript + '"'; //enclose path in quotation marks
RCmdArgs = "Rscript.exe" + " " + pathToRscriptString + " " + workingDirforRString; //specify execution parameters for command prompt

//exec("cmd", "/c", "start", "cmd", "/k", RCmdArgs)

feedback = exec("cmd", "/c", RCmdArgs); //run R Script via command prompt
return feedback;