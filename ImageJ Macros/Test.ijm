setBatchMode(true);
filelist = getArgument();
file = split(filelist,'#');
open(file[0]);
run("Invert");
saveAs("Tiff", file[1]);
close();
print("Done");
run("Quit");
//eval("script", "System.exit(0);");



ImageJ-win64.exe --headless batch"C:\Users\Victor Kumbol\Documents\GitHub\Image-Analysis\ImageJ Macros\Test.ijm" D:\In.tif#D:\Out3.tif