
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Neurphology macro
////
//// This macro quantifies neurite length, neurite area, neurite endpoints, neurite attachment points, and soma size
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  
  requires("1.42k");
  run("Set Measurements...", "area mean standard modal shape limit redirect=None decimal=3");
  setForegroundColor(0, 0, 0);
  showMessage("select image","select the image you want to analyze (16 bit or less)");
  open();
  run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
  rename("original");
  run("Duplicate...", "title=flatten");
  run("Subtract Background...", "rolling=50");

  ////ask whether a wizard set-up is needed
  
  if(getBoolean("Do you want to use the set-up wizard?")==1)
  {
  selectWindow("original");
  run("Duplicate...", "title=blurred");
  selectWindow("blurred");
  run("Gaussian Blur...", "sigma=1");
  imageCalculator("Subtract create", "original","blurred");
  rename("sub_blurred");
  selectWindow("blurred");
  close();
  selectWindow("sub_blurred");
  run("Threshold...");
  waitForUser("Select a proper threshold so that all neurites are shown\nThis is your contrast level. Click ok when done");
  getThreshold(lower1,upper1);
  close();
  selectWindow("flatten");
  run("Threshold...");
  waitForUser("Select a proper threshold so that all cell bodies are shown\nThis is your soma intensity. Click ok when done");
  getThreshold(lower2,upper2);
  print("\\Clear");
  print("contrast level: " + lower1);
  print("soma intensity: " + lower2);

  }
  else
  {
  }
  
  lowc=getNumber("Please type in your contrast level, increase it if too many particles are detected", 19);

  lowi=getNumber("Please type in your soma intensity, reduce it if too little cell body gets picked up", 245);

  nwidth=getNumber("Please type in the width of your neurite in pixel", 6);

  cleanup=getNumber("Please enter the particle cleanup value", 15);

  if(bitDepth()!=16)
  {
     run("16-bit");
  }
  else

////Background cleanup process

  ////detecting low contrast pixels
  selectWindow("original");
  run("Duplicate...", "title=blurred");
  selectWindow("blurred");
  run("Gaussian Blur...", "sigma=1");
  imageCalculator("Subtract create", "original","blurred");
  rename("sub_blurred");
  selectWindow("blurred");
  close();
  
  ////create a mask for low contrast pixels
  selectWindow("sub_blurred");
  setThreshold(0, lowc);
  run("Create Mask");
  rename("low_contrast");

  ////create a mask for low intensity pixels
  selectWindow("flatten");
  setThreshold(0, lowi);
  run("Create Mask");
  rename("low_intensity");
  run("Clear Results");
  selectWindow("flatten");
  close();

  ////create a mask to remove low intensity and low contrast pixels
  imageCalculator("AND create", "low_contrast","low_intensity");
  rename("zero_intensity");
  run("Invert");
  run("16-bit");
  run("Subtract...", "value=254");
  
  selectWindow("low_intensity");
  close();
  selectWindow("low_contrast");
  close();
  selectWindow("original");
  resetThreshold();
  imageCalculator("Multiply create", "original","zero_intensity");
  rename("new_original");
  
  ////soma detection
  selectWindow("new_original");
  run("Duplicate...", "title=open");
  run("Minimum...", "radius="+nwidth+"");
  run("Maximum...", "radius="+nwidth+"");
  run("Threshold...");
  waitForUser("select a proper threshold and click ok");
  run("Create Mask");
  rename("soma");
  run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
  
  ////image clean up (remove small particles and debris) and soma removal
  selectWindow("new_original");
  run("Duplicate...", "title=neuritesoma");
  waitForUser("select a proper threshold and click ok");
  run("Create Mask");
  rename("neuritesoma_mask");
  setBackgroundColor(0, 0, 0);
  setAutoThreshold();
  run("Particle Remover", "size=0-"+cleanup+" circularity=0.00-1.00 show=Nothing");
  rename("neuritesoma_clean");
  run("Duplicate...", "title=neurite");
  selectWindow("soma");
  run("Create Selection");
  selectWindow("neurite");
  run("Restore Selection");
  run("Clear");
  run("Select None");
  
  ////neurite detection
  selectWindow("neurite");
  setAutoThreshold();
  run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
  
  ////neurite length detection
  selectWindow("neuritesoma_clean");
  run("Duplicate...", "title=neuritesoma_skeleton");
  run("Skeletonize");
  imageCalculator("Subtract create", "neuritesoma_skeleton","soma");
  rename("neurite_length");
  setAutoThreshold();
  run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
  selectWindow("neuritesoma_clean");
  close();

  ////attachment point detection
  selectWindow("soma");
  run("Select None");
  run("Duplicate...", "title=soma_dilate");
  run("Options...", "iterations=1 black count=1");
  run("Dilate");
  imageCalculator("AND create", "soma_dilate","neuritesoma_skeleton");
  rename("stem");
  run("Duplicate...", "title=stem_erode");
  run("Options...", "iterations=1 black count=7");
  run("Erode");
  imageCalculator("Subtract create", "stem","stem_erode");
  rename("stem_points");
  imageCalculator("Subtract create", "stem_points","soma");
  rename("attachment_points");
  setAutoThreshold();
  run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
  run("Options...", "iterations=1 black count=1");
  run("Dilate");
  selectWindow("stem");
  close();
  selectWindow("stem_erode");
  close();
  selectWindow("neuritesoma_skeleton");
  close();
  selectWindow("stem_points");
  close();

  ////endpoint detection
  selectWindow("neurite_length");
  run("Duplicate...", "title=neurite_erode");
  run("Options...", "iterations=1 black count=7");
  run("Erode");
  imageCalculator("Subtract create", "neurite_length","neurite_erode");
  rename("tip");
  imageCalculator("Subtract create", "tip","soma_dilate");
  rename("endpoints");
  setAutoThreshold();
  run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Nothing summarize");
  run("Options...", "iterations=1 black count=1");
  run("Dilate");
  selectWindow("neurite_erode");
  close();
  selectWindow("tip");
  close();
  selectWindow("soma_dilate");
  close();

  ////create color-combined image for user evaluation
  selectWindow("soma");
  run("Select None");
  selectWindow("original");
  run("8-bit");
  run("Merge Channels...", "red=neurite_length green=endpoints blue=soma gray=original create keep");
  rename("red=neurite green=endpoints blue=soma gray=original");
  run("Channels Tool...");
  run("Merge Channels...", "red=neurite_length green=attachment_points blue=soma gray=original create keep");
  rename("red=neurite green=attachment-points blue=soma gray=original");
  run("Channels Tool...");

  ////close windows that are not needed anymore
  selectWindow("sub_blurred");
  close();
  selectWindow("zero_intensity");
  close();
  selectWindow("open");
  close();
  selectWindow("new_original");
  close();
  selectWindow("neurite");
  close();
  selectWindow("neurite_length");
  close();
  selectWindow("soma");
  close();
  selectWindow("neuritesoma");
  close();
  selectWindow("neuritesoma_mask");
  close();
  selectWindow("attachment_points");
  close();
  selectWindow("endpoints");
  close();
