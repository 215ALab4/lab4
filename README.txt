﻿1) Clone the github repository, maintaining the directory structure 
   git clone https://github.com/215ALab4/lab4.git

2) Place the image files into the "~/Rfiles" where the "~/" signifies
   the path to where you cloned the repository

3) To generate the plots in the LDA\QDA section
   a) Open "~/Rfiles/LDA_QDA.R"
   b) Change the working directory to "~/Rfiles" (line 8)
   c) Change the value of ImageSave to TRUE (line 9)
   d) Run the whole script

4) To generate the plots in the Logit section:
   a) Open "~/Rfiles/Logit.R”
   b) Change the working directory to "~/Rfiles" (line 8)
   c) Change the value of ImageSave to TRUE (line 9)
   d) Run the whole script

5) To generate the plots in the Random Forests section:
   a) Open "~/Rfiles/random_forest.R"
   b) Change the working directory to "~/Rfiles" (line 16)
   c) Change the value of ImageSave to TRUE (line 17)
   d) Run the whole script
   e) Change the number of trees by changing the num.tree variable.  For 
	our analysis, we used the 50 trees for the plots. The script will 
	some time for more than 50 trees (a couple of minutes).  
	REMARK: running random_forest.R will generate a lot of Rdata and cdv
	files in your directory.  Not all is used for the plots in the writeup.
	They are very useful for further exploration, and only some of the 
	analysis from them made it to the writeup.  The Rdata files all loads
	random forest objects, which one can then use as predictors of other
	image files.  Changing the loaded image files to any new ones and 
	rerunning the entire script will print and save all plots used in the
	writeup.  

6) To generate the plots in the EDA section:
   a) Open "~/Rfiles/EDA.R"
   b) Change the working directory to "~/Rfiles" (line 1)
   c) Change the value of ImageSave to TRUE (line 2)
   d) Run the whole script

7) All the image files should now be in "~/figures".  If there are any
   stragglers in "~/Rfiles", move them to "~/figures". 
   
8) The latex file should now compile.  Edit the analysis results as necessary.  Read our FAQ file for more information (mainly errata).
