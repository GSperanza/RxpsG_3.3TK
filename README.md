# RxpsG_3.1TK
The new RxpsG_3.1 version differs from the previous version because the graphic user interfaced are generated directly using the tclTK commands. 
This has the advantage of avoiding the need of the two libaries 'gWidgets2' and 'gWidgets2tcltk' rendering the software lighter, and  faster in
generating the GUIs.

RxpsG installation

1. Install R and Rstudio on your computer 

2. Verify all is working by typing:
   version
   you should get something like:
   
     platform       x86_64-pc-linux-gnu         
     arch           x86_64                      
     os             linux-gnu                   
     system         x86_64, linux-gnu           
     status                                     
     major          4                           
     minor          3.1                         
     year           2023                        
     month          06                          
     day            16                          
     svn rev        84548                       
     language       R                           
     version.string R version 4.3.1 (2023-06-16)
     nickname       Beagle Scouts   

RxpsG installation:

1. Click on the RxpsG_xx.xx.tar.gz package and download. Exit the unzipping procedure if it starts automatically.
 
2. Control in the Dowloads folder the RxpsG_xx.xx.tar.gz package is present (it could be the .gz extension is lacking do not worry).

3. Run RStudio 

4. Under RStudio copy and paste the following command to INSTALL THE REQUIRED LIBRARIES: 
                        
   install.packages(c("digest", "import", "latticeExtra", "memoise", "minpack.lm", "signal"), 
               repos = "https://cloud.r-project.org", dependencies=TRUE)                                    

   
   Control that installation proceed correctly without errors;

5. To INSTALL RxpsG_3.1 copy and paste the following command:

   install.packages("C:/Path-To-Tar.Gz/RxpsG_3.1.tar.gz", type = "source", dependencies=TRUE)

   where Path-To-Tar.Gz is the path to the dowloaded RxpsG_2.3-2.tar.gz file.
   
6. To run RxpsG, in RStudio select the PACKAGE pain (generally on the right of the RStudio console) 
   and select the RxpsG package. This should automatically load and run the software.
   
