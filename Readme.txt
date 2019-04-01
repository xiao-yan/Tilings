########  ‘Tilings’ Package for Unix/Linux ########
########    Readme.txt    ########

###### Note for user ######

#### How to use the package? 
# 1) Put it into a path which Mathematica would recognise (e.g. ~/Library/Mathematica/Applications).
# 2) Make sure the name is Tilings.m
# 3) Import the package as: <<Tilings`
# 4) ?Tilings will summarise the modules of the package.
# 5) ?<NameOfModule> will output the detailed usage of the module.
# 6) The file TilingsDemonstration.nb is a sample Mathematica file of using the package.


###### For Developer ######

#### What to improve about the package?
# 1) For the purpose of research, the code is purely in Mathematica style, namely it involves lots of virtual functions and compact coding (such as & @ # and their combinations). If necessary we will make the code much more readable.
# 2) More functionalities. 

#### DimerSystem ####

#### 0.0 - 2017/02/01
# Three versions of HiggsingDimerSU. 
# Fixed the following problem: when using Parallel functions in a package, must firstly distribute definitions into different kernels. 

#### 0.1 - 2017/02/01
# One version of HiggsingDimerSU. Chopped off the results of Zig-zag paths and consistency  flag.
# Requires documentation.

#### 0.2 - 2017/02/01
# Added on the simple usages of each module. 
# Need to read them again and add the description of the whole package and a demo.

#### Tilings ####

#### 0.1 - 2017/02/06
# Re-written part of the documentation. Needs to fill in the paper information.
# Added on a module DemonDimer[], used to guide the user by a simple example.

### GLSMData.nb ###
This file contains all the charge matrices Q_t for all diagrams of area 6, 7 and 8. This is a Mathematica notebook file and it requires Mathematica to open and all the data are grouped by diagram and are collapsed into a single group for each diagram.
 

