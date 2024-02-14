
## fix package loading issue

#.libPaths() #check library paths, there are two paths!!
# .libPaths()[-1] only use the second path? but i do not have access to this lib!

# install package in predefined path?
#install.packages("text", lib = .libPaths()[1])

# load text package from specific path
#library("text", lib.loc=.libPaths()[1]) # this gives me that weird error !
#library("text", lib.loc=.libPaths()[-1]) # there is no text package in that path!


# install text package to predefined 

#install.packages("text", )

# why do some packages load wo any issues and text does not?
# testing it with the quanteda package!
# install works! but same issue when loading the packages, maybe this happens when installing new packages??

library(text)

# i think i need to fix this through the terminal by setting the correct path
# problem: i have no permission to update!

# the lsitdc package thing is already in the latest version

# there are two paths to install r packages
# maybe the path i do not have root rights is not updated

# solution: b) make sure Conda's copy of libstdc++ is first in your library path, e.g.,:

# TO DO:I need to update that version in the given path! 


## this has worked in the console!!
# https://stackoverflow.com/questions/38241122/ubuntu-remove-libpaths-in-r

# so one way to fix it is to give it an empty string when you start R (cannot be done from within R):
#   
#   # in bash on the command line:
#   $ R_LIBS_SITE=" " R
# 
# # in R
# R> .libPaths()
# [1] "/usr/lib/R/library"
# The way to get this to work with RStudio is by creating a ~/.Renviron file with at least the following:
#   
#   R_LIBS_SITE=" "
# That done, you should not have to do anything further to remove the secondary site library paths from .libPaths():
#   
#   R> .libPaths()
# [1] "/usr/lib/R/library"


