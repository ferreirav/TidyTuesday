### This is a base script to be used every time it is needed to update RStudio #

# First, we need to create a list of the current packages installed #
# and then will run this script again in the updated version to download back #
# the missing packages!!! #


          ### OLD ### ACTIONS PERFORMED IN OLD VERSION ### OLD ###
#______________________________________________________________________________#
                      # Saving a list of packages installed #
#______________________________________________________________________________#

installed <- as.data.frame(installed.packages())

write.csv(installed, "installed_previously_190622.csv")





          ### NEW ### ACTIONS PERFORMED IN NEW VERSION ### NEW ###

#______________________________________________________________________________#
            # Create a list of libraries that need to be installed #
                # and do not come with new version installed #
#______________________________________________________________________________#

installed_previously <- read.csv("installed_previously_190622.csv")

baseR <- as.data.frame(installed.packages())

to_install <- setdiff(installed_previously, baseR)


#______________________________________________________________________________#
                          # Final installation #
#______________________________________________________________________________#

install.packages(to_install)



