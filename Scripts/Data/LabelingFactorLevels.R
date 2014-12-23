#  The following script declares the factor labels to be used with  **dsL**
#
###########################################################################

#### SCALES  ####

## sex ###################################################################
sexLevels<- c(1,2,0)
sexLabels<- c("Male","Female","No Information")

varlist<-c("sex")
for(i in varlist){
  dsF[,paste0(i,"F")]<-ordered(dsF[,i],
                                levels = sexLevels,
                                labels = sexLabels)
}
## race ##################################################################
raceLevels<- c(1,2,3,4)
raceLabels<- c("Black","Hispanic","Mixed (Non-H)","Non-B/Non-H")

varlist<-c("race")
for(i in varlist){
  dsF[,paste0(i,"F")]<-ordered(dsF[,i],
                                levels = raceLevels,
                                labels = raceLabels)
}
## birth month ###########################################################
bmonthLevels<- c(1:12)
bmonthLabels<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

varlist<-c("bmonth")
for(i in varlist){
  dsF[,paste0(i,"F")]<-ordered(dsF[,i],
                                levels = bmonthLevels,
                                labels = bmonthLabels)
}

## church attendance #####################################################
attendLevels<- c(1:8)
attendLabelsShort<-c("Never",
                     "Once or Twice",
                     "< once/month",
                     "~ once/month",
                     "~ twice/month",
                     "~ once/week",
                     "Several times/week",
                     "Everyday")
attendLabels<-c( "Never",
                 "Once or Twice",
                 "Less than once/month",
                 "About once/month",
                 "About twice/month",
                 "About once/week",
                 "Several times/week",
                 "Everyday")
varlist<- c("attend")
for(i in varlist){
  dsF[,paste0(i,"F")]<-ordered(dsF[,i],
                                levels = attendLevels,
                                labels = attendLabels)
}
# The color scale developed for this variable
attcol8<-c("Never"="#4575b4",
           "Once or Twice"="#74add1",
           "Less than once/month"="#abd9e9",
           "About once/month"="#e0f3f8",
           "About twice/month"="#fee090",
           "About once/week"="#fdae61",
           "Several times/week"="#f46d43",
           "Everyday"="#d73027")


# ADD MORE VARIABLES HERE

# Now data set contains a set of ghost Factor variables that can be used in graphing and modeling