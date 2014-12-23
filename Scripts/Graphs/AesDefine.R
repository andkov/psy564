# Definitions of of aesthetics used in reports


# COLORS
## the colors for response categories of item "attend"
attendCol8<-c("Never"="#4575b4",
           "Once or Twice"="#74add1",
           "Less than once/month"="#abd9e9",
           "About once/month"="#e0f3f8",
           "About twice/month"="#fee090",
           "About once/week"="#fdae61",
           "Several times/week"="#f46d43",
           "Everyday"="#d73027")

## The color of cohorts
byearCol5<- c("1980"="#9ecae1",
               "1981"="#6baed6",
               "1982"="#4292c6",
               "1983"="#2171b5",
               "1984"="#084594")


## the color of missing values
NACol<- "#f9f9f9" #"#636363"


# SIZES
# guides and legends, but not axes and title
baseSize<- 12


# SHAPES

# GROUPS

# Variables that DO NOT change with time, TI - time invariant
TIvars<-c("sample", "id", "sex","race", "bmonth","byear",  'attendPR', "relprefPR", "relraisedPR")

# Variables that DO change with time, TV - time variant
TVvars<- c("agemon", "ageyear", "famrel", "attend",
           "values","todo", "obeyed","pray",  "decisions", 
           "relpref", "bornagain", "faith", 
           "calm", "blue", "happy", "depressed","nervous",
           "tv", "computer", "internet")

# Service
counterAge<- c("0"="white","1"="#23c8b2")

basicDark<- "#4f8a83"

####
# All defined elements
aesDefs<- list("attendCol8" = attendCol8, 
                      "byearCol5" = byearCol5,
                      "NACol" = NACol, 
                      "baseSize"= baseSize,
                      "TIvars"=TIvars,
                      "TVvars"=TVvars)

colorFixed <- "blue"
colorRandom <- "red"
