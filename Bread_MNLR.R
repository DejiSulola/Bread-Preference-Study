
check.packages <- function(pkg){
  new.pkg <- pkg [!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only=TRUE)
}

packages <- c("mlogit","gmnl","apollo","dplyr","tidyr","tibble")
check.packages(packages)

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#####clear memory
rm(list = ls())

###load Apollo library
library(apollo)

apollo_initialise() ###initialise code

### Set core controls
apollo_control = list(
  modelName       = "Bread_MNL",
  modelDescr      = " MNL model on Bread choice data",
  indivID         = "id",
  outputDirectory = "output"
)
### Loading data
bread_data <- read.csv("bread_data.csv",header=TRUE)

### Add Identifier for individual record
bread_data <- bread_data %>%
  add_column(id = 1:188,
             .before = 1)
### Convert to Long format
#Change data format from wide to long
bread_data_Long <- bread_data %>% gather(ChoiceTask,Choice, ct1,ct2,ct3,ct4,ct5,ct6,ct7,ct8)

bread_data_Long <- bread_data_Long[order(bread_data_Long$id),]

#Subset Blocks 1 and 2 from Dataset
dtBlock1 <- subset(bread_data_Long, group_number == 1)

dtBlock2 <- subset(bread_data_Long, group_number == 2)

#Load Attributes Codes Data
dtBlock1_Attributes <- read.csv("bread_block_1_attributes.csv", header = TRUE)
dtBlock2__Attributes <- read.csv("bread_block_2_attributes.csv", header = TRUE)
view(dtBlock2__Attributes)

##Selecting the columns needed
dtBlock2_Attributes <- dtBlock2__Attributes[,3:29]
view(dtBlock2_Attributes)

#Repeat each Attribute Codes for the number of respondents
dtBlock1_Attributes <- dtBlock1_Attributes[rep(seq_len(nrow(dtBlock1_Attributes)), each = 84), ]
dtBlock2_Attributes <- dtBlock2_Attributes[rep(seq_len(nrow(dtBlock2_Attributes)), each = 104), ]
#Attach Ids to the Attribute Code Datasets
dtAttributeBlock1_rep <- data.frame(id = rep(1:84, 8), dtBlock1_Attributes)
dtAttributeBlock1_rep <- dtAttributeBlock1_rep[order(dtAttributeBlock1_rep$id),]

dtAttributeBlock2_rep <- data.frame(id = rep(85:188, 8), dtBlock2_Attributes)
dtAttributeBlock2_rep <- dtAttributeBlock2_rep[order(dtAttributeBlock2_rep$id),]

#Merge Attribute Codes with Observational Data
dtBlock1_With_Attributes <- data.frame(dtBlock1, dtAttributeBlock1_rep)

dtBlock2_With_Attributes <- data.frame(dtBlock2, dtAttributeBlock2_rep)

database <- rbind(dtBlock1_With_Attributes, dtBlock2_With_Attributes)

##############################




