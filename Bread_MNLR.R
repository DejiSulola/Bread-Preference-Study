
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
database$int_student <- as.numeric(database$germany_born==0)
database$treated <- as.numeric(database$treatment_number==2)

#Subset Datasets
Treated_Database <- subset(database, treatment_number == 2)
Non_Treated_Database <- subset(database, treatment_number == 1)
german_Database <- subset(database, germany_born == 1)
inter_student_Database <- subset(database, germany_born == 0)

#Transform Complete Database to mlogit format
choice_data_mlogit <- mlogit.data(
  database,
  choice = "Choice",
  shape = "wide",
  varying = 43:69,
  sep = "_",
  id.var = "id"
)

#Transform Treated Database to mlogit format
Treated_choice_data_mlogit <- mlogit.data(
  Treated_Database,
  choice = "Choice",
  shape = "wide",
  varying = 43:69,
  sep = "_",
  id.var = "id"
)

#Transform Non-Treated Database to mlogit format
Non_Treated_choice_data_mlogit <- mlogit.data(
  Non_Treated_Database,
  choice = "Choice",
  shape = "wide",
  varying = 43:69,
  sep = "_",
  id.var = "id"
)


#Transform Treated Database to mlogit format
German_choice_data_mlogit <- mlogit.data(
  german_Database,
  choice = "Choice",
  shape = "wide",
  varying = 43:69,
  sep = "_",
  id.var = "id"
)

#Transform Non-Treated Database to mlogit format
Int_Student_choice_data_mlogit <- mlogit.data(
  inter_student_Database,
  choice = "Choice",
  shape = "wide",
  varying = 43:69,
  sep = "_",
  id.var = "id"
)

# Create New Variables including those for Alternative specific constants
choice_data_mlogit$asc_a <- as.numeric(choice_data_mlogit$alt==1)
choice_data_mlogit$asc_b <- as.numeric(choice_data_mlogit$alt==2)
choice_data_mlogit$asc_c <- as.numeric(choice_data_mlogit$alt==3)

Treated_choice_data_mlogit$asc_a <- as.numeric(Treated_choice_data_mlogit$alt==1)
Treated_choice_data_mlogit$asc_b <- as.numeric(Treated_choice_data_mlogit$alt==2)
Treated_choice_data_mlogit$asc_c <- as.numeric(Treated_choice_data_mlogit$alt==3)

Non_Treated_choice_data_mlogit$asc_a <- as.numeric(Non_Treated_choice_data_mlogit$alt==1)
Non_Treated_choice_data_mlogit$asc_b <- as.numeric(Non_Treated_choice_data_mlogit$alt==2)
Non_Treated_choice_data_mlogit$asc_c <- as.numeric(Non_Treated_choice_data_mlogit$alt==3)


German_choice_data_mlogit$asc_a <- as.numeric(German_choice_data_mlogit$alt==1)
German_choice_data_mlogit$asc_b <- as.numeric(German_choice_data_mlogit$alt==2)
German_choice_data_mlogit$asc_c <- as.numeric(German_choice_data_mlogit$alt==3)

Int_Student_choice_data_mlogit$asc_a <- as.numeric(Int_Student_choice_data_mlogit$alt==1)
Int_Student_choice_data_mlogit$asc_b <- as.numeric(Int_Student_choice_data_mlogit$alt==2)
Int_Student_choice_data_mlogit$asc_c <- as.numeric(Int_Student_choice_data_mlogit$alt==3)


### Run a Conditional Logit Model with non-linear effects for all variables
Models_Non_Linear.cl <-
  gmnl(
    formula = Choice ~ wr + ww + rr + nf + ns + nsf + pr + asc_a + asc_b|0,
    data = choice_data_mlogit,
    reflevel = "3"
  )
summary(Models_Non_Linear.cl)

### Run the Random Parameter Model with Unobserved Heterogeneity
Models_Non_Linear.MIXL <-
  gmnl(
    formula = Choice ~ wr + ww + rr + nf + ns + nsf + pr + asc_a + asc_b |0,
    data = choice_data_mlogit,
    model = "mixl",
    ranp = c(
      wr = "n",
      ww = "n",
      rr = "n",
      nf = "n",
      ns = "n",
      nsf = "n",
      pr = "n"
    ),
    reflevel = "3",
    R = 1000,
    panel = TRUE,
    print.init = TRUE,
    method = "bfgs",
    correlation = FALSE
  )
summary(Models_Non_Linear.MIXL)

#compute willingness to pay estimates based on the Mixed logit Model
wtp.gmnl(Models_Non_Linear.MIXL, wrt = "pr")


### Run the Random Parameter Model with Observed Heterogeneity
Models_Non_Linear_OH.MIXL <-
  gmnl(
    formula = Choice ~ wr + ww + rr + nf + ns + nsf + pr + asc_a + asc_b | 0 | 0 | int_student + treated + Gender + language_proficiency - 1,
    data = choice_data_mlogit,
    model = "mixl",
    ranp = c(
      wr = "n",
      ww = "n",
      rr = "n",
      nf = "n",
      ns = "n",
      nsf = "n",
      pr = "n"
    ),
    mvar = list(
      wr = c("int_student"), 
      ww = c("int_student"),
      rr = c("int_student")
    ),
    reflevel = "3",
    R = 1000,
    panel = TRUE,
    print.init = TRUE,
    method = "bfgs",
    correlation = FALSE
  )
summary(Models_Non_Linear_OH.MIXL)

##We can also try to run a descriptive statistics before running into observing contributing heterogeneity factors.
#descriptive statistics
install.packages("skimr")
library(skimr)
skim(bread_data)


