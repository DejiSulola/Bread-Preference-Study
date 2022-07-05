

check.packages <- function(pkg){
  new.pkg <- pkg [!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only=TRUE)
}

packages <- c("mlogit","gmnl","apollo","dplyr","tidyr","tibble")
check.packages(packages)

#####clear memory
rm(list = ls())

###load Apollo library
library(apollo)

apollo_initialise() ###initialise code