#####################################################################################
#   
#   Data input: The model dataset women's file (IR file)
#   Author: Albert Lutakome
# 	Date Last Modified: September 11, 2023 by Albert Lutakome
# 
# Notes:
# 
# 
# * For the code below to work, you must save the dataset in same folder as do file.  
# 
# * Sample research question (for demonstration purposes only):
#   After controlling for other social-demographic variables (wealth, age, residence and region), does education 
#   have an effect on condom use by women aged 15-49?  
# !!!! More instructions in the do file
#####################################################################################

# install and load the packages
install.packages("haven") # used to open dataset with read_dta
install.packages("here") # used for path of your project. Save your datafile in this folder path.
install.packages("dplyr") # used for creating new variables
install.packages("labelled") # used for haven labelled variable creation
install.packages("survey") # used to set survey design
install.packages("expss") # for creating tables with Haven labelled data
install.packages("xlsx") # used to export tables to excel

library(naniar)   # to use replace_with_na function
library(haven)
library(here)
library(dplyr)
library(labelled) 
library(survey)
library(expss)    
library(xlsx)

here()

##################################################################################

# *******************************************************************************
# Preparing the data.

# Merging IR and PR file

# Step 1. Open secondary dataset
PRdata <-  read_dta(here("KEPR8AFL.DTA"))

# Step 2. Rename IDs (uniquely identify a case)
PRdata <- PRdata %>%
  mutate(v001=hv001) %>%
  mutate(v002=hv002) %>%
  mutate(v003=hvidx) 

# Step 3. Open master dataset
IRdata <-  read_dta(here("KEIR8AFL.DTA"))

# Step 4. Merge two datasets
IRPRdata <- merge(IRdata,PRdata,by=c("v001", "v002", "v003"))

#save merged data file 
#library(foreign) 
#write.dta(IRPRdata, here("KEIRPR8AFL.DTA"))

# Check our merged data and proceed if all is well. 
head(IRPRdata)

##############################
### Variable Recoding 
##############################
  
### Outcome variable ### 
# Condom use among women.
  # Recode to exclude those with inconsistent data. 
 
IRPRdata <- IRPRdata %>%
  mutate(condomuse = case_when(
    v761==0 ~ 0,
    v761==1 ~ 1,
    v531==0 ~ 99),
    condomuse = set_value_labels(condomuse, labels = c("Did not use condom"=0, "Used condom"=1)),
    condomuse = set_variable_labels(condomuse, label = "Condom use at last sexual intercourse in the past 12 months"))%>%
  replace_with_na(replace = list(condomuse = c(99)))

# education: recode v106 to combine primary and higher categories
IRPRdata <- IRPRdata %>%
    mutate(educ =
             case_when(v106 ==0 ~ 0,
                       v106 >=1 ~ 1)) %>%
    set_value_labels(educ = c(none = 0, "primary or higher" = 1)) %>%
    set_variable_labels(educ ="education level")

# Social-demographic variables.
  
# wealth quantile: use v190
# age: use v013
# place of residence: use v025
# region: use v024

# creating the sampling weight variable. We need it in our reduced dataset or else there will be an error.
  IRPRdata$wt <- IRPRdata$v005/1000000
  
vars <- c("condomuse","v013","educ","v502", "v190","v025","v024","v005", "v007", "wt","v021","v022","v001","v002","v003")

cudata <- IRPRdata %>%
  filter(v766b!=0) %>%
  select(all_of(vars))
# Filter only where there was intercourse in last 12 months. 

##############################################
### Set the survey design using svydesign ###
##############################################

# attaching data 
attach(cudata)

### Setting the survey design using the svydesign command###
# the survey design will be saved in an object named mysurvey. 
mysurvey<-svydesign(id=cudata$v021, data=cudata, strata=cudata$v022,  weight=cudata$wt, nest=T)
options(survey.lonely.psu="adjust") 
  
####################################################
###  Statistics and Crosstabulations ### 
###################################################

#### Descriptive table ####
# you can use the following code for checking the proportions of a variable
prop.table(svytable(~condomuse, mysurvey))

# all women in reduced dataset
cudata <- cudata %>% mutate(cu_all = case_when(v007>0  ~ "all"))

# Descriptive table:
# set expss package options to show one decimal place
expss_digits(digits=1)

tab<-  cudata %>% 
  cross_cpct(
    cell_vars = list(condomuse,v013,educ,v190,v025),
    col_vars = list(cu_all),
    weight = wt,
    expss_digits(digits=1)) 

write.xlsx(tab, "tables.xlsx", sheetName = "table1")
# note that this table gives you weighted percentages but does not produce confidence intervals. 

##### Crosstabulations of each variable with the outcome variables (Table 2 of your results) #####

# Crosstabulation of condomuse and place of residence (v025) and chi-square test.
svyby(~condomuse, by = ~v025 , design=mysurvey, FUN=svymean, vartype=c("se", "ci"))

svychisq(~condomuse+v025, mysurvey)

# Crosstabulation of condomuse for all variables at once: 

# Creating a vector of all variables
variables <- c("v013","educ", "v190","v025","v024") 
 
# Empty list for crosstabulation results
results <- list()  

# Loop through the variables
for (var in variables) {
  
  crosstab <- svyby(~ condomuse, by = as.formula(paste("~", var)), design = mysurvey, FUN = svymean, vartype = c("se", "ci"))
  
  chi_square <- svychisq(as.formula(paste("~ condomuse +", var)), design = mysurvey)
  
  # Store results in list
  results[[var]] <- list(crosstab = crosstab, chi_square = chi_square)
}

# Access the crosstabulation results for each variable
for (var in variables) {
  print(paste("Crosstabulation for", var))
  print(results[[var]])
}

# Interpretation:
# The results of the crosstabulation show that all variables were significantly associated with condom use
#Age: There is a significant difference condom use between age groups, rural and urban populations, 
#the educated and uneducated, and between the wealthy and the poor.

############################################################################################

####  regression #### 
#### Unadjusted regression ####
# we fit an unadjusted logistic regression 
reg1 <- svyglm(condomuse~ 1 + educ , design=mysurvey, family=binomial(link="logit"))

# to see the results
summary(reg1)

# Odds ratios inspection
ORreg1 <- exp(reg1$coefficients )
ORreg1

# Interpretation:
# The unadjusted result shows that educated women (primary and above) aged 15-49 years who report to have had intercourse in the last 12 months 
# have 4.7 times the odds of using a condom compared to women with no education.

# now we will include the background variables social-demographic in our varaiables in our model.
reg2 <- svyglm(condomuse~ 1 + educ + as.factor(v013) 
               + as.factor(v190)+ as.factor(v025)+ as.factor(v024),  
               design=mysurvey, family=binomial(link="logit"))


# to see the results
summary(reg2)

# Odds ratios inspection
ORreg2 <- exp(reg2$coefficients )
ORreg2

# Interpretation:
# After controlling for other variables, education was found to be significantly associated with condom use
# among women between 15-49 years who report to have had intercourse in the last 12 months. Educated women (primary and above)   
# had 1.55 times higher odds using condoms compared to un-educated women.


#Note on multi-collinearity: 
# In practical cases, we need to check for multi-collinearity within the independent variables before fitting the model.  
correl <- cudata[, variables]
# Calculate the correlation matrix
cor_matrix <- cor(correl)
print(cor_matrix)

# to produce a correlation plot
install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix)
# For instance, there is a relatively high correlation between v025 and v19. In practical situations, to avoid multicollinearity, it is advisable not to include both variables  
# in the model at the same time to better interpret the contribution of each to the outcome variable.  



