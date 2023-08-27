library(tidyverse)
CO2_tibble <- as_tibble(CO2)

# building tibble from scratch 
names <- c("Mike","Matt","Chris","Ricky")
birth_year <- c("1996","2000","1972","2003")
eyecolor <- c("blue","black","black","blue")

people <- tibble(names,birth_year,eyecolor)
head(people)

# using $ 
people$eyecolor
# using [[]] with column name
people[['eyecolor']]
# using [[]] with column index
people[[3]]

# people with black eyes 
filter(people,eyecolor=='black')
# people born after 1972 
filter(people,birth_year > 1972)
# people with blue eyes born after 1972 
filter(people,eyecolor=='blue' & birth_year >1972)

library(tidyverse)
inspections <- read_csv('inspections.csv')
glimpse(inspections)

# naming the columns so that we don't have spaces 
names <- c('inspectionID','RestaurantName' , 'OtherName','LicenseNumber','FacilityType','Risk',
           'Address','City','State','ZIP','InspectionDate','InspectionType','Results','Violations',
           'Latitude' , 'Longitude','Location')

inspections <- read_csv('inspections.csv',col_names=names)
glimpse(inspections)

library(tidyverse)
inpatient <- read_tsv("C:/Users/Atolb/inpatient.tsv")
glimpse(inpatient)

# solving the same problem as before space between columns 
names <- c("DRG","ProviderID","Name","Address","City","State","ZIP","Region","Discharges",
           "AverageCharges" ,"AverageTotalPayments","AverageMedicarePayments"
           )
inpatient <- read_tsv("C:/Users/Atolb/inpatient.tsv",skip=1,col_names=names)
summary(inpatient)

types <- 'ciccccccinnn'
inpatient <- read_tsv("C:/Users/Atolb/inpatient.tsv",skip=1,col_names=names,col_types=types)
summary(inpatient) 


stoppages <- read_delim('workstoppages.txt', delim = '^')
glimpse(stoppages)

inspections <- read_delim("inspections.csv",delim=',')

head(inspections,1)

inpatient <- read_delim('inpatient.tsv',delim='^')

names <- c('Name','Title','Department','Salary')
lengths <- c(32,50,24,NA)
widths <- fwf_widths(lengths,names)
employees <- read_fwf('chicagoemployees.txt',widths)
glimpse(employees)

library(readxl)
breakfast <- read_excel('breakfast.xlsx',skip=3)
glimpse(breakfast)

names <- c('Year','FreeStudents','ReducedStudents','PaidStudents','TotalStudents','MealsServed','PercentFree')

breakfast <- read_excel('breakfast.xlsx',skip=5,col_names=names)
glimpse(breakfast)

# doing some data cleaning
breakfast <-  breakfast %>%  
mutate(FreeStudents = FreeStudents * 1000000,
       ReducedStudents = ReducedStudents * 1000000,
       PaidStudents = PaidStudents * 1000000,
       TotalStudents = TotalStudents * 1000000,
       MealsServed = MealsServed * 1000000,
       PercentFree = PercentFree /100)
head(breakfast)

library(tidyverse)
library(readr)
pew <- read_csv("C:/Users/Atolb/pew.csv")

head(pew,1)
pew.long <- pivot_longer(pew,!religion,names_to='income',values_to='freq')
head(pew.long,1)

weather <- read_csv("C:/Users/Atolb/mexicanweather.csv")

head(weather,1)
weather.wide <- pivot_wider(weather,names_from=element,values_from=value)
head(weather.wide,1)

foulshots <- c(18, 22, 15, 13, 5)

# I can use the sum() function to total them up

sum(foulshots)

# But what if they are read in as strings?

foulshot_strings <- c("18", "22", "15", "13", "5")
sum(foulshot_strings)

class(foulshot_strings)

foulshot_converted <- as.numeric(foulshot_strings)
class(foulshot_converted)
sum(foulshot_converted)

# checking the data types 
is.numeric(foulshots)
is.character(foulshots)
is.numeric(foulshot_strings)
is.character(foulshot_strings)

weather <- read_csv("C:/Users/Atolb/mexicanweather.csv")

head(weather,1)

# We can use lubridate functions to extract elements of the date
library(lubridate)
weather <- weather %>%
  mutate(year=year(date), month=month(date), day=day(date))
head(weather,1)

# We can also extract some derived values such as the weekday
wday("2018-04-01")

# or day of the year
yday("2018-04-01")

# We can also use lubridate to create date values out of different strings
mdy("04/01/2018")
mdy("04/01/18")
dmy("04/01/18")
ymd("2018-04-01")


library(readr)
library(ggplot2)

names <- c("DRG","ProviderID","Name","Address","City","State","ZIP","Region","Discharges", # nolint
           "AverageCharges" ,"AverageTotalPayments","AverageMedicarePayments"
           )
types <- 'ciccccccinnn' # nolint
inpatient <- read_tsv("C:/Users/Atolb/inpatient.tsv",skip=1,col_names=names,col_types=types) # nolint
summary(inpatient)

# Let's look at a histogram
ggplot(inpatient,aes(x=AverageCharges)) + geom_histogram()


# What if we change the limit of the y axis
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges)) +
  coord_cartesian(ylim=c(0,25))

# We could also view this with a single boxplot
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes("charges",AverageCharges))

# Plenty to investigate here, but let's dig in to those over $500,000
highCharges <- inpatient %>%
filter(AverageCharges > 500000)

unique(highCharges$DRG)

ggplot(data=highCharges) + 
  geom_point(mapping=aes(DRG,AverageCharges)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))


names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('C:/users/Atolb/inspections.csv', 
                        col_names=names, skip=1)

summary(inspections)

# Filtering for missing values 
# Which inspections have NA values for license?
unlicensed <- inspections %>%
  filter(is.na(License))

### special values #### 
badmath <- c(1,2,3,4/0,0/0,NA)
badmath
is.na(badmath)
is.nan(badmath)
is.infinite(badmath)
is.finite(badmath)

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('C:/users/Atolb/inpatient.tsv', col_names = names, skip=1, col_types = types)


# Take a look at the diagnosis-related group unique values
unique(inpatient$DRG)
# we can separate the diagnosis-related group into code and description.

# Let's try separating this on the hyphen
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),'-')

# What's going on with those warning rows?  Let's look at row 45894
inpatient$DRG[45894]

# Let's separate with character position instead
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separate)


# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ")

# Let's look at the data
glimpse(regional_inspections)


# Whoops. I didn't want to DELETE the City and State columns.  Let's try again.
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)
glimpse(regional_inspections)

library(stringr)
# convert to upper case using str_to_upper
names <- c("mike","peter","sandra")
names_upper <- str_to_upper(names)
names_upper

# convert to lower case using str_to_lower 
names_lower <- str_to_lower(names_upper)
names_lower
# convert to title case using str_to_title 
names_title <- str_to_title(names_upper)
names_title

regional_inspections <- regional_inspections %>%
mutate(Region=str_to_upper(Region))
unique(regional_inspections$Region)

# Trim the DRGcode field
inpatient_separate <- inpatient_separate %>%
  mutate(DRGcode=str_trim(DRGcode))

glimpse(inpatient_separate)

# The DRGdescription field has a hyphen in front so we need to do something different
inpatient_separate <- inpatient_separate %>%
  mutate(DRGdescription=str_sub(DRGdescription, 3))

glimpse(inpatient_separate)

