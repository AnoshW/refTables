#script to make comparisions tables from spreadsheets of references
#edit to fit different spreadsheets
#open fine working directory

library(tidyverse)

df <- readxl::read_xlsx("name of file here.elsx", "specific sheet (optional)")%>%   #point to excel spreadsheet | name, specific sheet
  rowid_to_column("ID")%>%  #add an ID column
  subset(author!="authorName")%>%    #avoid entries by specific authors
  mutate(ref = paste(author, " (", year ,")", sep = ""))    #make a new column combining author and year into one saprated with a , and inclosed in brackets
names(dfall) <- make.names(names(dfall), unique = T)    #replace spaces with unique names

tableOne <- df%>%
  subset(type!="entry")%>%  #only show everything but remove rows that match "entry"
  distinct(author, year, title, .keep_all = T)%>%   
  group_by(column1, column2)%>%
  summarise(ref = paste(ref, collapse = ", "), n = str_count(ref, ",") + 1)%>%  #counts the number of refs and adds the count to a new column
  select(column2, column1, n, ref)    #order the columns in a particular order

write.csv(tableOne, "tableOne.csv") #make and write tableOne to new csv

#table for min, max and mean for x and y combo. Current values are for a specific dataframe
tableSpArch <- df%>%
  subset(type!="MFC")%>%
  select(anode.electrode, species, max.power.density, type, ref)
tableSpArch$species <- str_replace_na(tableSpArch$species, "N/A")
tableSpArch$max.power.density <- str_replace_na(tableSpArch$max.power.density, "not reported")
tableSpArch <- subset(tableSpArch, max.power.density!="not reported")
tableSpArch$max.power.density <- as.numeric(tableSpArch$max.power.density)
boxdfAnSp <- tableSpArch    #dataframe can be used to make box charts with this hence saparated
tableSpArch <- tableSpArch%>%   #table with min, max, mean of x and y combo
  group_by(anode.electrode, species, .drop = FALSE)%>%
  summarise(ref = paste(ref, collapse = ", "), n = str_count(ref, ",") + 1, mean = mean(max.power.density), min = min(max.power.density), max = max(max.power.density))
write.csv(tableSpArch, "tableSpArch.csv")

#table grouping species and max power density
tableSpMaxPower <- boxdfAnSp%>%
  group_by(species, max.power.density, ref, .drop = F)%>%
  summarise(ref = paste(ref, collapse = ", "))
write.csv(tableSpMaxPower, "tabeSpMaxPower.csv")

#how many times was the same electrode material used for both anode and cathode
dfall %>%
  subset(type!="MFC")%>%
  count(anode.electrode==cathode.electrode)

#count times each species was referenced
tabSpeciesN <- boxdfAnSp %>%
  subset(type!="MFC")%>%
  count(species)
write.csv(tabSpeciesN, "tableSpeciesN.csv")