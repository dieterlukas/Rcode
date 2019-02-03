#---------------------------------------------------------------------------------------------------------------------------
#This code can be used to check pedigree files for potential errors before using them in analyses. Files with the correct input format can for example be found on kinsources.net (though these files have been checked)

#For questions please contact dieter_lukas@eva.mpg.de




#---------------------------------------------------------------------------------------------------------------------------
#Getting started

#load supporting libraries
library(dplyr)
library(stringr)

#load dataset (in the following called 'd')

#Variable names are
names(d)
#  [1] "runningnumber" "Name"          "ego"           "newyob"        "month"         "day"           "YOB"          
#  [8] "YOD"           "Sex"           "FID"           "MID"           "em"            "HSE"           "dEAD"         
# [15] "born_in_vil"   "note" 

# In this file, name is the written name, ego is intended to be a unique identifier, sex is coded as (0=female,1=male),FID is the unique identifier of the father of the individual (though this dataset also includes written name entries), MID is the unique identifier of the mother of the individual (though this dataset also includes written name entries), newyob is the updated year of birth and in case there is an entry here it takes precedent over YOB, HSE reflects household membership, dead (1=dead,0=alive), born_in_vil (1= individual born in this village, 0= individual born elsewhere), em (1=individual emigrated from the village, 0=individual stayed in the village), note are written additional information, runningnumber is an additional rownumber to separate each entry (since some individuals have the same identifier assigned at ego and other individuals are repeated with multiple assigned identifiers)

#for other datasets, the crucial variables are the unique identifier "ego", the year of birth "YOB", the parent identifications "FID" & "MID", the gender of the individual "Sex", and whether the individual is still alive "dead". This would be sufficient for a list containing entries focused on a single community at a single timepoint. If the study spans multiple communities across longer timespans, additional information is needed: an identification of the community that an individual belongs to and information on whether an individual left or joined a community at a specific timepoint.


#End Setup
#---------------------------------------------------------------------------------------------------------------------------
#Create unitary entries in the dataset


#In order to run additional checks, I first create a new data frame that splits the name entry into a column with the family name and a column with the first name (in this dataset names are listed as familyname_firstname; adjust the split according to the format in your dataset)
newnames<-data.frame(str_split_fixed(d$Name, "_", 2),d$runningnumber)
colnames(newnames)<-c("FamilyName","FirstName","runningnumber")

#This dataset contains a mixture of numbered IDs and written names in the two parent columns. The following commmands create new data frames, one for each sex containing all the numbered IDs (and 'na' entries for all other rows) and one containing all the written entries (and 'na' entries for all other rows)
numbereddads<-data.frame(as.integer(d$FID))
colnames(numbereddads)<-"clearFID"
nameddads<-d$FID
nameddads[is.na(numbereddads)==FALSE]<-"NA"
splitnameddads <-data.frame(str_split_fixed(nameddads, "_", 2))
colnames(splitnameddads)<-c("FamilyName_NamedFather","FirstName_NamedFather")
splitnameddads[splitnameddads$FamilyName_NamedFather=="",]$FamilyName_NamedFather<-"NA"
splitnameddads[splitnameddads$FirstName_NamedFather =="",]$FirstName_NamedFather <-"NA"


numberedmoms<-data.frame(as.integer(d$MID))
colnames(numberedmoms)<-"clearMID"
namedmoms<-d$MID
namedmoms[is.na(numberedmoms)==FALSE]<-"NA"
splitnamedmoms <-data.frame(str_split_fixed(namedmoms, "_", 2))
colnames(splitnamedmoms)<-c("FamilyName_NamedMother","FirstName_NamedMother")
splitnamedmoms[splitnamedmoms $FamilyName_NamedMother=="",]$FamilyName_NamedMother <-"NA"
splitnamedmoms[splitnamedmoms $FirstName_NamedMother =="",]$FirstName_NamedMother <-"NA"




#End preparation
#---------------------------------------------------------------------------------------------------------------------------
#First step of creating a dataset with a single unique ID per individual: remove duplicate IDs




#The first set of checks is to make sure that each individual appears once in the dataset with one unique ID. That means sorting out cases where the same ID in the ego column has been assigned to more than one individual in order to give each individual their own ID; and finding individuals that have multiple entries (appear in multiple rows) with different IDs in order to combine these into a single entry.



#The first step is a search for duplicated IDs, where more than one individual has been assigned the same ID in the ego column
ids <- sort(unique(d$ego))
check <- ids[which(ids %in% d$ego[duplicated(d$ego)])]
check

#-->VISUAL CHECK OF OUTPUT

#This lists the ids are assigned to more than one individual (here all seven IDs have been assigned to two individuals) - they need to be each assigned a different new unique ID

#For each duplicated ID, check whether they appear elsewhere in the dataset to correct these instances too - this involves checking for the ID of the duplicated individuals as well as their names in the Mother/Father ID columns

#-->VISUAL CHECK OF OUTPUT for each of the following lines:

filter(d,FID==check[1])
filter(d,MID==check[1])#0

duplicate<-filter(d,ego==check[1])
duplicate

#Check whether any of these individuals can be the parent of ego the duplicated ego - if yes, assign the correct ego, if no, assign NA



#CONCLUSION: need to assign the individuals with duplication completely new egos
# 			 need to set MID/FID for some individuals as NA



#End First Step
#---------------------------------------------------------------------------------------------------------------------------
#Second step of creating a dataset with a single unique ID per individual: check notes for two individuals being the same


filter(d,note!="NA")

#-->VISUAL CHECK OF OUTPUT

#BASED ON NOTES: remove any duplicate entry



#End Second Step
#---------------------------------------------------------------------------------------------------------------------------
#Third step of creating a dataset with a single unique ID per individual: check entries cases where same individual appears more than once


#First check on 1) name match, 2) birth date match, 3) father match 

e<-mutate(d,combined = paste(Name, day, FID, sep = '_'))
check_names<-e[duplicated(e$combined),]
arrange(e[e$combined %in% check_names$combined,],combined)

#-->VISUAL CHECK OF OUTPUT

#For two of these, the MID differs, so they appear to represent unique entries


#Next check on 1) name match, 2) birth date match, 3) mother match

e<-mutate(d,combined = paste(Name, day, MID, sep = '_'))
check_names<-e[duplicated(e$combined),]
arrange(e[e$combined %in% check_names$combined,],combined)

#-->VISUAL CHECK OF OUTPUT



#Next check on 1) name match and 2) birth date match

e<-mutate(d,combined = paste(Name, day, sep = '_'))
check_names<-e[duplicated(e$combined),]
duplicated_names<-check_names$Name
arrange(e[e$Name %in% duplicated_names,],Name)

#-->VISUAL CHECK OF OUTPUT

#In many dataset it now needs a manual check because sometimes NA appear and sometimes FID/MID are written names to reveal any duplicate entries


#Next check on 1) double parent match, 2) gender match (sort by MID/FID/sex) 3) year of birth match 4) family name match

e<-inner_join(d,newnames,by="runningnumber")
e<-mutate(e,birthyear = pmax(YOB,newyob,na.rm=T))
e<-mutate(e,combined = paste(FamilyName, birthyear, FID, MID, Sex, sep = '_'))

n_occur <- data.frame(table(e$combined))
sum(n_occur[n_occur$Freq > 1,]$Freq)
duplicated_entries<-n_occur[n_occur$Freq > 1,]$Var

e<-e[e$combined %in% duplicated_entries,]
e<-arrange(e,combined)
nrow(e)  

#-->VISUAL CHECK OF OUTPUT

#from the long list, going through the first names and additional information to see whether there is an error in the birth dates or whether individuals could have been entered more than once



#check for married couples - if a woman or a man supposedly have children with multiple partners, are these partners in fact the same individual?

#remove missing values from the mother/father identity columns
f<-d[is.na(d$MID)==FALSE,]
f<-f[is.na(f$FID)==FALSE,]

#filter for individuals who appear as parents with different partners
g<-data.frame(f$MID,f$FID)
g<-distinct(g)
m_occur <- data.frame(table(g$f.MID))
sum(m_occur[m_occur$Freq > 1,]$Freq)
women_linked_with_two_fatherIDs<-m_occur[m_occur $Freq > 1,]$Var1
f_occur <- data.frame(table(g$f.FID))
sum(f_occur[f_occur $Freq > 1,]$Freq)
men_linked_with_two_motherIDs<-f_occur[f_occur $Freq > 1,]$Var1


#create a table that includes all instances where a given woman is linked with more than one man
women_two_men<-g[g$f.MID %in% women_linked_with_two_fatherIDs,]
women_two_men <-arrange(women_two_men, f.MID)

#for the FID, retrieve name
colnames(women_two_men)<-c("MID","ego")
intermediate<-d
intermediate$ego<-as.factor(intermediate$ego)
women_two_men<-left_join(women_two_men,intermediate,by="ego")
colnames(women_two_men)<-c("MID","Name","runningnumber","NewName", "newyob", "month", "day", "YOB", "YOD", "Sex", "FID.father", "MID.father", "em", "HSE", "dEAD", "born_in_vil", "note")
women_two_men<-left_join(women_two_men,d,by="Name")
women_two_men<-select(women_two_men,MID.x,Name,NewName,ego)

#-->VISUAL CHECK OF OUTPUT

# After identifying any duplicated entries, need to check whether they appear as parents anywhere in the FID or the MID column and need to be corrected accordingly.




#create a table that includes all instances where a given man is linked with more than one woman
men_two_women<-g[g$f.FID %in% men_linked_with_two_motherIDs,]
men_two_women <-arrange(men_two_women, f.FID)

#for the MID, retrieve name
colnames(men_two_women)<-c("ego","FID")
men_two_women <-left_join(men_two_women,intermediate,by="ego")
colnames(men_two_women)<-c("Name","FID","runningnumber","NewName", "newyob", "month", "day", "YOB", "YOD", "Sex", "FID.father", "MID.father", "em", "HSE", "dEAD", "born_in_vil", "note")
men_two_women <-left_join(men_two_women,d,by="Name")
men_two_women <-select(men_two_women,FID.x,Name,NewName,ego)

#-->VISUAL CHECK OF OUTPUT

# After identifying any duplicated entries, need to check whether they appear as parents anywhere in the FID or the MID column and need to be corrected accordingly.


# The visual checkes will provide a list of individuals who are entered twice ore more times
# Create a list with all the duplicated individuals
duplicates<-c(...)

# Create a matched list where all the duplicated individuals are linked to their match in the first list, so that duplicate[1] is identical to duplicate_partners[1]
duplicate_partners<-c(...)

list_duplicates<- matrix(data=c(duplicates,duplicate_partners), ncol=2,nrow=...)

list_duplicates<-as.data.frame(list_duplicates)
colnames(list_duplicates)<-c("DuplicatedIndividual","MatchOfDuplicatedIndividual")






#End third step
#---------------------------------------------------------------------------------------------------------------------------
#Fourth step of cleaning: Create a revised datafile





max(as.numeric(d$ego),na.rm=T)
# Check for the highest assigned ego ID, and start new ego IDs above it

#Create a new dataframe that will be used for the output - we will call it 'e' to keep it short
e<-d

#The checks revealed that there are two things to fix: 

# first: assign unique IDs to individuals which are different but had been assigned the same ID:
#			 need to assign the duplicated individuals ccompletely new egos;
# 			 need to set parent ID for runningnumber for individuals that don't match to NA

duplicated_egos<-c(...)

newid<-5001
for (i in 1:length(duplicated_egos)) {
	set_to_replace<-filter(e,ego==duplicated_egos[i])
	e<-filter(e,ego != duplicated_egos[i])
	
		for (k in 1:nrow(set_to_replace)) {
										set_to_replace[k,]$ego<-newid
										newid<-newid+1
										 }
	
	e<-bind_rows(e,set_to_replace)
	
}

e[e$runningnumber=="wrongMID",]$MID<-NA
e[e$runningnumber=="wrongFID",]$MID<-NA


# second: merge the individuals that are the same but appear multiple times with separate IDs- assigning them all new ego IDs, and checking the FID and MID whether they appear there.

temporaryfile<-e

#The individuals are duplicates, so we need to assign the new ID twice, check whether either of the two previous ids appear in the FID/MID columns, or whether the names of the duplicated individuals appear in the FID/MID columns - one individual appears three times so we have to take this into account as well (for example, it might mean there are 60 new individuals - adjust accordingly below). We also need to merge the information for these individuals into a single entry - the approach here is to take the first entry for the runningnumber, the name, and the name, the average date of birth, any known information for sex, dead, emigrated, and born in village, and for the FID and MID any information that is already in numeric ID format and only a written name if no other information is available.

newids<-c((max(e$ego)+1):(max(e$ego)+61))

duplicate_list_for_filtering<-list_duplicates


oldw <- getOption("warn")
options(warn = -1)

for (i in 1:60) {
	
	firstid<-duplicate_list_for_filtering[1,]$DuplicatedIndividual
	secondid<-duplicate_list_for_filtering[duplicate_list_for_filtering$DuplicatedIndividual %in% firstid,]$MatchOfDuplicatedIndividual
	firstname<-e[e$ego %in% firstid,]$Name
	secondname<-e[e$ego %in% secondid,]$Name
	newid<-newids[i]
	
	temporaryfile[temporaryfile$ego %in% firstid,]$ego<-newid
	temporaryfile[temporaryfile$ego %in% secondid,]$ego<-newid
	  
	e[e$ego %in% firstid,]$ego<-newid
	e[e$ego %in% secondid,]$ego<-newid
	
	if(nrow(e[e$MID %in% firstid,]) !=0 ) {e[e$MID %in% firstid,]$MID<-newid}
	if(nrow(e[e$MID %in% secondid,]) !=0 ) {e[e$MID %in% secondid,]$MID<-newid}
	if(nrow(e[e$MID %in% firstname,]) !=0 ) {e[e$MID %in% firstname,]$MID<-newid}
	if(nrow(e[e$MID %in% secondname,]) !=0 ) {e[e$MID %in% secondname,]$MID<-newid}
	
	if(nrow(e[e$FID %in% firstid,]) !=0 ) {e[e$FID %in% firstid,]$FID<-newid}
	if(nrow(e[e$FID %in% secondid,]) !=0 ) {e[e$FID %in% secondid,]$FID<-newid}
	if(nrow(e[e$FID %in% firstname,]) !=0 ) {e[e$FID %in% firstname,]$FID<-newid}
	if(nrow(e[e$FID %in% secondname,]) !=0 ) {e[e$FID %in% secondname,]$FID<-newid}
	
	
	currententries<-e[e$ego==newid,]
	currentego<-summarise_all(currententries,funs(first))
	currentego$newyob<-mean(currententries $newyob,na.rm=T)
	currentego$month<-mean(currententries $month,na.rm=T)
	currentego$day<-mean(currententries $day,na.rm=T)
	currentego$YOB<-mean(currententries $YOB,na.rm=T)
	currentego$YOD<-mean(currententries $YOD,na.rm=T)
	currentego$Sex<-mean(currententries $Sex,na.rm=T)
	currentego$em<-mean(currententries $em,na.rm=T)
	currentego$HSE<-mean(currententries $HSE,na.rm=T)
	currentego$dEAD<-mean(currententries $dEAD,na.rm=T)
	currentego$born_in_vil <-mean(currententries $born_in_vil,na.rm=T)
	momids<-currententries$MID
	momids<-momids[is.na(momids)==F]
	numericmomids<-as.numeric(momids)
	numericmomids <-numericmomids[is.na(numericmomids)==F]
	namedmomids<-momids[!(momids %in% numericmomids)]
	if(length(namedmomids)!=0) {currentego$MID <-namedmomids[1]}
	if(length(numericmomids)!=0) {currentego$MID <-numericmomids[1]}
	currentego$MID<-as.character(currentego$MID)
	dadids<-currententries$FID
	dadids <-dadids[is.na(dadids)==F]
	numericdadids <-as.numeric(dadids)
	numericdadids <-numericdadids[is.na(numericdadids)==F]
	nameddadids<-dadids[!(dadids %in% numericdadids)]
	if(length(nameddadids)!=0) {currentego$FID <-nameddadids[1]}
	if(length(numericdadids)!=0) {currentego$FID <-numericdadids[1]}
	currentego$FID<-as.character(currentego$FID)
	

	e<-filter(e,ego != newid)
	e<-bind_rows(e,currentego)
	
		
	duplicate_list_for_filtering<-duplicate_list_for_filtering[duplicate_list_for_filtering$DuplicatedIndividual != firstid,]
	duplicate_list_for_filtering<-duplicate_list_for_filtering[duplicate_list_for_filtering$DuplicatedIndividual != secondid,]		
}

options(warn = oldw)





#We now have the new vector "e" which contains a single entry for each unique individual, each assigned a unique ID.



#End fourth step
#---------------------------------------------------------------------------------------------------------------------------
#Fifth step of cleaning: check the assignments of parents



#First step needs to be to resolve all the assigmnents and turn them into numerical IDs - after that error checking can happen


#Correct errors identified above

#errors - these matter for assigning identies of fathers, so need to be corrected as part of the process below: 
e[e$MID %in% "ID from above",]$MID<-assign new ID after duplication
#change misspelled names
e[e$MID %in% "old name",]$FID<-"correct name"
#change missing information to NA
e[e$FID %in% "?_",]$FID<-NA


#Next we need to turn the written names in the MID/FID columns into egos   

runningid<-max(e$ego)+1

#This dataset contains a mixture of numbered IDs and written names in the two parent columns. The following commmands create new data frames, one for each sex containing all the numbered IDs (and 'na' entries for all other rows) and one containing all the written entries (and 'na' entries for all other rows)
numbereddads<-data.frame(as.integer(e$FID))
colnames(numbereddads)<-"clearFID"
nameddads<-e$FID
nameddads[is.na(numbereddads)==FALSE]<-"NA"
splitnameddads <-data.frame(str_split_fixed(nameddads, "_", 2))
colnames(splitnameddads)<-c("FamilyName_NamedFather","FirstName_NamedFather")
splitnameddads[splitnameddads$FamilyName_NamedFather=="",]$FamilyName_NamedFather<-"NA"
splitnameddads[splitnameddads$FirstName_NamedFather =="",]$FirstName_NamedFather <-"NA"


numberedmoms<-data.frame(as.integer(e$MID))
colnames(numberedmoms)<-"clearMID"
namedmoms<-e$MID
namedmoms[is.na(numberedmoms)==FALSE]<-"NA"
splitnamedmoms <-data.frame(str_split_fixed(namedmoms, "_", 2))
colnames(splitnamedmoms)<-c("FamilyName_NamedMother","FirstName_NamedMother")
splitnamedmoms[splitnamedmoms $FamilyName_NamedMother=="",]$FamilyName_NamedMother <-"NA"
splitnamedmoms[splitnamedmoms $FirstName_NamedMother =="",]$FirstName_NamedMother <-"NA"




writtenmomidstoreplace<-unique(namedmoms)
writtendadidstoreplace<-unique(nameddads)



#We now need to check for all individuals in the files with written MIDs or FIDs whether a match exists among an individual and if so assign the correct ID from the ego column, or if no matching individual exists create a new entry for this individual at the end, assigning a new ID in the ego column

additionalindividual<-e[1,]
additionalindividual$YOB<-NA
additionalindividual$FID<-NA
additionalindividual$MID<-NA
additionalindividual$em<-NA
additionalindividual$HSE<-NA
additionalindividual$dEAD<-NA
additionalindividual$born_in_vil<-NA

formanualcheck<-additionalindividual


#The following 'counters' are to see how often a written name matched an already existing individual, how often a name matched multiple potential options, and how often there was no match
counter_mom_resolved<-1
counter_mom_absent<-1
counter_mom_multiple<-1
counter_mom_na<-1

counter_dad_resolved<-1
counter_dad_absent<-1
counter_dad_multiple<-1
counter_dad_na<-1

nearmatch_record<-NA


#In this loop, we check for each name whether it exists (or a close match with a small spelling mistake) and if the individual with the name could potentially be the parent as they were between 18-68 years old when their child was born. For all individuals with multiple potential matches, it creates a new vector that need to be manually checked.

 for (i in 3:length(writtenmomidstoreplace)) 
               { currentwrongid<-writtenmomidstoreplace[i]
                 nearmatch<-agrep(currentwrongid,temporaryfile$Name,max.distance=2)
                 nearmatch_record<-c(nearmatch_record,length(nearmatch))
                 
                 if(length(nearmatch)==0){ additionalindividual$Name<-currentwrongid
                                           additionalindividual$Sex<-0
                                           additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                           additionalindividual$ego<-runningid 
                                           e<-bind_rows(e,additionalindividual)
                                           e[e$MID %in% currentwrongid,]$MID<-runningid
                                           runningid<-runningid+1
                                           counter_mom_absent<-counter_mom_absent + 1
                                     }
                 
                 if(length(nearmatch)==1){ to_be_compared<-temporaryfile[nearmatch,]
                                           to_be_compared
                                           to_be_compared<-to_be_compared[to_be_compared$Sex==0,]
                                           to_be_compared[is.na(to_be_compared$newyob==T),]$newyob<-to_be_compared[is.na(to_be_compared$newyob==T),]$YOB
                                           to_be_compared<-to_be_compared[is.na(to_be_compared$newyob)==F,]
                                           minimumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 18
                                           maximumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 68
                                           if(is.na(temporaryfile[nearmatch,]$newyob)==F){ to_be_compared<-to_be_compared[to_be_compared$newyob < minimumage,  ] }
                                         if(is.na(temporaryfile[nearmatch,]$newyob)==F){ to_be_compared<-to_be_compared[to_be_compared$newyob > maximumage,  ] }
                                         if(nrow(to_be_compared)==0){
                                             additionalindividual$Name<-currentwrongid
                                             additionalindividual$Sex<-0
                                             additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                             additionalindividual$ego<-runningid 
                                             e<-bind_rows(e,additionalindividual)
                                             e[e$MID %in% currentwrongid,]$MID<-runningid
                                             runningid<-runningid+1
                                           counter_mom_absent<-counter_mom_absent + 1
                                           }
                                           if(nrow(to_be_compared)>0){
                                             e[e$MID %in% currentwrongid,]$MID<-temporaryfile[nearmatch,]$ego
                                             counter_mom_resolved<-counter_mom_resolved + 1
                                           }
                                           
                                     }
                 
                 if(length(nearmatch)>1 & length(nearmatch)<9){ 
                                          to_be_compared<-temporaryfile[nearmatch,]
                                          to_be_compared<-to_be_compared[to_be_compared$Sex==0,]
                                          to_be_compared[is.na(to_be_compared$newyob==T),]$newyob<-to_be_compared[is.na(to_be_compared$newyob==T),]$YOB
                                          to_be_compared<-to_be_compared[is.na(to_be_compared$newyob)==F,]
                                          minimumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 18
                                          maximumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 68
                                          to_be_compared<-to_be_compared[to_be_compared$newyob < minimumage,  ]
                                          to_be_compared<-to_be_compared[to_be_compared$newyob > maximumage,  ]
                                          if(nrow(to_be_compared)==0){e[e$MID %in% currentwrongid,]$MID<-NA
                                                                      counter_mom_na<-counter_mom_na + 1
                                                                     }
                                          if(nrow(to_be_compared)>0){
                                                                     formanualcheck<-bind_rows(formanualcheck,e[e$MID %in% currentwrongid,]%>% group_by(MID) %>% summarise_all(funs(first)))           
                                                                     formanualcheck<-bind_rows(formanualcheck,to_be_compared)
                                                                     counter_mom_multiple <- counter_mom_multiple + 1
                                                                     }
                                          
                                     }
                 
                 if(length(nearmatch)>8){ e[e$MID %in% currentwrongid,]$MID<-NA
                                          counter_mom_na<-counter_mom_na + 1
                                     }
                 
                 
                 }


for (i in 3:length(writtendadidstoreplace)) 
              { currentwrongid<-writtendadidstoreplace[i]
                nearmatch<-agrep(currentwrongid,temporaryfile$Name,max.distance=2)
                nearmatch_record<-c(nearmatch_record,length(nearmatch))

                if(length(nearmatch)==0){ additionalindividual$Name<-currentwrongid
                                          additionalindividual$Sex<-1
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-runningid 
                                          e<-bind_rows(e,additionalindividual)
                                          e[e$FID %in% currentwrongid,]$FID<-runningid
                                          runningid<-runningid+1
                                          counter_dad_absent<-counter_dad_absent + 1
                                        }

              if(length(nearmatch)==1){ to_be_compared<-temporaryfile[nearmatch,]
                                        to_be_compared
                                        to_be_compared<-to_be_compared[to_be_compared$Sex==1,]
                                        to_be_compared[is.na(to_be_compared$newyob==T),]$newyob<-to_be_compared[is.na(to_be_compared$newyob==T),]$YOB
                                        to_be_compared<-to_be_compared[is.na(to_be_compared$newyob)==F,]
                                        minimumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 18
                                        maximumage<-mean(e[e$MID %in% currentwrongid,]$newyob) - 68
                                        if(is.na(temporaryfile[nearmatch,]$newyob)==F){ to_be_compared<-to_be_compared[to_be_compared$newyob < minimumage,  ] }
                                        if(is.na(temporaryfile[nearmatch,]$newyob)==F){ to_be_compared<-to_be_compared[to_be_compared$newyob > maximumage,  ] }
                                        if(nrow(to_be_compared)==0){
                                          additionalindividual$Name<-currentwrongid
                                          additionalindividual$Sex<-1
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-runningid 
                                          e<-bind_rows(e,additionalindividual)
                                          e[e$FID %in% currentwrongid,]$FID<-runningid
                                          runningid<-runningid+1
                                          counter_dad_absent<-counter_dad_absent + 1
                                          }
                                      if(nrow(to_be_compared)>0){
                                          e[e$FID %in% currentwrongid,]$FID<-temporaryfile[nearmatch,]$ego
                                          counter_dad_resolved<-counter_dad_resolved + 1
                                          }
              
              }
                                      

              if(length(nearmatch)>1 & length(nearmatch)<9 ){ 
                                       to_be_compared<-temporaryfile[nearmatch,]
                                       to_be_compared<-to_be_compared[to_be_compared$Sex==1,]
                                       to_be_compared[is.na(to_be_compared$newyob==T),]$newyob<-to_be_compared[is.na(to_be_compared$newyob==T),]$YOB
                                       to_be_compared<-to_be_compared[is.na(to_be_compared$newyob)==F,]
                                       minimumage<-mean(e[e$FID %in% currentwrongid,]$newyob) - 18
                                       maximumage<-mean(e[e$FID %in% currentwrongid,]$newyob) - 68
                                       to_be_compared<-to_be_compared[to_be_compared$newyob < minimumage,  ]
                                       to_be_compared<-to_be_compared[to_be_compared$newyob > maximumage,  ]
                                       if(nrow(to_be_compared)==0){e[e$FID %in% currentwrongid,]$FID<-NA
                                                                   counter_dad_na<-counter_dad_na + 1
                                                                   }
                                       if(nrow(to_be_compared)>0){formanualcheck<-bind_rows(formanualcheck,e[e$FID %in% currentwrongid,]%>% group_by(FID) %>% summarise_all(funs(first)))
                                                                  formanualcheck<-bind_rows(formanualcheck,to_be_compared)
                                                                  counter_dad_multiple <- counter_dad_multiple + 1
                                                                  }
                                       
                                        }
                
              if(length(nearmatch)>8){ e[e$FID %in% currentwrongid,]$FID<-NA
                                        counter_dad_na<-counter_dad_na + 1
                                        }
                
              }

#-->VISUAL CHECK OF OUTPUT

#There are (counter_mom_absent) a number of written MIDs that do not match any individual, (counter_mom_resolved) several that match a single individual, (counter_mom_multiple) some more written IDs that have to be manually resolved, and a subset that cannot be resolved
#There are (counter_dad_absent) a number of written FIDs that do not match any individual, (counter_dad_resolved) several that match a single individual, (counter_dad_multiple) some written IDs that have to be manually resolved, and a subset that cannot be resolved

#The breakpoint (length(nearmatch)>8) was determined by leaving that final step (and the length(nearmatch)<9 condition) out of the code initially. In this case the histogram for nearmatch_record suggests a break at values larger than 8, so we include an additional category - whenever there are 9 or more matches to a given name the assumption is that it cannot be resolved and the individual is assumed to be unknown


#-->VISUAL CHECK OF OUTPUT
#because of the length of it we are writing it into a file to be checked in Excel

write.csv(formanualcheck,file="Quinlan_writtenParentalIDs_manualcheck.csv")


#The check revealed that for many written MIDs/FIDs a single existing individual is likely to be the match, but for some this cannot be resolved.

e[e$MID %in% "old name",]$MID<-NA
e[e$MID %in% "old name",]$MID<-'new ego ID'e$newyob<-as.numeric(e$newyob)
e[is.na(e$newyob)==F,]$newyob<-round(e[is.na(e$newyob)==F,]$newyob,digits=0)

write.csv(e,file="Cleaned_Dataset.csv")






#Additional checks for the parental assignments

#First check whether a woman has been assigned as the father or a man as the mother

f<-e
f$FID<-as.numeric(f$FID)
f$MID<-as.numeric(f$MID)
fids <- as.data.frame(sort(unique(f$FID[!is.na(f$FID)])))
colnames(fids)<-"ego"
mids <- as.data.frame(sort(unique(f$MID[!is.na(as.numeric(f$MID))])))
colnames(mids)<-"ego"


wrongfids<-inner_join(f,fids,by="ego")
parentswrongfids<-filter(wrongfids,Sex==0)
names(parentswrongfids)[names(parentswrongfids) == 'FID'] <- 'oldFID'
names(parentswrongfids)[names(parentswrongfids) == 'ego'] <- 'FID'
parentswrongfids$ego<-as.numeric(parentswrongfids$FID)
f$FID<-as.numeric(f$FID)
childrenwrongfids<-inner_join(f, parentswrongfids,by="FID")



wrongmids<-inner_join(f,mids,by="ego")
parentswrongmids<-filter(wrongmids,Sex==1)
names(parentswrongmids)[names(parentswrongmids) == 'MID'] <- 'oldMID'
names(parentswrongmids)[names(parentswrongmids) == 'ego'] <- 'MID'
f$MID<-as.numeric(f$MID)
childrenwrongmids<-inner_join(f, parentswrongmids,by="MID")


#-->VISUAL CHECK OF OUTPUT

#for some of these an individual of the wrong sex is assigned to the father
#for others the assignment is wrong
e[e$ego %in% 'father id',]$Sex <- 1
e[e$MID %in% "wrong id",]$MID<-'new ego id'
e[e$MID %in% "wrong id",]$MID<-NA




#Next we check whether the birthdate is within the possible range

parentalage<-select(e,ego,newyob,YOB)
parentalage[is.na(parentalage$newyob)==T,]$newyob<-parentalage[is.na(parentalage$newyob)==T,]$YOB

childage<-select(e,ego,MID,FID,newyob,YOB)
childage[is.na(childage$newyob)==T,]$newyob<-childage[is.na(childage$newyob)==T,]$YOB

maternalage<-select(childage,ego,MID,newyob)
colnames(maternalage)<-c("oldego","ego","newyob")
maternalage$ego<-as.numeric(maternalage$ego)
mothercheck<-inner_join(childage,maternalage,by="ego")
mothercheck<-mutate(mothercheck,birthyear = newyob.y - newyob.x)
mothercheck<-mothercheck[is.na(mothercheck$birthyear)==F,]
tooyoungmothers<-mothercheck[mothercheck$birthyear<16,]
toooldmothers<-mothercheck[mothercheck$birthyear>65,]

#There are some mothers who appear to old and some who appear to young. For the too old mothers, there are assignments that seem wrong, instances where the birthyear of ego is wrong, and instances where the birthyear of the mother appears wrong 

#-->VISUAL CHECK OF OUTPUT
#because of the length of it we are writing it into a file to be checked in Excel

write.csv(tooyoungmothers,file="MaternalId_manualcheck.csv")

e[e$MID %in% 'wrong id',]$MID<-NA
e[e$ego %in% 'ego id',]$newyob<-NA
e[e$ego %in% 'maternal id',]$newyob<-NA


paternalage<-select(childage,ego,FID,newyob)
colnames(paternalage)<-c("oldego","ego","newyob")
paternalage$ego<-as.numeric(paternalage$ego)
fathercheck<-inner_join(childage,paternalage,by="ego")
fathercheck<-mutate(fathercheck,birthyear = newyob.y - newyob.x)
fathercheck<-fathercheck[is.na(fathercheck$birthyear)==F,]
tooyoungfathers<-fathercheck[fathercheck$birthyear<16,]
toooldfathers<-fathercheck[fathercheck$birthyear>65,]

tooyoungfathers<-tooyoungfathers[is.na(tooyoungfathers$newyob.x)==F,]
tooyoungfathers<-tooyoungfathers[is.na(tooyoungfathers$newyob.y)==F,]
tooyoungfathers<-tooyoungfathers[tooyoungfathers$newyob.x!=1916,]
tooyoungfathers<-tooyoungfathers[tooyoungfathers$newyob.y!=1916,]

toooldfathers<-toooldfathers[is.na(toooldfathers$newyob.x)==F,]

#Check whether the entries appear possible, correct mistakes



#-->VISUAL CHECK OF OUTPUT
#because of the length of it we are writing it into a file to be checked in Excel

write.csv(tooyoungfathers,file="PaternalId_manualcheck.csv")




#Additional things to clean: individuals with YOD should have 'dEAD' correctly listed
#Any value that has become 0.5 in the averaging should be 1 in the em or dEAD columns

e[is.na(e$YOD)==F,]$dEAD<-1
e[e$dEAD %in% 0.5,]$dEAD<-1
e[e$em %in% 0.5,]$em<-1

missingdads<-filter(e,is.na(FID),!is.na(MID))$ego
missingmoms<-filter(e,!is.na(FID),is.na(MID))$ego

additionalindividual<-e[1,]
additionalindividual$YOB<-NA
additionalindividual$FID<-NA
additionalindividual$MID<-NA
additionalindividual$em<-NA
additionalindividual$HSE<-NA
additionalindividual$dEAD<-NA
additionalindividual$born_in_vil<-NA

formanualcheck<-additionalindividual

newid<-max(e$ego)+1

for (i in 1:length(missingdads)) {			e[e$ego==missingdads[i],]$FID<-newid
						                  additionalindividual$Name<-paste("Individual", newid,sep="")
                                          additionalindividual$Sex<-1
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-newid 
                                          e<-bind_rows(e,additionalindividual)
                                          newid <-newid +1
                                        
							}
newid<-max(e$ego)+1

for (i in 1:length(missingmoms)) {			e[e$ego== missingmoms[i],]$MID<-newid
						                  additionalindividual$Name<-paste("Individual", newid,sep="")
                                          additionalindividual$Sex<-0
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-newid 
                                          e<-bind_rows(e,additionalindividual)
                                          newid <-newid +1
                                        
							}


dead<-filter(e,dEAD!=0 | em!=0)$ego
e$new_householdID<-e$HSE
e[e$ego %in% alive,]$new_householdID
e[e$dEAD %in% 3,]$new_householdID<-NA
e[e$dEAD %in% 1,]$new_householdID<-NA
e[e$dEAD %in% "NaN",]$new_householdID<-NA
e[is.na(e$dEAD)==T,]$new_householdID<-NA
e[e$em %in% 1,]$new_householdID<-NA
e[e$em %in% "NaN",]$new_householdID<-NA
e[is.na(e$em)==T,]$new_householdID<-NA

e$status<-e$new_householdID
e[is.na(e$status)==T,]$status <-0
e[e$status!=0,]$status <-1




missingdads<-filter(e,is.na(FID),!is.na(MID))$ego
missingmoms<-filter(e,!is.na(FID),is.na(MID))$ego

additionalindividual<-e[1,]
additionalindividual$YOB<-NA
additionalindividual$FID<-NA
additionalindividual$MID<-NA
additionalindividual$em<-NA
additionalindividual$HSE<-NA
additionalindividual$dEAD<-NA
additionalindividual$born_in_vil<-NA

formanualcheck<-additionalindividual

newid<-max(e$ego)+1

for (i in 1:length(missingdads)) {			e[e$ego==missingdads[i],]$FID<-newid
						                  additionalindividual$Name<-paste("Individual", newid,sep="")
                                          additionalindividual$Sex<-1
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-newid 
                                          e<-bind_rows(e,additionalindividual)
                                          newid <-newid +1
                                        
							}
newid<-max(e$ego)+1

for (i in 1:length(missingmoms)) {			e[e$ego== missingmoms[i],]$MID<-newid
						                  additionalindividual$Name<-paste("Individual", newid,sep="")
                                          additionalindividual$Sex<-0
                                          additionalindividual$runningnumber<-paste("A",runningid,sep="")
                                          additionalindividual$ego<-newid 
                                          e<-bind_rows(e,additionalindividual)
                                          newid <-newid +1
                                        
							}







