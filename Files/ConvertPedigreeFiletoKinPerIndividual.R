
#---------------------------------------------------------------------------------------------------------------------------
#This code can be used to determine for all pairs of individuals in a dataset whether they belong to any of a subset of close kin relationships (parent-offspring, siblings, etc.). It takes a pedigree file as an input.

#For questions please contact dieter_lukas@eva.mpg.de


library(tidyr)
library(dplyr)


#The input file should be a table in text format. The important columns are 
# - an identifier for the individual (here called ID); this should be unique so that no two individuals have the same ID and each individual has exactly one ID
# - an identifier listing the ID of the mother of an individual (here called MID)
# - an identifier listing the ID of the mother of an individual (here called FID)
# - a column listing whether the individual is dead or not (all individuals will be used to reconstruct the pedigree, but this can be subset in case kinship is only interesting to know among individuals still alive) (here called DEAD with values of 0 indicating that the individual is still alive)


  test<- data.frame(ID=c("AFR","AKA","MAG","CAN","UMW", "PUC", "EFF"), MID= c("MAG","UMW","EFF","PUC",NA,"EFF",NA), FID=c("CAN","CAN",NA,NA,NA,NA,NA), sex=c("F","F","F","M","F","F","F"), DEAD= rep(0, times=7),stringsAsFactors=F)

input_IUR_file<-test


#upload file:
input_IUR_file<-read.table(file="YOURINPUTFILE.txt",header=TRUE)

#Here are the two options - either work with the whole dataset
pairwise_comparison<-expand.grid(x= input_IUR_file$ID,y=input_IUR_file$ID)

#Or only with individuals who are still alive
pairwise_comparison<-expand.grid(x= input_IUR_file[input_IUR_file$DEAD==0,]$ID,y=input_IUR_file[input_IUR_file$DEAD ==0,]$ID)

dyadic_kinship<-matrix(nrow=nrow(pairwise_comparison),ncol=18)
dyadic_kinship <-as.data.frame(dyadic_kinship)
colnames(dyadic_kinship)<-c("ID","PartnerID","ParentOffspring","OffspringParent","FullSilbing","MaternalHalfSib","PaternalHalfSib","PaternalGrandparentGrandoffspring","PaternalGrandoffspringGrandparent","MaternalGrandparentGrandoffspring","MaternalGrandoffspringGrandparent","SistersChild","MothersSibling","BrothersChild","FathersSibling","MothersSistersChild","FathersBrothersChild","ClosestDegree")

#go through all the different categories. They are listed as Target-Partner (in symmetrical instances, eg. siblings, there is only one entry) and are combined independent of the sex of the target or the partner (that information is listed separately, through the commands right below): are they parent-offspring, offspring-parent, fullsib, maternal sib, paternal sib, paternal grandparent-grandoffspring (the mother or father of the father of a child), paternal grandoffspring-grandparent (the child of a son of a woman/man), maternal grandparent-grandoffspring (the mother or father of the mother of a child), maternal grandoffspring-grandparent (the child of a daugher of a woman/man), sister's-child (aunt or uncle to niece of nephew through the mother of the child), brother's-child (aunt or uncle to niece or nephew through the father of the child), father's-sibling (niece or nephew to aunt or uncle through the father), mother's-sibling (niece or nephew to aunt or uncle through the mother), mother's sister's-child (maternal cousins), father's brother's-child(paternal cousins); the final column 'closest degree' will list the highest relatedness category between the pair, with highness sorted from left to right, asssuming that the relationship listed first (parentoffspring) is highest and the relationship listed last (FathersBrothersChild) is lowest.


dyadic_kinship $ID<-pairwise_comparison $x
dyadic_kinship $PartnerID <-pairwise_comparison $y

dyadic_kinship <-left_join(dyadic_kinship, input_IUR_file[,c("ID","sex")],by="ID")


colnames(dyadic_kinship)<-c("TargetID","ID","ParentOffspring","OffspringParent","FullSilbing","MaternalHalfSib","PaternalHalfSib","PaternalGrandparentGrandoffspring","PaternalGrandoffspring-Grandparent","MaternalGrandparentGrandoffspring","MaternalGrandoffspringGrandparent","Sisters-Child","Mothers-Sibling","BrothersChild","FathersSibling","MothersSistersChild","FathersBrothersChild","ClosestDegree","TargetSex")

dyadic_kinship <-left_join(dyadic_kinship, input_IUR_file[,c("ID","sex")],by="ID")

colnames(dyadic_kinship)<-c("TargetID","PartnerID","ParentOffspring","OffspringParent","FullSilbing","MaternalHalfSib","PaternalHalfSib","PaternalGrandparentGrandoffspring","PaternalGrandoffspringGrandparent","MaternalGrandparentGrandoffspring","MaternalGrandoffspringGrandparent","SistersChild","MothersSibling","BrothersChild","FathersSibling","MothersSistersChild","FathersBrothersChild","ClosestDegree","TargetSex","PartnerSex")

dyadic_kinship<-filter(dyadic_kinship, TargetID!= PartnerID) 



#We now have a file called "dyadic_kinship" which will be our output file, containing and receiving all data for further analyses.

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------



#Next we create an ancestor file, that for each individual lists that ids' of it's parents and grandparents - these ties capture all relationships that we are interested in

mothers<-data.frame(input_IUR_file$MID)
mothers<-data.frame(mothers[is.na(mothers$input_IUR_file.MID)==FALSE,])
colnames(mothers)<-"ID"
fathers<-data.frame(input_IUR_file$FID)
fathers <-data.frame(fathers[is.na(fathers $input_IUR_file.FID)==FALSE,])
colnames(fathers)<-"ID"
individuals<-data.frame(input_IUR_file$ID)
colnames(individuals)<-"ID"
missingfathers<-anti_join(fathers,individuals,by="ID")
missingmothers<-anti_join(mothers,individuals,by="ID")


ancestorfile<-matrix(nrow=nrow(input_IUR_file),ncol=7)
ancestorfile <-as.data.frame(ancestorfile)
colnames(ancestorfile)<-c("Id","Mother","Father","PaternalGrandfather","MaternalGrandfather","PaternalGrandmother","MaternalGrandmother")
ancestorfile$Id<-input_IUR_file $ID


ancestorfile$Father<-input_IUR_file[input_IUR_file $ID== ancestorfile$Id,]$FID
ancestorfile$Mother<-input_IUR_file[input_IUR_file $ID== ancestorfile$Id,]$MID



for (i in 1:nrow(ancestorfile) ) {
						
						currentId<-ancestorfile[i,1]
						
						ifelse(is.na(ancestorfile[ancestorfile$Id==currentId,]$Father) | ancestorfile[ancestorfile$Id==currentId,]$Father %in% missingfathers$ID,
									ancestorfile[ancestorfile$Id==currentId,]$PaternalGrandfather<-NA,
									ancestorfile[ancestorfile$Id==currentId,]$PaternalGrandfather<-input_IUR_file[input_IUR_file $ID== ancestorfile[ancestorfile$Id==currentId,]$Father,]$FID														)
									
									ifelse(is.na(ancestorfile[ancestorfile$Id==currentId,]$Father) | ancestorfile[ancestorfile$Id==currentId,]$Father %in% missingfathers$ID,
									ancestorfile[ancestorfile$Id==currentId,]$PaternalGrandmother <-NA,
									ancestorfile[ancestorfile$Id==currentId,]$PaternalGrandmother <-input_IUR_file[input_IUR_file $ID== ancestorfile[ancestorfile$Id==currentId,]$Father,]$MID														)
									
						ifelse(is.na(ancestorfile[ancestorfile$Id==currentId,]$Mother) | ancestorfile[ancestorfile$Id==currentId,]$Mother %in% missingmothers $ID,
									ancestorfile[ancestorfile$Id==currentId,]$MaternalGrandfather<-NA,
									ancestorfile[ancestorfile$Id==currentId,]$MaternalGrandfather<-input_IUR_file[input_IUR_file $ID== ancestorfile[ancestorfile$Id==currentId,]$Mother,]$FID														)
									
									ifelse(is.na(ancestorfile[ancestorfile$Id==currentId,]$Mother) | ancestorfile[ancestorfile$Id==currentId,]$Mother %in% missingmothers $ID,
									ancestorfile[ancestorfile$Id==currentId,]$MaternalGrandmother <-NA,
									ancestorfile[ancestorfile$Id==currentId,]$MaternalGrandmother <-input_IUR_file[input_IUR_file $ID== ancestorfile[ancestorfile$Id==currentId,]$Mother,]$MID														)
									
												
						
										} 



#------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#This loop to determine all pairwise relationships considers all relationships, such that dyads can be in multiple relationships (e.g. if their parents are related). It therefore also does not automatically calculate the closest degree, since this can be a composite. This loop is a bit slower to run (takes about 4 sec per dyad)


for (i in 1:nrow(dyadic_kinship)) {
		
	#ParentOffspring
				currentparent<-dyadic_kinship[i,"TargetID"]
				currentoffspring <-dyadic_kinship[i,"PartnerID"]
				parent<-ancestorfile[ancestorfile$Mother %in% currentparent | ancestorfile$Father %in% currentparent,]
				dyadic_kinship[i,] $ParentOffspring<-ifelse(currentoffspring %in% parent$Id, 1, 0)

				
	#OffspringParent

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentparent <-dyadic_kinship[i,"PartnerID"]
				offspring<-ancestorfile[ancestorfile$Id %in% currentoffspring,]
				dyadic_kinship[i,] $OffspringParent<-ifelse(currentparent %in% offspring $Mother | currentparent %in% offspring $Father,  1, 0)


	#FullSilbing

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentsibling<-dyadic_kinship[i,"PartnerID"]
				offspringsfather<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Father
				offspringsmother<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Mother
				family<-ancestorfile[ancestorfile$Father %in% offspringsfather,]
				family<-family[family$Mother %in% offspringsmother,]
				family<-family[is.na(family$Father)==F,]
				family<-family[is.na(family$Mother)==F,]
				dyadic_kinship[i,] $FullSilbing <-ifelse(currentsibling %in% family $Id,  1, 0)

	#MaternalHalfSib

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentsibling<-dyadic_kinship[i,"PartnerID"]
				offspringsmother<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Mother
				family<-ancestorfile[ancestorfile$Mother %in% offspringsmother,]
				family<-family[is.na(family$Mother)==F,]
				dyadic_kinship[i,] $MaternalHalfSib <-ifelse(currentsibling %in% family $Id,  1, 0)
				dyadic_kinship[i,] $MaternalHalfSib <-ifelse(dyadic_kinship[i,] $FullSilbing %in% "1", 0, dyadic_kinship[i,] $MaternalHalfSib)

	#PaternalHalfSib

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentsibling<-dyadic_kinship[i,"PartnerID"]
				offspringsfather<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Father
				family<-ancestorfile[ancestorfile$Father %in% offspringsfather,]
				family<-family[is.na(family$Father)==F,]
				dyadic_kinship[i,] $PaternalHalfSib <-ifelse(currentsibling %in% family $Id,  1, 0)
				dyadic_kinship[i,] $PaternalHalfSib <-ifelse(dyadic_kinship[i,] $FullSilbing %in% "1", 0, dyadic_kinship[i,] $PaternalHalfSib)


	#PaternalGrandparentGrandoffspring

				currentgrandparent<-dyadic_kinship[i,"TargetID"]
				currentoffspring <-dyadic_kinship[i,"PartnerID"]
				grandparent<-ancestorfile[ancestorfile$PaternalGrandfather %in% currentgrandparent | ancestorfile$PaternalGrandmother %in% currentgrandparent,]
				grandparent<-grandparent[is.na(grandparent$PaternalGrandmother)==F | is.na(grandparent$PaternalGrandmother)==F,]
				dyadic_kinship[i,] $PaternalGrandparentGrandoffspring<-ifelse(currentoffspring %in% grandparent $Id,  1, 0)


	#PaternalGrandoffspringGrandparent

				currentgrandparent<-dyadic_kinship[i,"PartnerID"]
				currentoffspring <-dyadic_kinship[i,"TargetID"]
				offspring<-ancestorfile[ancestorfile$Id %in% currentoffspring,]
				dyadic_kinship[i,] $PaternalGrandoffspringGrandparent <-ifelse(currentgrandparent %in% offspring $PaternalGrandmother | currentgrandparent %in% offspring $PaternalGrandfather,  1, 0)


	#MaternalGrandparentGrandoffspring

				currentgrandparent<-dyadic_kinship[i,"TargetID"]
				currentoffspring <-dyadic_kinship[i,"PartnerID"]
				grandparent<-ancestorfile[ancestorfile$MaternalGrandfather %in% currentgrandparent,]
				grandparent<-grandparent[is.na(grandparent$MaternalGrandfather)==F,]
				dyadic_kinship[i,] $MaternalGrandparentGrandoffspring <-ifelse(currentoffspring %in% grandparent $Id,  1, 0)
				grandparent<-ancestorfile[ancestorfile$MaternalGrandmother %in% currentgrandparent,]
				grandparent<-grandparent[is.na(grandparent$MaternalGrandmother)==F,]
				if(dyadic_kinship[i,] $MaternalGrandparentGrandoffspring==0){dyadic_kinship[i,] $MaternalGrandparentGrandoffspring <-ifelse(currentoffspring %in% grandparent $Id,  1, 0)}

	#MaternalGrandoffspringGrandparent

				currentgrandparent<-dyadic_kinship[i,"PartnerID"]
				currentoffspring <-dyadic_kinship[i,"TargetID"]
				offspring<-ancestorfile[ancestorfile$Id %in% currentoffspring,]
				dyadic_kinship[i,] $MaternalGrandoffspringGrandparent <-ifelse(currentgrandparent %in% offspring $MaternalGrandmother | currentgrandparent %in% offspring $MaternalGrandfather,  1, 0)
			
				
	#SistersChild

				currentsister<-dyadic_kinship[i,"TargetID"]
				currentoffspring <-dyadic_kinship[i,"PartnerID"]
				currentsistermother<-ancestorfile[ancestorfile$Id %in% currentsister,]$Mother
				if(is.na(currentsistermother)){currentsistermother<-0}
				currensisterfather<-ancestorfile[ancestorfile$Id %in% currentsister,]$Father
				if(is.na(currensisterfather)){currensisterfather <-0}
				family<-ancestorfile[ancestorfile$Mother %in% currentsistermother | ancestorfile$Father %in% currensisterfather,]
				family<-family[is.na(family$Mother)==F | is.na(family$Father)==F,]
				siblingschildren<-ancestorfile[ancestorfile$Mother %in% family$Id,]
				siblingschildren <-siblingschildren[siblingschildren $Mother != currentsister,]
				dyadic_kinship[i,] $SistersChild <-ifelse(currentoffspring %in% siblingschildren$Id,  1, 0)



	#MothersSibling

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentsibling <-dyadic_kinship[i,"PartnerID"]
				currentmother<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Mother
				if(is.na(currentmother)){currentmother <-0}
				currentmaternalgrandmother <-ancestorfile[ancestorfile$Id %in% currentmother,]$Mother
				currentmaternalgrandfather <-ancestorfile[ancestorfile$Id %in% currentmother,]$Father
				family<-ancestorfile[ancestorfile$Mother %in% currentmaternalgrandmother ,]
				family<-family[is.na(family$Mother)==F,]
				family<-family[family$Id != currentmother,]
				dyadic_kinship[i,] $MothersSibling <-ifelse(currentsibling %in% family$Id,  1, 0)				
				family<-ancestorfile[ancestorfile$Father %in% currentmaternalgrandfather ,]
				family<-family[is.na(family$Father)==F,]	
				family<-family[family$Id != currentmother,]			 
				if(dyadic_kinship[i,] $MothersSibling ==0) {dyadic_kinship[i,] $MothersSibling <-ifelse(currentsibling %in% family$Id,  1, 0)}


	#BrothersChild

				currentbrother<-dyadic_kinship[i,"TargetID"]
				currentoffspring <-dyadic_kinship[i,"PartnerID"]
				currentbrothermother<-ancestorfile[ancestorfile$Id %in% currentbrother,]$Mother
				if(is.na(currentbrothermother)){currentbrothermother <-0}
				currentbrotherfather<-ancestorfile[ancestorfile$Id %in% currentbrother,]$Father
				if(is.na(currentbrotherfather)){currentbrotherfather <-0}
				family<-ancestorfile[ancestorfile$Mother %in% currentbrothermother | ancestorfile$Father %in% currentbrotherfather,]
				family<-family[is.na(family$Mother)==F | is.na(family$Father)==F,]
				siblingschildren<-ancestorfile[ancestorfile$Father %in% family$Id,]
				siblingschildren <-siblingschildren[siblingschildren $Father != currentbrother,]
				dyadic_kinship[i,] $BrothersChild <-ifelse(currentoffspring %in% siblingschildren$Id,  1, 0)



	#FathersSibling

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentsibling <-dyadic_kinship[i,"PartnerID"]
				currentfather<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Father
				if(is.na(currentfather)){currentfather<-0}
				currentpaternalgrandmother <-ancestorfile[ancestorfile$Id %in% currentfather,]$Mother
				currentpaternalgrandfather <-ancestorfile[ancestorfile$Id %in% currentfather,]$Father
				family<-ancestorfile[ancestorfile$Mother %in% currentpaternalgrandmother ,]
				family<-family[is.na(family$Mother)==F,]
				family<-family[family$Id != currentfather,]	
				dyadic_kinship[i,] $FathersSibling <-ifelse(currentsibling %in% family$Id,  1, 0)				
				family<-ancestorfile[ancestorfile$Father %in% currentpaternalgrandfather ,]
				family<-family[is.na(family$Father)==F,]	
				family<-family[family$Id != currentfather,]			 
				if(dyadic_kinship[i,] $FathersSibling==0) {dyadic_kinship[i,] $FathersSibling <-ifelse(currentsibling %in% family$Id,  1, 0)}


	#MothersSistersChild

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentcousin <-dyadic_kinship[i,"PartnerID"]
				currentmother<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Mother
				if(is.na(currentsistermother)){currentsistermother<-0}
				currentmaternalgrandmother <-ancestorfile[ancestorfile$Id %in% currentmother,]$Mother
				family<-ancestorfile[ancestorfile$Mother %in% currentmaternalgrandmother ,]
				family<-family[is.na(family$Mother)==F,]
				family<-family[family$Id!= currentmother]
				allmaternalcousins<-ancestorfile[ancestorfile$Mother %in% family$Id,]
				allmaternalcousins<-allmaternalcousins[allmaternalcousins$Id %in% currentcousin,]
				dyadic_kinship[i,] $MothersSistersChild <-ifelse(currentcousin %in% allmaternalcousins $Id,  1, 0)				
				
				currentmaternalgrandfather <-ancestorfile[ancestorfile$Id %in% currentmother,]$Father
				family<-ancestorfile[ancestorfile$Father %in% currentmaternalgrandfather,]
				family<-family[is.na(family$Father)==F,]
				family<-family[family$Id!= currentmother]
				allmaternalcousins<-ancestorfile[ancestorfile$Mother %in% family$Id,]
				allmaternalcousins<-allmaternalcousins[allmaternalcousins$Id %in% currentcousin,]
				if(dyadic_kinship[i,] $MothersSistersChild==0){dyadic_kinship[i,] $MothersSistersChild <-ifelse(currentcousin %in% allmaternalcousins $Id,  1, 0)}

		
	#FathersBrothersChild

				currentoffspring <-dyadic_kinship[i,"TargetID"]
				currentcousin <-dyadic_kinship[i,"PartnerID"]
				currentfather<-ancestorfile[ancestorfile$Id %in% currentoffspring,]$Father
				if(is.na(currentfather)){currentfather<-0}
				currentmaternalgrandmother <-ancestorfile[ancestorfile$Id %in% currentfather,]$Mother
				family<-ancestorfile[ancestorfile$Mother %in% currentmaternalgrandmother,]
				family<-family[is.na(family$Mother)==F,]	
				family<-family[family$Id!= currentfather]			
				allpaternalcousins<-ancestorfile[ancestorfile$Father %in% family$Id,]
				allpaternalcousins<-allpaternalcousins[allpaternalcousins$Id %in% currentcousin,]
				dyadic_kinship[i,] $FathersBrothersChild <-ifelse(currentcousin %in% allpaternalcousins $Id,  1, 0)				
				currentmaternalgrandfather <-ancestorfile[ancestorfile$Id %in% currentfather,]$Father
				family<-ancestorfile[ancestorfile$Father %in% currentmaternalgrandfather,]
				family<-family[is.na(family$Father)==F,]
				family<-family[family$Id!= currentfather]
				allpaternalcousins<-ancestorfile[ancestorfile$Father %in% family$Id,]
				allpaternalcousins<-allpaternalcousins[allpaternalcousins$Id %in% currentcousin,]
				if(dyadic_kinship[i,] $FathersBrothersChild==0){dyadic_kinship[i,] $FathersBrothersChild <-ifelse(currentcousin %in% allpaternalcousins $Id,  1, 0)}				
			
				
								}


#To connect this information to other data files with information on group composition etc.
currentgroup<-c("AKA","AFR","MAG")
filter(dyadic_kinship,TargetID %in% currentgroup, PartnerID %in% currentgroup)

