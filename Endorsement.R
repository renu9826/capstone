# Endorsement #1
#Assumption 1) Anyone can endorsed anyone. 
#2) people with profile in Demographic file will have at least one endorsement
#3)People who give endorsements also have endorsements. And whenever we will data from linkedin we will have have info.This will be utilized in creating High Credibility Score.
#4)Relationship- 1= Friend-who studied with endoce 2=colleague- who worked with endorce at the same title level 3= direct supervisor/mentor
rm(Endorcer1)
Endorcer1 <- data.frame (Title_level_Endorcer1 = integer(973))
                         
Endorcer1$Endorcer1_ID <- sample(c(1:973), size =973, replace = FALSE) 
Endorcer1$Title_level_Endorcer1 <- sample(c(1,2,3,4,5,6), size = 973, replace = TRUE)
Endorcer1$YOEEndorcer1 <- sample(c(1:43), size = 973, replace = TRUE)
Endorcer1$No_Endorsement_received <- NULL
Endorcer1$Relation_to_Endorce <- sample(c(1,2,3), size = 973, replace = TRUE)
Endorcer1$EndorceID <- sample(c(CurtisData$ID), size =973, replace = FALSE)
EndorSkill1 <- data.frame()
table(Endorcer1$EndorceID)
table(Endorcer1$Endorcer1_ID)
write.csv(Endorcer1, file = "Endorcer1.csv")
write.csv(Endorcer2, file = "Endorcer2.csv")
write.csv(Endorcer3, file = "Endorcer3.csv")

 colnames(Endorcer1)
for(i in 1:973){
  EndorSkill1 <- rbind(EndorSkill1,Skillfun(Endorcer1$Endorcer1_ID[i],Endorcer1$Title_level_Endorcer1[i]));
}
colnames(EndorSkill1) <- c("Endorcer1_ID","Title_LevelEndorcer1","Skill")
EndorSkill1<- join(EndorSkill1,Skill, "Skill")
#Removing Duplicates
EndorSkill11<- EndorSkill1[!duplicated(EndorSkill1),]
EndorSkill11$Skill_Type <- ifelse(EndorSkill11$Type==1, "Soft", "Technical")
EndorSkill11 <- EndorSkill11[,-c(3,5:6)]
colnames(EndorSkill11)
EndorSkill11$ENDOSKI1_ID <- 1:nrow(EndorSkill11)

#Assigning Endorsements
EndorSkill11$SkillEndorsed1 <- NULL
#NO of Endorsements per skill that is endorsed
EndorSkill11$No_of_Endorsed1 <- sample(c(0:40),size = 4689,replace = TRUE)
EndorSkill11<- EndorSkill11[complete.cases(EndorSkill11),]
Endorcer1$Total_Endo1 <- tapply(EndorSkill11$No_of_Endorsed1, EndorSkill11$Endorcer1_ID, median)
Endorcer1$CS1 <- (Endorcer1$Title_level_Endorcer1+Endorcer1$YOEEndorcer1+Endorcer1$Total_Endo1)

write.csv(EndorSkill11, file = "endorskill11.csv")

# Endorcer#2
Endorcer2 <- data.frame (Title_level_Endorcer2 = integer(973),
                           Endorcer1ID = integer(973))
Endorcer2$Endorcer2_ID <- sample(c(1:973), size =973, replace = FALSE) 
Endorcer2$Title_level_Endorcer2 <- sample(c(1,2,3,4,5,6), size = 973, replace = TRUE)
Endorcer2$YOEEndorcer2 <- sample(c(1:43), size = 973, replace = TRUE)
Endorcer2$No_Endorsement_received <- NULL
Endorcer2$Relation_to_Endorcer1 <- sample(c(1,2,3), size = 973, replace = TRUE)
Endorcer2$ Endorcer1_ID <- sample(c(Endorcer1$Endorcer1_ID), size =973, replace = FALSE)
EndorSkill2 <- data.frame()

for(i in 1:973){
 EndorSkill2 <- rbind(EndorSkill2,Skillfun(Endorcer2$Endorcer2_ID[i],Endorcer2$Title_level_Endorcer2[i]));
}
colnames(EndorSkill2) <- c("Endorcer2_ID","Title_LevelEndorcer2","Skill")
EndorSkill2<- join(EndorSkill2,Skill, "Skill")
#Removing Duplicates
EndorSkill21<- EndorSkill2[!duplicated(EndorSkill2),]
EndorSkill21<- EndorSkill21[complete.cases(EndorSkill21),]
EndorSkill21$Skill_Type <- ifelse(EndorSkill21$Type==1, "Soft", "Technical")
EndorSkill21 <- EndorSkill21[,-c(3,5)]
colnames(EndorSkill21)
EndorSkill21$ENDOSKI2_ID <- 1:nrow(EndorSkill21)
#Assigning Endorsements
EndorSkill21$SkillEndorsed2 <- NULL
#NO of Endorsements per skill that is endorsed
EndorSkill21$No_of_Endorsed2 <- sample(c(0:40),size =4736,replace = TRUE)
Endorcer2$Total_Endo <- tapply(EndorSkill21$No_of_Endorsed2, EndorSkill21$Endorcer2_ID, median)
Endorcer2$CS2 <- (Endorcer2$Title_level_Endorcer2+Endorcer2$YOEEndorcer2+Endorcer2$Total_Endo)
write.csv(EndorSkill21, file = "Endorskill21.csv")


# Endorcer# 3
Endorcer3 <- data.frame (Title_level_Endorcer3 = integer(973),
                         Endorcer2ID = integer(973))
Endorcer3$Endorcer3_ID <- sample(c(1:973), size =973, replace = FALSE) 
Endorcer3$Title_level_Endorcer3 <- sample(c(1,2,3,4,5,6), size = 973, replace = TRUE)
Endorcer3$YOEEndorcer3 <- sample(c(1:43), size = 973, replace = TRUE)
Endorcer3$No_of_Endorsed <- NULL
Endorcer3$Relation_to_Endorcer2 <- sample(c(1,2,3), size = 973, replace = TRUE)
Endorcer3$ Endorcer2_ID <- sample(c(Endorcer2$Endorcer2_ID), size =973, replace = FALSE)
EndorSkill3 <- data.frame()

for(i in 1:973){
  EndorSkill3 <- rbind(EndorSkill3,Skillfun(Endorcer3$Endorcer3_ID[i],Endorcer3$Title_level_Endorcer3[i]));
}
colnames(EndorSkill3) <- c("Endorcer3_ID","Title_LevelEndorcer3","Skill")
EndorSkill3<- join(EndorSkill3,Skill, "Skill")
#Removing Duplicates
EndorSkill31<- EndorSkill3[!duplicated(EndorSkill3),]
colnames(EndorSkill31)
EndorSkill31<- EndorSkill31[complete.cases(EndorSkill31),]
EndorSkill31$ENDOSKI3_ID <- 1:nrow(EndorSkill31)
#Assigning Endorsements
EndorSkill31$SkillEndorsed3 <- NULL
#NO of Endorsements per skill that is endorsed
EndorSkill31$No_of_Endorsed3 <- sample(c(0:40),size = 4648,replace = TRUE)
Endorcer3$Total_Endo3 <- tapply(EndorSkill31$No_of_Endorsed3, EndorSkill31$Endorcer3_ID, median)
Endorcer3$CS3 <- (Endorcer3$Title_level_Endorcer3+Endorcer3$YOEEndorcer3+Endorcer3$Total_Endo3)
write.csv(EndorSkill31, file = "endorskill31.csv")

table(EndorSkill11$No_of_Endorsed1)
