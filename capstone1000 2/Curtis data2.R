# Data on Endorsement, Skills
# Skills

Skils_and_EndorsementCapstone2$Title<- vapply(lapply(strsplit(Skils_and_EndorsementCapstone2$Title, " "), unique), 
                                              paste, character(1L), collapse = " ")
Skils_and_EndorsementCapstone2$Title_1 <- vapply(lapply(strsplit(Skils_and_EndorsementCapstone2$Title_1, " "), unique), 
                                                 paste, character(1L), collapse = " ")
Skils_and_EndorsementCapstone2$Title_1 <- strtrim(Skils_and_EndorsementCapstone2$Title_1, 23)
strsplit(Skils_and_EndorsementCapstone2$Title_1, " ")[[1]][67]
word(Skils_and_EndorsementCapstone2$Title_1,c(1:10))
substr(Skils_and_EndorsementCapstone2$Title_1, start = 1, stop = 23)
Skils_and_EndorsementCapstone2$Title_1 <- trimws(Skils_and_EndorsementCapstone2$Title_1, which = c("both", "left", "right"))
Skils_and_EndorsementCapstone2$Title_2<- vapply(lapply(strsplit(Skils_and_EndorsementCapstone2$Title_2, " "), unique), 
                                              paste, character(1L), collapse = " ")
Skils_and_EndorsementCapstone2$Title_3 <- vapply(lapply(strsplit(Skils_and_EndorsementCapstone2$Title_3, " "), unique), 
                                                 paste, character(1L), collapse = " ")
Skils_and_EndorsementCapstone2$Title_4 <- unique(vapply(lapply(strsplit(Skils_and_EndorsementCapstone2$Title_4, " "), unique), 
                                                 paste, character(1L), collapse = " "))
gsub("[[:punct:]]", "",Skils_and_EndorsementCapstone2$Title_4 )

Skill6 <- unique(Skils_and_EndorsementCapstone_1_$Skill6)
Skill5 <- unique(Skils_and_EndorsementCapstone_1_$Skill5)
Skill4 <- unique(Skils_and_EndorsementCapstone_1_$Skill4)
Skill3 <- unique(Skils_and_EndorsementCapstone_1_$Skill3)
Skill2<- unique(Skils_and_EndorsementCapstone_1_$Skill2)
Skill1 <- unique(Skils_and_EndorsementCapstone_1_$Skill1)
Skils_and_EndorsementCapstone_1_[complete.cases(Skils_and_EndorsementCapstone_1_),]
is.na(Skils_and_EndorsementCapstone_1_)

UniqueSkill<- unique(Skils_and_EndorsementCapstone1)
colnames(UniqueSkill) <- c("Skill6","Skill5","Skill4","Skill3","Skill2","Skill1")
skill_demo <- data.frame()


Skillfun <- function(Title_Level,ID,Skill){
  if(Title_Level >5  & ID){
    l <- sample(Skill6, size = 1, replace = TRUE)
  }else if(Title_Level > 4){
    l <- sample(Skill5, size = 1, replace = TRUE)
  }else if(Title_Level >3){
    l <- sample(Skill4, size = 1, replace = TRUE)
  }else if(Title_Level > 2){
    l <- sample(Skill3, size = 1, replace = TRUE)
  }else if(Title_Level > 1){
   l <-  sample(Skill2, size = 1, replace = TRUE)
  }else{
    l <-  sample(Skill1, size = 1, replace = TRUE)
  }
  return(cbind(ID,l));
}

for (i in 1: 973){
skill_demo <- rbind(skill_demo,Skillfun(DemographicED$ID[i],
                                        c(Skill6,Skill5,Skill4,Skill3,Skill2, Skill1)[i]));
}                                               



Skillfun <- function(ID,Title_Level,Skill){
  if(ID & Title_Level >5){
    l <- sample(Skill6, size = 10, replace = TRUE)
  }else if(ID & Title_Level >4){
    l <- sample(Skill5, size = 10, replace = TRUE)
  }else if(ID & Title_Level >3){
    l <- sample(Skill4, size = c(5:7), replace = TRUE)
  }else if (ID & Title_Level >2){
    l <- sample(Skill3, size = c(5:7), replace = TRUE)
  }else if (ID & Title_Level >1){
    l <- sample(Skill2, size = c(5:7), replace = TRUE)
  }else{
    l <- sample(Skill1, size = c(5:7), replace = TRUE)
  }  
  
  return(cbind(ID,Title_Level,l));
}

for(i in 1:973){
  skill_demo <- rbind(skill_demo,Skillfun(Demographic$Title_Level[i], Demographic$ID[i]));
}

colnames(skill0) <- c("Title_level", "ID","Skill")
Skill<- Skill[!duplicated(Skill),]

Curtis_Skill<- join(skill_demo,Skill, by = "Skill_Name")
Curtis_Skill1<- Curtis_Skill[!duplicated(Curtis_Skill),]
Curtis_Skill1$SkillEndorsed <- sample(c(0,1), size= 12069, replace= TRUE)
Curtis_Skill1$No_Times_Endorsed <-ifelse((Curtis_Skill1$SkillEndorsed==1), sample(c(20:60),size = 12069,replace = TRUE),0)


write.csv(FinalSkill, file = "CurtisSkill.csv")
colnames(Skill) <- c("Skill_ID","Skill_Name","Skill_Type")
