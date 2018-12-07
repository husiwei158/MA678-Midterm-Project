setwd("C:\\Users\\husiw\\OneDrive\\document\\MA678\\midterm project\\League of Legend")
LeagueofLegends <- read_csv("LeagueofLegends.csv")
gold <- read_csv("gold.csv")
kills <- read_csv("kills.csv")
monsters <- read_csv("monsters.csv")
structures <- read_csv("structures.csv")

### general information about LOL competitions
LOL <- LeagueofLegends %>% dplyr::select(League,Year,Season,Type,blueTeamTag,bResult,rResult,redTeamTag,gamelength,Address) %>% filter(!is.na(blueTeamTag))
LOL$id <- str_sub(LOL$Address,-16,-1)
LOL %<>% dplyr::select(-Address) 


### Gold part
gold$id <- str_sub(gold$Address,-16,-1)
maxgold <- matrix(NA,nrow = nrow(gold))
onlygold <- gold %>% dplyr::select(-c(Address,Type,id))

## try a time series gold vary for each game 
golddiff <- gold %>% filter(Type == "golddiff") %>% dplyr::select(-c(Type,Address))

for(i in 1:nrow(gold)){
  b <- as.vector(as.numeric(onlygold[i,]))
  b <- b[!is.na(b)]
  maxgold[i,] <- tail(b,n=1)
}

# id <- unique(gold$id)
# LOL <- cbind(id,LOL)
maxgold <- as.data.frame(maxgold)
maxgold <- cbind(gold$id,gold$Type,maxgold)
colnames(maxgold) <- c("id","Type","finalgold")

maxgold<- maxgold %>% spread(key= Type, value = finalgold) %>% mutate(ADCdiff = goldblueADC - goldredADC) %>% mutate(Middlediff = goldblueMiddle - goldredMiddle) %>% mutate(Topdiff = goldblueTop - goldredTop) %>% mutate(Junglediff = goldblueJungle - goldredJungle) %>% mutate(Supportdiff = goldblueSupport - goldredSupport)

maxgold$c.ADCdiff <- maxgold$ADCdiff/1000
maxgold$c.Middlediff <- maxgold$Middlediff/1000
maxgold$c.Topdiff<-maxgold$Topdiff/1000
maxgold$c.Junglediff<-maxgold$Junglediff/1000
maxgold$c.Supportdiff <- maxgold$Supportdiff/1000



# kill part
kills$id <- str_sub(kills$Address,-16,-1)

kill <- kills %>% group_by(id,Team) %>% summarise(kill = n(),assit = sum(!is.na(Assist_1),!is.na(Assist_2),!is.na(Assist_3),!is.na(Assist_4))) %>% left_join(LOL)

kill.blue<- kill %>% filter(str_detect(Team,"b")) %>% dplyr::select(-assit)%>% spread(key = Team, value = kill)
kill.red <- kill %>% filter(str_detect(Team,"r")) %>% dplyr::select(-assit)%>% spread(key = Team, value = kill)
assit.blue <- kill %>% filter(str_detect(Team,"b")) %>% dplyr::select(-kill)%>% spread(key = Team, value = assit)
assit.red <- kill %>% filter(str_detect(Team,"r")) %>% dplyr::select(-kill)%>% spread(key = Team, value = assit)

finalkill <- cbind(kill.blue$id,kill.blue$bKills,kill.red$rKills,assit.blue$bKills,assit.red$rKills)
finalkill <- as.data.frame(finalkill)
colnames(finalkill) <- c("id","bkills","rkills","bassits","rassits")
finalkill$bkills <- as.numeric(as.character(finalkill$bkills)) 
finalkill$bassits <- as.numeric(as.character(finalkill$bassits)) 
finalkill$rkills <- as.numeric(as.character(finalkill$rkills)) 
finalkill$rassits <- as.numeric(as.character(finalkill$rassits)) 

finalkill <- finalkill %>% mutate(killdiff = bkills-rkills) %>% mutate(assitsdiff = bassits -rassits)


##Monster part
library(data.table)
monsters$id <- str_sub(monsters$Address,-16,-1)
monsters$color <- str_sub(monsters$Team,1,1)
monster.final <-  monsters %>% mutate(n = 1) %>%  group_by(id,Team,Type) %>% mutate(number = sum(n)) %>% dplyr::select(-n) %>% arrange(id)
monster.final$monsterkiller <- paste0(monster.final$color,"_",monster.final$Type)
monster.final2 <- monster.final %>% dplyr::select(id,monsterkiller)%>% mutate(n = 1) %>% group_by(id,monsterkiller) %>% summarise(number = sum(n))

finalmonster <- monster.final2 %>% spread(key = monsterkiller,value = number)

finalmonster[is.na(finalmonster)] <- 0

finalmonster <- finalmonster %>% mutate(Barondiff = b_BARON_NASHOR-r_BARON_NASHOR) %>% mutate(Dragondiff = b_DRAGON -r_DRAGON) %>% mutate(fireDragondiff = b_FIRE_DRAGON -r_FIRE_DRAGON) %>% mutate(waterDragondiff = b_WATER_DRAGON -r_WATER_DRAGON) %>% mutate(airDragondiff = b_AIR_DRAGON -r_AIR_DRAGON) %>% mutate(earthDragondiff = b_EARTH_DRAGON -r_EARTH_DRAGON) %>% mutate(riftdiff = b_RIFT_HERALD - r_RIFT_HERALD)

###structuer part
structures$id <- str_sub(structures$Address,-16,-1)
structures$color <- str_sub(structures$Team,1,1)
structures$Turret <- paste0(structures$color,"_",structures$Type)
structures <- arrange(structures,id)
structures.1 <- structures %>% mutate(n = 1) %>% filter(!str_detect(Turret, "NA"))%>% group_by(id,Turret) %>% summarise(number = sum(n)) 
finalstructure <- structures.1 %>% spread(key = Turret , value = number)
finalstructure[is.na(finalstructure)] <- 0

finalstructure <- finalstructure %>% mutate(basediff = b_BASE_TURRET - r_BASE_TURRET) %>%  mutate(inhibitordiff = b_INHIBITOR - r_INHIBITOR) %>% mutate(innerdiff = b_INNER_TURRET - r_INNER_TURRET) %>% mutate(outerdiff = b_OUTER_TURRET -r_OUTER_TURRET) %>%  mutate(nexusdiff = b_NEXUS_TURRET-r_NEXUS_TURRET )


##Combination 
LOL.all <- LOL%>% left_join(maxgold,by = "id") %>% left_join(finalkill,by = "id") %>% left_join(finalmonster,by = "id") %>% left_join(finalstructure,by="id") %>% arrange(Year)

LOL.all$id2 <- c(1:7582) 


write.csv(LOL.all, file = "C:\\Users\\husiw\\OneDrive\\document\\MA678\\midterm project\\League of Legend\\LOL.all.csv")
