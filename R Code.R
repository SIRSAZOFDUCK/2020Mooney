## Primary care prescribing and socioeconomic deprivation in England, 2019 
## Jess Mooney, Roger Yau, Farah Kidy, Sarah Hillman & Saran Shantikumar (May 2020) 
## Contact: saran.shantikumar@gmail.com

rm(list = ls())

setwd("CHANGE TO WORKING DIRECTORY") # This directory has 12 x monthly primary care prescribing .csv files from NHS Digital

list.of.packages <- c("data.table", "dplyr","plyr","stringr","ppcor","ggplot2","plotly","scales","Cairo","viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(plyr)
library(stringr)
library(ppcor)
library(ggplot2)
library(plotly)
library(scales)
library(Cairo)
library(viridis)

## Create an aggregated dataset for 1 year of prescribing data, from monthly NHS Digital data ----------------------


# read in .csv file of GP prescription data for one month
data201901 <- fread("2019_01.csv", header=T,sep = ',')
data201902 <- fread("2019_02.csv", header=T,sep = ',')
data201903 <- fread("2019_03.csv", header=T,sep = ',')

# bind data
data2019A <- rbind(data201901,data201902,data201903)

#remove data not required
rm(data201901) 
rm(data201902)
rm(data201903)

# aggregate
data2019A <- setDT(data2019A)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]

#Contine as above
data201904 <- fread("2019_04.csv", header=T,sep = ',')
data201905 <- fread("2019_05.csv", header=T,sep = ',')
data201906 <- fread("2019_06.csv", header=T,sep = ',')
data2019B <- rbind(data201904,data201905,data201906)
rm(data201904) 
rm(data201905)
rm(data201906)
data2019B <- setDT(data2019B)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]

data2019AB <- rbind(data2019A,data2019B)
data2019AB <- setDT(data2019AB)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]
rm(data2019A) 
rm(data2019B)

data201907 <- fread("2019_07.csv", header=T,sep = ',')
data201908 <- fread("2019_08.csv", header=T,sep = ',')
data201909 <- fread("2019_09.csv", header=T,sep = ',')
data2019C <- rbind(data201907,data201908,data201909)
rm(data201907) 
rm(data201908)
rm(data201909)
data2019C <- setDT(data2019C)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]

data201910 <- fread("2019_10.csv", header=T,sep = ',')
data201911 <- fread("2019_11.csv", header=T,sep = ',')
data201912 <- fread("2019_12.csv", header=T,sep = ',')
data2019D <- rbind(data201910,data201911,data201912)
rm(data201910) 
rm(data201911)
rm(data201912)
data2019D <- setDT(data2019D)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]

data2019CD <- rbind(data2019C,data2019D)
rm(data2019C) 
rm(data2019D)
data2019CD <- setDT(data2019CD)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]


data2019ABCD <- rbind(data2019AB,data2019CD)
rm(data2019AB) 
rm(data2019CD)
data2019ABCD <- setDT(data2019ABCD)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,`BNF CODE`,`BNF NAME`)]

#save complete 1 year aggregation
write.csv(data2019ABCD,"2019RxData.csv",row.names = F)

#Re-aggregate by BNF chemical code (9 characters)

data2019ABCD$chemical.code <- str_sub(data2019ABCD$`BNF CODE`,1,9) #extract chemical codes
dataZZ <- setDT(data2019ABCD)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,chemical.code)]
write.csv(dataZZ,"2019RxData_by.chemical.code.csv",row.names = F)

## Analyse all drug sections and all chemicals for association with deprivation ---------------

rm(list = ls())


data <- fread("2019RxData_by.chemical.code.csv", sep = ",",header = T) #aggregated precribing data
imd <- fread("practice_deprivation2.csv",sep = ",",header = T) #IMD scores by practice
listsize <- fread("practice_listsize.csv",sep = ",",header = T) # listsize, age and sex distribution by practice
bnf.lookup <- fread("bnf_codes_lookup.txt",colClasses = "character") # BNF lookup table - needed to add drug names to codes
bnf.lookup <- bnf.lookup[,c(5,6)] %>% unique() #keep chemical code and name columns
names(bnf.lookup) <- c("chemical.code","chemical.name") # rename to join with Rx data

# Add bnf lookup details to Rx data
data <- left_join(data,bnf.lookup,by="chemical.code")

#link practice data
data <- merge(data,imd,by="PRACTICE") # add IMD2019 score to data
data <- merge(data,listsize,by="PRACTICE") # add list size/age/sex to data


# Calculate items (prescriptions) per 1000 patients
data$ITEMS.PER.1000 <- 1000*(data$ITEMS / data$LIST.SIZE)

data %>% filter(!grepl("Dummy", chemical.name)) -> data #remove rows with "Dummy chemical name" in
data %>% filter(!grepl("15", chemical.code)) -> data #remove anaesthesia
data %>% filter(!grepl("19", chemical.code)) -> data #remove "other preparations"


### ANALYSES FOR INDIVIDUAL DRUGS ###

all.chemical.names <- as.vector(unique(data$chemical.name))

## Unadjusted analyses of deprivation vs prescribing rate association

results=list() #initiate a new list to hold all results

for(i in all.chemical.names){ #Spearmans rank for all included drugs
  data %>% filter(chemical.name==i) -> XX
  
  if(nrow(XX)>1000){ #set nrow threshold of 1000 so we don't analyse drugs where fewer than 1000 prescriptions were dispensed
    test.result <- cor.test(XX$IMD2019, XX$ITEMS.PER.1000, method="spearman")
    df<-data.frame(noquote(i),test.result$estimate,test.result$p.value,sum(XX$ITEMS))
    results[[i]] <- df 
  }
}

all.results <- do.call(rbind, results) #collate results
names(all.results)<-c("chemical.name","rho","p.value","items")  #rename columns
all.results <- all.results %>% filter(items>=1000) #remove rarely prescribed items. This should already be done, but just in case...


## Adjusted analyses of deprivation vs prescribing rate association (adjusting for practice list size, % male and % over-65s)

results=list() #reinitiate list to store results

for(i in all.chemical.names){
  data %>% filter(chemical.name==i) -> XX
  
  if(nrow(XX)>1000){
    test.result <- pcor.test(XX$IMD2019, XX$ITEMS.PER.1000, XX[,c(12,13,14)], method="spearman")
    df<-data.frame(noquote(i),test.result$estimate,test.result$p.value,sum(XX$ITEMS))
    results[[i]] <- df 
  }
}

all.results.adj <- do.call(rbind, results) # collate results
names(all.results.adj)<-c("chemical.name","rho.partial","p.value.partial","items.partial")  # rename columns
all.results.adj <- all.results.adj %>% filter(items.partial>=1000) #remove rarely prescribed items

#combine unadjusted and adjusted results & save
full.results <- merge(all.results,all.results.adj,by="chemical.name")
full.results$chemical <- as.character(full.results$chemical.name)
full.results <- merge(full.results,bnf.lookup,by="chemical.name")#add chemical codes
full.results <- full.results[!duplicated(full.results$chemical.name),]
full.results <- full.results %>% filter(chapter!="15") #remove anaesthesia chapter drugs
full.results$chemical.name <- factor(full.results$chemical.name, levels = full.results$chemical.name[order(full.results$chemical.code)]) #specify order by chemical code
full.results$chapter <- str_sub(full.results$chemical.code,1,2) # define BNF chapter
full.results$paragraph <- str_sub(full.results$chemical.code,1,6) # define BNF paragraph

bnf.lookup <- fread("bnf_codes_lookup.txt",colClasses = "character") # BNF lookup table - needed to add drug names to codes
bnf.lookup <- bnf.lookup[,c(5,7:12,14)] %>% unique() #keep chemical code and section/paragraph codes and names
names(bnf.lookup) <- c("chemical.code","subparagraph.code","subparagraph.name","paragraph.code","paragraph.name","section.code","section.name","chapter.name") # rename to join with results data

full.results <- left_join(full.results,bnf.lookup,by="chemical.code") # Add bnf lookup details to results data

full.results$p.value[full.results$p.value == 0] <- 1e-320
full.results$p.value.partial[full.results$p.value.partial == 0] <- 1e-320

write.csv(full.results,"results_by_drug.csv",row.names = F) # save


### ANALYSES FOR INDIVIDUAL DRUG CLASSES (PARAGRAPHS) ###
rm(list=ls())


data <- fread("2019RxData_by.chemical.code.csv", sep = ",",header = T) #aggregated precribing data
data$paragraph.code <- str_sub(data$chemical.code,1,6) #extract chemical codes
data <- setDT(data)[,.(ITEMS = sum(ITEMS),QUANTITY = sum(QUANTITY),NIC = sum(NIC),`ACT COST`=sum(`ACT COST`)), by = .(PRACTICE,paragraph.code)]

imd <- fread("practice_deprivation2.csv",sep = ",",header = T) #IMD scores by practice
listsize <- fread("practice_listsize.csv",sep = ",",header = T) # listsize, age and sex distribution by practice
bnf.lookup <- fread("bnf_codes_lookup.txt",colClasses = "character") # BNF lookup table - needed to add drug names to codes
bnf.lookup <- bnf.lookup[,c(9,10)] %>% unique() #keep paragraph code and name columns
names(bnf.lookup) <- c("paragraph.code","paragraph.name") # rename to join with Rx data

# Add bnf lookup details to Rx data
data <- left_join(data,bnf.lookup,by="paragraph.code")

#link practice data
data <- merge(data,imd,by="PRACTICE") # add IMD2019 score to data
data <- merge(data,listsize,by="PRACTICE") # add list size/age/sex to data


# Calculate items (prescriptions) per 1000 patients
data$ITEMS.PER.1000 <- 1000*(data$ITEMS / data$LIST.SIZE)

data %>% filter(!grepl("Dummy", paragraph.name)) -> data #remove rows with "Dummy chemical name" in

#create vector of paragraph names
all.paragraph.name <- as.vector(unique(data$paragraph.name))

## Unadjusted analyses of deprivation vs prescribing rate association

results=list() #initiate a new list to hold all results

for(i in all.paragraph.name){ #Spearmans rank for all included drugs
  data %>% filter(paragraph.name==i) -> XX
  
  if(nrow(XX)>1000){ #set nrow threshold of 1000 so we don't analyse drugs where fewer than 1000 prescriptions were dispensed
    test.result <- cor.test(XX$IMD2019, XX$ITEMS.PER.1000, method="spearman")
    df<-data.frame(noquote(i),test.result$estimate,test.result$p.value,sum(XX$ITEMS))
    results[[i]] <- df 
  }
}

all.results <- do.call(rbind, results) #collate results
names(all.results)<-c("paragraph.name","rho","p.value","items")  #rename columns
all.results <- all.results %>% filter(items>=1000) #remove rarely prescribed items. This should already be done, but just in case...

## Adjusted analyses of deprivation vs prescribing rate association (adjusting for practice list size, % male and % over-65s)

results=list() #reinitiate list to store results

for(i in all.paragraph.name){
  data %>% filter(paragraph.name==i) -> XX
  
  if(nrow(XX)>1000){
    test.result <- pcor.test(XX$IMD2019, XX$ITEMS.PER.1000, XX[,c(12,13,14)], method="spearman")
    df<-data.frame(noquote(i),test.result$estimate,test.result$p.value,sum(XX$ITEMS))
    results[[i]] <- df 
  }
}

all.results.adj <- do.call(rbind, results) # collate results
names(all.results.adj)<-c("paragraph.name","rho.partial","p.value.partial","items.partial")  # rename columns
all.results.adj <- all.results.adj %>% filter(items.partial>=1000) #remove rarely prescribed items

#combine unadjusted and adjusted results & save
full.results <- merge(all.results,all.results.adj,by="paragraph.name")
#full.results$chemical <- as.character(full.results$chemical.name)
full.results <- merge(full.results,bnf.lookup,by="paragraph.name")#add chemical codes
full.results <- full.results[!duplicated (full.results$paragraph.name),]
full.results$paragraph.name <- factor(full.results$paragraph.name, levels = full.results$paragraph.name[order(full.results$paragraph.code)]) #specify order by paragraph code
full.results$chapter <- str_sub(full.results$paragraph.code,1,2) # define BNF chapter
full.results <- full.results %>% filter(chapter!="15") #remove anaesthesia chapter drugs
full.results <- full.results %>% filter(chapter!="19") #remove other preparations chapter drugs

bnf.lookup <- fread("bnf_codes_lookup.txt",colClasses = "character") # BNF lookup table - needed to add drug names to codes
bnf.lookup <- bnf.lookup[,c(5,7:12,14)] %>% unique() #keep chemical code and section/paragraph codes and names
names(bnf.lookup) <- c("chemical.code","subparagraph.code","subparagraph.name","paragraph.code","paragraph.name","section.code","section.name","chapter.name") # rename to join with results data
bnf.lookup <- bnf.lookup[,-c(1:3)] %>% unique() #drop chemical code /subparagraph details 

full.results <- left_join(full.results,bnf.lookup,by="paragraph.code") # Add bnf lookup details to results data

full.results$p.value[full.results$p.value == 0] <- 1e-320
full.results$p.value.partial[full.results$p.value.partial == 0] <- 1e-320

write.csv(full.results,"results_by_class.csv",row.names = F) # save


## Plot results for associations of prescribing with deprivation ----------------

# Plots of individual drugs

full.results <- read.csv("results_by_drug.csv") #read in data
full.results$chemical.name <- factor(full.results$chemical.name, levels = full.results$chemical.name[order(full.results$chemical.code)]) #specify order by chemical code                   )
full.results$chapter <- as.factor(full.results$chapter)
full.results <- full.results %>% filter(chapter!="15") #remove anaesthesia chapter drugs

# plot adjusted p values (horizontal line is Bonferroni-adjusted p value threshold: 0.05/1000)
p<-ggplot(full.results, aes(x=chemical.name, y=-log10(p.value.partial),colour=chapter, size = items,
                            text=paste0("Chemical Name: ",chemical.name,
                                        "<br>",
                                        "BNF Chapter: ",chapter,
                                        "<br>",
                                        "Number of items: ",items,
                                        "<br>",
                                        "-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+ 
  scale_x_discrete() + geom_hline(aes(yintercept = 4.3,colour="Significance\nthreshold"), linetype=2) +
  geom_hline(aes(yintercept = 0),colour="#a9a9a9") +
  xlab("\nDrug / Chemical") + ylab("-log10 (p value)\n") +
  scale_y_continuous(breaks = seq(0, 400, by = 100))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12)) +
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "BNF Chapter") + #legend title
  scale_size(guide = "none")+  #suppress legend for items
  theme(legend.key.size = unit(6,"line"))
  #scale_color_viridis(discrete = TRUE, option = "D") 


save1 <- ggplotly(p,tooltip = "text")
htmlwidgets::saveWidget(save1, "Figure 1A. P values by drug.html")

#plot adjusted Spearmans rank correlation results
q<- ggplot(full.results, aes(x=chemical.name, y=rho.partial,colour=chapter, size = items,
                             text=paste0("Chemical Name: ",chemical.name,
                                         "<br>",
                                         "BNF Chapter: ",chapter,
                                         "<br>",
                                         "Number of items: ",items,
                                         "<br>",
                                         "Correlation coefficient (adjusted):  ",round(rho.partial,2)))) +
  geom_point(alpha=0.7)+ 
  scale_x_discrete() + 
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n") +
  scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "BNF Chapter") + #legend title
  scale_size(guide = "none") #+#suppress legend for items
  #scale_color_viridis(discrete = TRUE, option = "D")  #colour scheme
  

save2 <- ggplotly(q,tooltip = "text")
htmlwidgets::saveWidget(save2, "Figure 1B. Correlation coefficients by drug.html")


# Plots of individual drugs, by chapter: correlation coefficient with points coloured if significant

full.results$significance[full.results$p.value.partial < 0.00005]<- "Significant" #set column showing if significant (to bonferonni corrected p value)
full.results$significance[full.results$p.value.partial >= 0.00005]<- "Not Significant" 

full.results1 <- full.results %>% filter(chapter=="1")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                             text=paste0("Chemical Name: ",chemical.name,
                                         "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                         "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                         "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 1. Gastrointestinal System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2A. Correlation coefficients by drug Ch01.html")


full.results1 <- full.results %>% filter(chapter=="2")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 2. Cardiovascular System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2B. Correlation coefficients by drug Ch02.html")

full.results1 <- full.results %>% filter(chapter=="3")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 3. Respiratory System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2C. Correlation coefficients by drug Ch03.html")

full.results1 <- full.results %>% filter(chapter=="4")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 4. Central Nervous System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2D. Correlation coefficients by drug Ch04.html")

full.results1 <- full.results %>% filter(chapter=="5")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 5. Infections") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2E. Correlation coefficients by drug Ch05.html")

full.results1 <- full.results %>% filter(chapter=="6")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 6. Endocrine System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2F. Correlation coefficients by drug Ch06.html")

full.results1 <- full.results %>% filter(chapter=="7")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 7. Obstetrics, Gynaecology and Urinary Tract Disorders") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2G. Correlation coefficients by drug Ch07.html")

full.results1 <- full.results %>% filter(chapter=="8")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 8. Malignant Disease & Immunosuppression") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2H. Correlation coefficients by drug Ch08.html")

full.results1 <- full.results %>% filter(chapter=="9")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 9. Nutrition & Blood") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2I. Correlation coefficients by drug Ch09.html")

full.results1 <- full.results %>% filter(chapter=="10")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 10. Musculoskeletal & Joint Diseases") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2J. Correlation coefficients by drug Ch10.html")

full.results1 <- full.results %>% filter(chapter=="11")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 11. Eye") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2K. Correlation coefficients by drug Ch11.html")

full.results1 <- full.results %>% filter(chapter=="12")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 12. Ear, Nose & Oropharynx") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2L. Correlation coefficients by drug Ch12.html")

full.results1 <- full.results %>% filter(chapter=="13")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 13. Skin") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2M. Correlation coefficients by drug Ch13.html")

full.results1 <- full.results %>% filter(chapter=="14")
q<- ggplot(full.results1, aes(x=chemical.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Chemical Name: ",chemical.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 14. Immunological Products & Vaccines") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 2N. Correlation coefficients by drug Ch14.html")


# By paragraph (drug class)

rm(list = ls())

full.results <- read.csv("results_by_class.csv") #read in data
names(full.results)[1] <- "paragraph.name"
full.results$paragraph.name <- factor(full.results$paragraph.name, levels = full.results$paragraph.name[order(full.results$paragraph.code)]) #specify order by chemical code                   )
full.results$chapter <- as.factor(full.results$chapter)
full.results <- full.results %>% filter(chapter!="15") #remove anaesthesia chapter drugs

# plot adjusted p values (horizontal line is Bonferroni-adjusted p value threshold: 0.05/1000)
p<-ggplot(full.results, aes(x=paragraph.name, y=-log10(p.value.partial),colour=chapter, size = items,
                            text=paste0("Paragraph Name: ",paragraph.name,
                                        "<br>",
                                        "BNF Chapter: ",chapter,
                                        "<br>",
                                        "Number of items: ",items,
                                        "<br>",
                                        "-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+ 
  scale_x_discrete() + geom_hline(aes(yintercept = 4.3,colour="Significance\nthreshold"), linetype=2) +
  geom_hline(aes(yintercept = 0),colour="#a9a9a9") +
  xlab("\nDrug / Chemical Class") + ylab("-log10 (p value)\n") +
  scale_y_continuous(breaks = seq(0, 400, by = 100))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12)) +
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "BNF Chapter") + #legend title
  scale_size(guide = "none")#+  #suppress legend for items
  theme(legend.key.size = unit(6,"line"))
#scale_color_viridis(discrete = TRUE, option = "D") 

save1 <- ggplotly(p,tooltip = "text")
htmlwidgets::saveWidget(save1, "Figure 3A. P values by drug class.html")

#plot adjusted Spearmans rank correlation results
q<- ggplot(full.results, aes(x=paragraph.name, y=rho.partial,colour=chapter, size = items,
                             text=paste0("Paragraph Name: ",paragraph.name,
                                         "<br>",
                                         "BNF Chapter: ",chapter,
                                         "<br>",
                                         "Number of items: ",items,
                                         "<br>",
                                         "Correlation coefficient (adjusted):  ",round(rho.partial,2)))) +
  geom_point(alpha=0.7)+ 
  scale_x_discrete() + 
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical Class") + ylab("Correlation coefficient (rho)\n") +
  scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "BNF Chapter") + #legend title
  scale_size(guide = "none") #+#suppress legend for items
#scale_color_viridis(discrete = TRUE, option = "D")  #colour scheme

save2 <- ggplotly(q,tooltip = "text")
htmlwidgets::saveWidget(save2, "Figure 3B. Correlation coefficients by drug class.html")

# Plots of individual drugs, by chapter: correlation coefficient with points coloured if significant

full.results$significance[full.results$p.value.partial < 0.00005]<- "Significant" #set column showing if significant (to bonferonni corrected p value)
full.results$significance[full.results$p.value.partial >= 0.00005]<- "Not Significant" 

full.results1 <- full.results %>% filter(chapter=="1")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 1. Gastrointestinal System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4A. Correlation coefficients by drug Ch01.html")


full.results1 <- full.results %>% filter(chapter=="2")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 2. Cardiovascular System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4B. Correlation coefficients by drug Ch02.html")

full.results1 <- full.results %>% filter(chapter=="3")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 3. Respiratory System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4C. Correlation coefficients by drug Ch03.html")

full.results1 <- full.results %>% filter(chapter=="4")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 4. Central Nervous System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4D. Correlation coefficients by drug Ch04.html")

full.results1 <- full.results %>% filter(chapter=="5")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 5. Infections") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4E. Correlation coefficients by drug Ch05.html")

full.results1 <- full.results %>% filter(chapter=="6")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 6. Endocrine System") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4F. Correlation coefficients by drug Ch06.html")

full.results1 <- full.results %>% filter(chapter=="7")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 7. Obstetrics, Gynaecology and Urinary Tract Disorders") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4G. Correlation coefficients by drug Ch07.html")

full.results1 <- full.results %>% filter(chapter=="8")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 8. Malignant Disease & Immunosuppression") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4H. Correlation coefficients by drug Ch08.html")

full.results1 <- full.results %>% filter(chapter=="9")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 9. Nutrition & Blood") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4I. Correlation coefficients by drug Ch09.html")

full.results1 <- full.results %>% filter(chapter=="10")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 10. Musculoskeletal & Joint Diseases") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4J. Correlation coefficients by drug Ch10.html")

full.results1 <- full.results %>% filter(chapter=="11")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 11. Eye") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4K. Correlation coefficients by drug Ch11.html")

full.results1 <- full.results %>% filter(chapter=="12")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 12. Ear, Nose & Oropharynx") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4L. Correlation coefficients by drug Ch12.html")

full.results1 <- full.results %>% filter(chapter=="13")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 13. Skin") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4M. Correlation coefficients by drug Ch13.html")

full.results1 <- full.results %>% filter(chapter=="14")
q<- ggplot(full.results1, aes(x=paragraph.name, y=rho.partial, colour=significance, size = items,
                              text=paste0("Paragraph Name: ",paragraph.name,
                                          "<br>","BNF Chapter: ",chapter,"<br>","Number of items: ",items,
                                          "<br>","Correlation coefficient (adjusted):  ",round(rho.partial,2),
                                          "<br>","-log10(p value): ",floor(-log10(p.value.partial))))) +
  geom_point(alpha=0.7)+
  scale_color_manual(values=c("grey", "black","red"))+
  geom_hline(aes(yintercept = 0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0.2,colour="Correlation\ncoefficient\n+/- 0.2"), linetype="dashed") +
  geom_hline(aes(yintercept = -0),colour="black") +
  xlab("Drug / Chemical") + ylab("Correlation coefficient (rho)\n\n\n") +
  scale_y_continuous(limits = c(-0.6, 0.6),breaks = seq(-0.6, 0.6, by = 0.2),labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))+
  theme(axis.ticks.x = element_blank(),axis.text.x=element_blank(),axis.title=element_text(size = 14),axis.text.y=element_text(size = 12))+
  theme(plot.margin=unit(c(3,8,1,1),"cm"))+
  labs(colour = "Significance") + #legend title
  scale_size(guide = "none") +#suppress legend for items
  theme(panel.background = element_rect(fill = "white",colour = "grey",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "#F5F5F5"))+
  ggtitle("Chapter 14. Immunological Products & Vaccines") +
  theme(plot.title = element_text(hjust = 0.5))
htmlwidgets::saveWidget(ggplotly(q,tooltip = "text"), "Figure 4N. Correlation coefficients by drug Ch14.html")



