#import libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(plotly)
library(extrafont)
library(viridis) 
library(RColorBrewer)



#eliminate scientific notation
options(scipen=999)

# list of disciplines
disciplines <- c("Liberal Arts", "Science", "Fine Arts","Teacher Education","Agriculture",
                 "Engineering","Home Economics", "Law","Social Service","Library Science","Veterinary Science",
                 "Vocational Training","Physical Training","Health Services","Pharmacy","Business Admin",
                 "Optometry","Teacher Ed-Practice Teaching","Technology","Nursing")





project<- read_excel("Desktop/Work/Relative Weight /Relative Weights 2019.xlsx")
View(Relative_Weights_2019)


#____________________________________________







# run to receive a list of all disciplines
disciplines




read2 <- readline(prompt = "Which measure would you like? UGL, UGU, MAS, DOC, SP or Total? ")
read1 <- readline(prompt = "Which Discipline would u like to compare? ")


if (read2 == "UGL"){
  
  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGL to plot independently on boxplot
  utsaUGL <- inst%>%
    filter(Institution=="UT-San Antonio")
  ugl2 <- as.double(utsaUGL$UGL[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, UGL,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("UGL for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = ugl2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=ugl2, label="UTSA UGL",
             color="red")
  
  
  
  
}else if (read2== "UGU"){
  

  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGU to plot independently on boxplot
  utsaUGU <- inst%>%
    filter(Institution=="UT-San Antonio")
  ugu2 <- as.double(utsaUGU$UGU[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, UGU,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("UGU for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = ugu2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=ugu2, label="UTSA UGU",
             color="red")
  
} else if(read2=="MAS"){
  
  #MAS Graph
  
  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGU to plot independently on boxplot
  utsaMAS <- inst%>%
    filter(Institution=="UT-San Antonio")
  mas2 <- as.double(utsaMAS$MAS[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, MAS,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("MAS for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = mas2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=mas2, label="UTSA MAS",
             color="red")
  
  
  
  
}else if(read2=="DOC"){
  
  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGU to plot independently on boxplot
  utsaDOC <- inst%>%
    filter(Institution=="UT-San Antonio")
  doc2 <- as.double(utsaDOC$DOC[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, DOC,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("DOC for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = doc2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=doc2, label="UTSA DOC",
             color="red")
  
  
  
  
  
}else if(read2 =="SP"){
  
  
  #SP Graph
  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGU to plot independently on boxplot
  utsaSP <- inst%>%
    filter(Institution=="UT-San Antonio")
  sp2 <- as.double(utsaSP$SP[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, SP,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("SP for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = sp2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=sp2, label="UTSA SP",
             color="red")
  
  
  
}else if(read2 == "Total"){
  
  #Total Graph
  
  
  #Filtering only chosen Discipline
  inst <- project%>%
    filter(Discipline ==toString(read1))
  inst1 = toString(read1)
  regio = c(inst1)
  
  
  #Now Filtering only UTSA's UGU to plot independently on boxplot
  utsaTotal <- inst%>%
    filter(Institution=="UT-San Antonio")
  total2 <- as.double(utsaTotal$Total[1])
  
  
  
  #plot boxplot
  ggplot(inst, aes(Discipline, Total,color = Discipline ) )+
    geom_boxplot(fill= "#4271AE", color = "#1F3552", alpha = 0.7)+
    theme_bw()+
    labs(title = paste("Total for",regio[1], "Discipline"),
         subtitle = "2019")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=13))+
    # add utsa
    annotate("point", x = 1, y = total2, colour = "red", size = 3) +
    annotate(geom="text", x=1.05, y=total2, label="UTSA Total",
             color="red")
  
  
  
  
}else
  print("ERROR:Incorrect Input")






