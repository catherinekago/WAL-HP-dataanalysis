library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(forcats)

# Add your working directory (ADJUST)
# setwd("C:/Users/kathr_/OneDrive/Desktop/HCI Master/2.Semester/WAL/Studie/Datenauswertung")

# Retrieve the excel sheet (ADJUST)
exceldata = read_excel("C:\\Users\\kathr_\\OneDrive\\Desktop\\HCI Master\\2.Semester\\WAL\\Studie\\Survey zur Involvierung in die Harry Potter Fankultur (Antworten).xlsx")                                                                            
data = data.frame(exceldata)

TIMER = 25 #minutes the timer was set to by us
MAX_HPFKS = 4 * 14 + 5;

color_gryffindor = "#A70001";
color_slytherin = "#2F7F4B";
color_hufflepuff = "#FFD800";
color_ravenclaw = "#374C93"
houseColors = c(color_gryffindor, color_slytherin, color_ravenclaw, color_hufflepuff)

tidyUpDataframe = function(df){
  
  # remove participant names and timer data at the end of the questionnaire
  df = df[-c(1,3)];
  
  # Replace Prefects with Hogwarts houses
  df[2][df[2] == "Gemma Farley"] <- "Slytherin"
  df[2][df[2] == "Percy Weasly"] <- "Gryffindor"
  df[2][df[2] == "Gabriel Truman"] <- "Hufflepuff"
  df[2][df[2] == "Robert Hilliard"] <- "Ravenclaw"
  
  # Remove empty rows
  df = df[!(is.na(df[2])),];
  
  #Change rownames to match number of rows
  row.names(df) <- c(1:nrow(df));
  
  # Calculate completiontime
  for(i in 1:nrow(df)){
    # Skip rows with empty time stamp
    if (!is.na(df[i,1])){
      isInTime = substr(df[i,1],1,9) == "In time (";
      if(isInTime){
        df[i,1] = stringr::str_extract_all(df[i,1], "\\d+");
      } else {
        df[i,1] = as.numeric(stringr::str_extract_all(df[i,1], "\\d+")) + TIMER;
      }
    }
  }  
  
  # Replace Column names with more readable tags
  colnames(df) <- c("Completiontime(mm)", "HogwartsHouse", "HP_Movies_+", "HP_Movies_-", "HP_Books_+", "HP_Books_-", "HP_World_+", "HP_World_-", 
                    "HP_Identification_+", "HP_Identification_-", "FreeTV_+", "FreeTV_-", "Read_+", "Read_-", "HP_SortingTest_+", "HP_SortingTest_-", "HP_Podcasts_+", "HP_Podcasts_-", "HP_Drawn_+", "HP_Drawn_-", "Coldmirror_+", "Coldmirror_-", "HP_Talk_+", "HP_Talk_-", 
                    "HP_MoviePosEmotion_+", "HP_MoviePosEMotion_-", "HP_Tattoo_+", "HP_Tattoo_-", "HP_Quotes_+", "HP_Quotes_-", "HP_BooksRead", "HP_BookCount", "HP_MovieCount", "HP_Merch", "HP_MerchCount", "HP_MerchHouse", 
                    "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15");
  return (df); 

}

tidyData = tidyUpDataframe(data);


#  Create separate df for likert scale and convert answers to values, add score column

createLikertDf = function(df){
  likertDf = data.frame(df[c(2:31)]);
  
  # Replace all likert options with numbers
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Stimme überhaupt nicht zu", -2, x) }))
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Stimme nicht zu", -1, x) }))
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Weder noch", 0, x) }))
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Stimme zu", 1, x) }))
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Stimme völlig zu", 2, x) }))
  
  # Replace all response options of "HP_BooksRead" with numbers
  likertDf[30][likertDf[30] == "Kein einziges"] <- 0
  likertDf[30][likertDf[30] == "Nur das erste Buch"] <- 1
  likertDf[30][likertDf[30] == "Ich bin nicht über den fünften Teil hinaus gekommen"] <- 2
  likertDf[30][likertDf[30] == "Alle ein Mal"] <- 3
  likertDf[30][likertDf[30] == "Alle ein mal, einige mehrfach"] <- 4
  likertDf[30][likertDf[30] == "Alle mehrfach"] <- 5
  
  # Convert characters to numeric and factors
  likertDf[, c(2:30)] <- sapply(likertDf[, c(2:30)], as.numeric)
  
  # Reverse negatively phrased items
  for (i in 2:(ncol(likertDf)-1)){
    if (i %% 2 == 1){ 
      likertDf[i] = likertDf[i]*(-1);
    }
  }
  
  # Adjust values of likert scale
  HogwartsHouse = likertDf$HogwartsHouse;
  HP_Movies = c();
  HP_Books = c();
  HP_World = c();
  HP_Identification = c();
  FreeTV = c();
  Read = c();
  HP_SortingTest = c();
  HP_Podcasts = c();
  HP_Drawn = c();
  Coldmirror = c();
  HP_Talk = c();
  HP_MoviePosEmotion = c();
  HP_Tattoo = c();
  HP_Quotes = c();
  HP_BooksRead = likertDf$HP_BooksRead;
  for(row in 1:nrow(likertDf)){
    HP_Movies = c(HP_Movies, (likertDf[row,2] + likertDf[row,3])/2 + 2);
    HP_Books = c(HP_Books, (likertDf[row,4] + likertDf[row,5])/2 + 2);
    HP_World = c(HP_World, (likertDf[row,6] + likertDf[row,7])/2 + 2);
    HP_Identification = c(HP_Identification, (likertDf[row,8] + likertDf[row,9])/2 + 2);
    FreeTV = c(FreeTV, (likertDf[row,10] + likertDf[row,11])/2 + 2);
    Read = c(Read, (likertDf[row,12] + likertDf[row,13])/2 + 2);
    HP_SortingTest = c(HP_SortingTest, (likertDf[row,14] + likertDf[row,15])/2 + 2);
    HP_Podcasts = c(HP_Podcasts, (likertDf[row,16] + likertDf[row,17])/2 + 2);
    HP_Drawn = c(HP_Drawn, (likertDf[row,18] + likertDf[row,19])/2 + 2);
    Coldmirror = c(Coldmirror, (likertDf[row,20] + likertDf[row,21])/2 + 2);
    HP_Talk = c(HP_Talk, (likertDf[row,22] + likertDf[row,23])/2 + 2);
    HP_MoviePosEmotion = c(HP_MoviePosEmotion, (likertDf[row,24] + likertDf[row,25])/2 + 2);
    HP_Tattoo = c(HP_Tattoo, (likertDf[row,26] + likertDf[row,27])/2 + 2);
    HP_Quotes = c(HP_Quotes, (likertDf[row,28] + likertDf[row,29])/2 + 2);
  }


  likertDf = data.frame(HogwartsHouse, HP_Movies, HP_Books,HP_World, HP_Identification, FreeTV,Read, HP_SortingTest, HP_Podcasts, HP_Drawn, Coldmirror, HP_Talk, HP_MoviePosEmotion, HP_Tattoo, HP_Quotes, HP_BooksRead);
  
  # Add score column
  likertDf$Score = rowSums( likertDf[,2:16] )
  return (likertDf);
}
likertData = createLikertDf(tidyData);


#  Create separate df for merch
createMerchDf = function(df){
  merchDf = data.frame(df[c(2, 32:36)]);
  
  merchDf[5][is.na(merchDf[5])] <- 0
  
  # Initialize column for each house
  HP_MerchHouse_G = c();
  HP_MerchHouse_H = c();
  HP_MerchHouse_R = c();
  HP_MerchHouse_S = c();
  
  # Identify merch of houses
  for(i in 1:nrow(merchDf)){
    if (grepl( "Gryffindor", merchDf[i,6], fixed = TRUE)){
      HP_MerchHouse_G = c(HP_MerchHouse_G, TRUE);
    } else {
      HP_MerchHouse_G = c(HP_MerchHouse_G, FALSE);
    }
    if (grepl( "Hufflepuff", merchDf[i,6], fixed = TRUE)){
      HP_MerchHouse_H = c(HP_MerchHouse_H, TRUE);
    } else {
      HP_MerchHouse_H = c(HP_MerchHouse_H, FALSE);
    }
    if (grepl( "Ravenclaw", merchDf[i,6], fixed = TRUE)){
      HP_MerchHouse_R = c(HP_MerchHouse_R, TRUE);
    } else {
      HP_MerchHouse_R = c(HP_MerchHouse_R, FALSE);
    }
    if (grepl( "Slytherin", merchDf[i,6], fixed = TRUE)){
      HP_MerchHouse_S = c(HP_MerchHouse_S, TRUE);
    } else {
      HP_MerchHouse_S = c(HP_MerchHouse_S, FALSE);
    }
  }
  
  merchDf$HP_MerchHouse_G = HP_MerchHouse_G;
  merchDf$HP_MerchHouse_H = HP_MerchHouse_H;
  merchDf$HP_MerchHouse_R = HP_MerchHouse_R;
  merchDf$HP_MerchHouse_S = HP_MerchHouse_S; 
  
  return (merchDf);

}

merchData = createMerchDf(tidyData);

# Create Dataframe for MerchHouse
createMerchHouseDf = function(){
  merchHouseDf = data.frame(merchData[c(1, 4, 5, 7:10)]);
  
  houses = c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin");
  
  nTotal = c();
  nWithMerch =c();
  nWithoutMerch = c();
  noHouseMerch = c(); 
  merch_G = c();
  merch_H = c();
  merch_R = c();
  merch_S = c();
  
  for (house in houses){
    houseDf <- subset(merchHouseDf, HogwartsHouse == house, select=c(HogwartsHouse, HP_Merch, HP_MerchCount, HP_MerchHouse_G, HP_MerchHouse_H, HP_MerchHouse_R, HP_MerchHouse_S));
    
    # Identify participants in total
    nTotal = c(nTotal, nrow(houseDf));
    
    # Identify participants with merch total
    nWithMerch =c(nWithMerch, sum(houseDf$HP_Merch == "Ja", na.rm = TRUE));
    
    # Identify participants with no merch at all
    nWithoutMerch = c(nWithoutMerch, (length(houseDf$HP_Merch) - sum(houseDf$HP_Merch == "Ja", na.rm = TRUE)));
    
    
    # Identify participants with no house related merch
    noHouseMerch = c(noHouseMerch, nrow(subset(houseDf, HP_Merch == "Ja" && HP_MerchHouse_G == FALSE && HP_MerchHouse_H == FALSE && HP_MerchHouse_R == FALSE && HP_MerchHouse_S == FALSE, select=c(HogwartsHouse))));
    
    # Identify participants with Gryffindor merch
    merch_G = c(merch_G, nrow(subset(houseDf, HP_MerchHouse_G == TRUE, select=c(HogwartsHouse))));
    
    # Identify participants with Hufflepuff merch
    merch_H = c(merch_H, nrow(subset(houseDf, HP_MerchHouse_H == TRUE, select=c(HogwartsHouse))));
    
    # Identify participants with Ravenclaw merch
    merch_R = c(merch_R, nrow(subset(houseDf, HP_MerchHouse_R == TRUE, select=c(HogwartsHouse))));
    
    # Identify participants with Slytherin merch
    merch_S = c(merch_S, nrow(subset(houseDf, HP_MerchHouse_S == TRUE, select=c(HogwartsHouse))));
    
    
  }
  merchHouseSummaryDf = data.frame(houses, nTotal, nWithMerch, nWithoutMerch ,merch_G, merch_H, merch_R, merch_S, noHouseMerch);
  
  colnames(merchHouseSummaryDf) <- c("HogwartsHouse", "nTotal", "nWithMerch", "nWithoutMerch", "merch_G", "merch_H", "merch_R", "merch_S", "noHouseMerch");
  
  # create relative values

  merchHouseSummaryDf$nWithoutMerch = merchHouseSummaryDf$nWithoutMerch/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$merch_G = merchHouseSummaryDf$merch_G/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$merch_H = as.numeric(merchHouseSummaryDf$merch_H)/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$merch_R = as.numeric(merchHouseSummaryDf$merch_R)/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$merch_S = merchHouseSummaryDf$merch_S/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$noHouseMerch = merchHouseSummaryDf$noHouseMerch/merchHouseSummaryDf$nWithMerch*100;
  merchHouseSummaryDf$nWithMerch = merchHouseSummaryDf$nWithMerch/merchHouseSummaryDf$nTotal*100;
  return (merchHouseSummaryDf);
}


# Create separate df for quiz, convert answers to values (1 and 0), add score column
creatQuizDf = function(df){
  quizDf = data.frame(df[c(2, 37:51)]);
  
  correctAnswers =c("Mit seinem Mund", "Severus Snape", "Gringotts", "Ligusterweg", "Ginny", "Dobby", "Mollyröllchen", "Abteilung Missbrauch von Muggelartefakten", "Bei Zonkos", "Spielzeugbesen", "713", "5", "21", "Es ist unter anderem mit dem Kabinett bei Borgin & Burkes verbunden.", "Mimbulus Mimbeltonia");
  
  # Replace correct answers with 1
  for(i in 1:length(correctAnswers)){
    quizDf[i+1][quizDf[i+1] == correctAnswers[i]] <- 1;
  }
  
  # Replace wrong answers with 0
  quizDf[2:16][quizDf[2:16] != 1] <- 0;
  
  # Convert columns to numeric
  quizDf <- quizDf %>% mutate_at(c(2:16), as.numeric)
  
  
  # Add score column
  quizDf$Score = rowSums( quizDf[,2:16])
  
  return (quizDf);
  
}

quizData = creatQuizDf(tidyData)

### Plotting

## Pie Chart Hogwarts House Sorting 
createPieChart = function()
{
  houseDf = as.data.frame(table(tidyData$HogwartsHouse))
  colnames(houseDf) = c("HogwartsHouse", "Count");
  # Basic piechart
  pie = ggplot(houseDf, aes(x="", y=Count, fill=HogwartsHouse)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    geom_label(size=9, aes(label = trunc(Count/sum(Count)* 100*10^2)/10^2), color = c("white", "black", "white", "white"),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    guides(fill = guide_legend(title = "Hogwartshaus")) +
    scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
    ggtitle("Hogwartshaus Zugehörigkeit\n") +
    theme_void() + # remove background, grid, numeric labels 
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
      legend.position="top",
          legend.margin=margin(40,0,0,0),
          legend.box.margin=margin(-20,-20,-20, -20))
  jpeg(file="hogwartshaus_zugehoerigkeit.png", width=600, height=600)
  plot(pie);
  dev.off();
}
createPieChart()

## Boxplot HPFKS
createBoxplotHPFKS = function()
{

  # Basic Boxplot
  boxplot = ggplot(likertData, aes(x=HogwartsHouse, y=Score, fill=HogwartsHouse)) +
    geom_boxplot() +
    ylab("HPFKS") +
    scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
    ggtitle("Harry Potter Fankultur Score (HPFKS)") +
    coord_cartesian(ylim = c(0, MAX_HPFKS)) +
    guides(fill = guide_legend(title = "Hogwartshaus")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
          legend.position="top",
          legend.margin=margin(40,0,0,0),
          axis.title = element_text(size=18),
          axis.text = element_text(size=16),
          legend.box.margin=margin(10,10,10, 10),
          axis.text.x=element_blank(),
          axis.title.x=element_blank())
  jpeg(file="hogwartshaus_HPFKS.png", width=600, height=600)
  plot(boxplot);
  dev.off();
}
createBoxplotHPFKS();

## Boxplot MerchCount
createBoxplotMerchCount = function()
{
  
  # Basic Boxplot
  boxplot = ggplot(merchData, aes(x=HogwartsHouse, y=HP_MerchCount, fill=HogwartsHouse)) +
    geom_boxplot() +
    ylab("Anzahl Merchartikel") +
    scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
    ggtitle("Anzahl von Merchartikeln nach Hogwartshaus") +
    guides(fill = guide_legend(title = "Hogwartshaus")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
          legend.position="top",
          legend.margin=margin(40,0,0,0),
          axis.title = element_text(size=18),
          axis.text = element_text(size=16),
          legend.box.margin=margin(10,10,10, 10),
          axis.text.x=element_blank(),
          axis.title.x=element_blank())
  jpeg(file="hogwartshaus_MerchCount.png", width=600, height=600)
  plot(boxplot);
  dev.off();
}
createBoxplotMerchCount();

## Barplot MerchYesNo
merchYesNo <- createMerchHouseDf() %>% gather(key = Merch, value = Value, nWithMerch:nWithoutMerch)
createBarplotMerchYesNo = function(){
  
  # Basic Barplot
  barplot = ggplot(merchYesNo, aes(Merch, Value, fill = HogwartsHouse)) + geom_col(position = "dodge") + 
    ylab("Teilnehmer:innen (%)") +
    xlab("Besitz von Merchartikeln") +
    scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
    ggtitle("Besitz von Merchartikeln nach Hogwartshaus") +
    guides(fill = guide_legend(title = "Hogwartshaus")) +
    theme_bw() + 
    scale_x_discrete(labels=c("nWithMerch" = "Ja", "nWithoutMerch" = "Nein",
                                "2" = "Dose 2")) +
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
          legend.position="top",
          legend.margin=margin(40,0,0,0),
          axis.title = element_text(size=18),
          axis.text = element_text(size=16),
          legend.box.margin=margin(10,10,10, 10))
  jpeg(file="hogwartshaus_MerchYesNo.png", width=600, height=600)
  plot(barplot);
  dev.off();
  
}

createBarplotMerchYesNo();

## Barplot MerchHouse

merchType <- createMerchHouseDf() %>% gather(key = Merch, value = Value, merch_G:noHouseMerch)

merchType$Merch[merchType$Merch == "merch_G"] = "Gryffindor";
merchType$Merch[merchType$Merch == "merch_H"] = "Hufflepuff";
merchType$Merch[merchType$Merch == "merch_R"] = "Ravenclaw";
merchType$Merch[merchType$Merch == "merch_S"] = "Slytherin";
merchType$Merch[merchType$Merch == "noHouseMerch"] = "Haus\n-unabhängig";
merchType$Merch[merchType$Merch == "nWithoutMerch"] = "kein Merch";
merchType$Merch <- factor(merchType$Merch,levels = c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin", "Haus\n-unabhängig"));


createBarplotMerchType = function()
{
  
  # Basic Barplot
  barplot = ggplot(merchType, aes(Merch, Value, fill = HogwartsHouse)) + geom_col(position = "dodge") + 
    ylab("Merchbesitzende Teilnehmer:innen (%)") +
    xlab("Merchartikel Art") +
      scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
      ggtitle("Arten von Merchartikeln nach Hogwartshaus") +
      guides(fill = guide_legend(title = "Hogwartshaus")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
          legend.position="top",
          legend.margin=margin(40,0,0,0),
          axis.title = element_text(size=18),
          axis.text = element_text(size=16),
          legend.box.margin=margin(10,10,10, 10))
  jpeg(file="hogwartshaus_MerchType.png", width=600, height=600)
  plot(barplot);
  dev.off();
}
createBarplotMerchType();

## Boxplot HPGWS
createBoxplotHPGWS = function()
{
  
  # Basic Boxplot
  boxplot = ggplot(quizData, aes(x=HogwartsHouse, y=Score, fill=HogwartsHouse)) +
    geom_boxplot() +
    ylab("HPGWS") +
    scale_fill_manual(values = c("Gryffindor" = color_gryffindor, "Hufflepuff" = color_hufflepuff, "Ravenclaw" = color_ravenclaw, "Slytherin" = color_slytherin)) + 
    ggtitle("Harry Potter Grundwissen Score (HPGWS)") +
    coord_cartesian(ylim = c(0, 15)) +
    guides(fill = guide_legend(title = "Hogwartshaus")) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size=24),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.text=element_text(size=16), 
          legend.title=element_text(size=18),
          legend.position="top",
          legend.margin=margin(40,0,0,0),
          axis.title = element_text(size=18),
          axis.text = element_text(size=16),
          legend.box.margin=margin(10,10,10, 10),
          axis.text.x=element_blank(),
          axis.title.x=element_blank())
  jpeg(file="hogwartshaus_HPGWS.png", width=600, height=600)
  plot(boxplot);
  dev.off();
}
createBoxplotHPGWS();

