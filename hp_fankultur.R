library(readxl)

# Add your working directory (ADJUST)
# setwd("C:/Users/kathr_/OneDrive/Desktop/HCI Master/2.Semester/WAL/Studie/Datenauswertung")

# Retrieve the excel sheet (ADJUST)
exceldata = read_excel("C:\\Users\\kathr_\\OneDrive\\Desktop\\HCI Master\\2.Semester\\WAL\\Studie\\Survey zur Involvierung in die Harry Potter Fankultur (Antworten).xlsx")                                                                            
data = data.frame(exceldata)

TIMER = 25 #minutes the timer was set to by us
MAX_HPFKS = 4 * 14 + 5;

tidyUpDataframe = function(df){
  
  # remove participant names and timer data at the end of the questionnaire
  df = df[-c(1,3, 52:59)];
  
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

  
  # Add score column

  
  likertDf = data.frame(HogwartsHouse, HP_Movies, HP_Books,HP_World, HP_Identification, FreeTV,Read, HP_SortingTest, HP_Podcasts, HP_Drawn, Coldmirror, HP_Talk, HP_MoviePosEmotion, HP_Tattoo, HP_Quotes, HP_BooksRead);
  likertDf$Score = rowSums( likertDf[,2:16] )
  return (likertDf);
}
likertData = createLikertDf(tidyData);

#  TODO: create separate df for merch

#  TODO: create separate df for quiz, convert answers to values (1 and 0), add score column