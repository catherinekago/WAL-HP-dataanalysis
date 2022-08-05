library(readxl)

# Add your working directory (ADJUST)
# setwd("C:/Users/kathr_/OneDrive/Desktop/HCI Master/2.Semester/WAL/Studie/Datenauswertung")

# Retrieve the excel sheet (ADJUST)
exceldata = read_excel("C:\\Users\\kathr_\\OneDrive\\Desktop\\HCI Master\\2.Semester\\WAL\\Studie\\Survey zur Involvierung in die Harry Potter Fankultur (Antworten).xlsx")                                                                            
data = data.frame(exceldata)

tidyUpDataframe = function(df){
  
  # Replace Prefects with Hogwarts houses
  df[2][df[2] == "Gemma Farley"] <- "Slytherin"
  df[2][df[2] == "Percy Weasly"] <- "Gryffindor"
  df[2][df[2] == "Gabriel Truman"] <- "Hufflepuff"
  df[2][df[2] == "Robert Hilliard"] <- "Ravenclaw"
  
  # Replace Column names with more readable tags
  colnames(df) <- c("Date", "HogwartsHouse", "HP_Movies_+", "HP_Movies_-", "HP_Books_+", "HP_Books_-", "HP_World_+", "HP_World_-", 
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
  likertDf <- data.frame(lapply(likertDf, function(x) {gsub("Stimme weder zu noch zu", 0, x) })) # TODO exchange with Weder noch
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
  
  # Add score column
  likertDf$Score = rowSums(likertDf[,c(-1)])
  
  
  colnames(likertDf) = c("HogwartsHouse", "HP_Movies_+", "HP_Movies_-", "HP_Books_+", "HP_Books_-", "HP_World_+", "HP_World_-", 
                         "HP_Identification_+", "HP_Identification_-", "FreeTV_+", "FreeTV_-", "Read_+", "Read_-", "HP_SortingTest_+", "HP_SortingTest_-", "HP_Podcasts_+", "HP_Podcasts_-", "HP_Drawn_+", "HP_Drawn_-", "Coldmirror_+", "Coldmirror_-", "HP_Talk_+", "HP_Talk_-", 
                         "HP_MoviePosEmotion_+", "HP_MoviePosEMotion_-", "HP_Tattoo_+", "HP_Tattoo_-", "HP_Quotes_+", "HP_Quotes_-", "HP_BooksRead", "Score");
  return (likertDf);
  
}
likertData = createLikertDf(tidyData);

#  TODO: create separate df for merch

#  TODO: create separate df for quiz, convert answers to values (1 and 0), add score column