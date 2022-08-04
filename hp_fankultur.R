library(readxl)


setwd("C:/Users/kathr_/OneDrive/Desktop/HCI Master/2.Semester/WAL/Studie/Datenauswertung")
excelPath = ("C:/Users/kathr_/OneDrive/Desktop/HCI Master/2.Semester/WAL/Studie/Datenauswertung")

exceldata = read_excel("C:\\Users\\kathr_\\OneDrive\\Desktop\\HCI Master\\2.Semester\\WAL\\Studie\\Datenauswertung\\Survey zur Involvierung in die Harry Potter Fankultur (Antworten).xlsx")                                                                            
data = data.frame(exceldata)

tidyUpDataframe = function(df){
  
  # Replace Prefects with Hogwarts houses
  df[2][df[2] == "Gemma Farley"] <- "Slytherin"
  df[2][df[2] == "Percy Weasly"] <- "Gryffindor"
  df[2][df[2] == "Gabriel Truman"] <- "Hufflepuff"
  df[2][df[2] == "Robert Hilliard"] <- "Ravenclaw"
  
  # Replace Column names with more readable tags
  colnames(df) <- c("Date", "HogwartsHouse", "HP_Movies", "HP_Movies", "HP_Books", "HP_Books", "HP_World", "HP_World", 
                    "HP_Identification", "HP_Identification", "FreeTV", "FreeTV", "Read", "Read", "HP_SortingTest", "HP_SortingTest", 
                    "HP_Podcasts", "HP_Podcasts", "HP_Drawn", "HP_Drawn", "Coldmirror", "Coldmirror", "HP_Talk", "HP_Talk", 
                    "HP_MoviePosEmotion", "HP_MoviePosEMotion", "HP_Tattoo", "HP_Tattoo", "HP_Quotes", "HP_Quotes", "HP_BooksRead", 
                    "HP_BookCount", "HP_MovieCount", "HP_Merch", "HP_MerchCount", "HP_MerchHouse", 
                    "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15");

  return (df); 
}

tidyData = tidyUpDataframe(data);

#  TODO: create separate df for likert scale and convert answers to values, add score column

#  TODO: create separate df for merch

#  TODO: create separate df for quiz, convert answers to values (1 and 0), add score column