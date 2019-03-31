library(tidyverse)
library(httr)
library(jsonlite)

accessToken = as.character(read_csv("AccessToken.csv",col_names = FALSE))
surveyID = as.character(read_csv("SurveyID.csv",col_names = FALSE))
url = "https://api.surveymonkey.com/v3/surveys"

getSurveyDetails = function(url,surveyID){
  response = GET(
    paste(url, surveyID, "details", sep = "/"),
    add_headers(
      "Authorization" = paste("Bearer ", accessToken, sep = ""),
      "Content-Type" = "application/json"
    )
  )
  responseText = content(response, "text")
  responseJSON = fromJSON(responseText,flatten = TRUE)
  surveyDetailsDF <<- as.data.frame(responseJSON)
  surveyDetailsUnnested = surveyDetailsDF %>% select(pages.questions) %>% unnest()
  headingData = parseHeadingDetails(surveyDetailsUnnested)
  questionData = parseQuestionDetails(surveyDetailsUnnested)
  
  #keep function or pull up code from that function??
  buildDetailTable(headingData,questionData)
}

getSurveyResponses = function(url,surveyID){
  response = GET(
    paste(url, surveyID, "responses/bulk", sep = "/"),
    add_headers(
      "Authorization" = paste("Bearer ", accessToken, sep = ""),
      "Content-Type" = "application/json"
    )
  )
  responseText = content(response, "text")
  responseJSON = fromJSON(responseText,flatten = TRUE)
  surveyResponsesDF <<- as.data.frame(responseJSON)
}

#parses through nested Heading column found in Detail view of survey
parseHeadingDetails = function(df){
  surveyDetailsHeadingUnnested = surveyDetailsUnnested %>% select(headings,id) %>% unnest() %>% mutate(SurveyID = surveyID)
  surveyDetailsHeadingUnnested = surveyDetailsHeadingUnnested %>% rename("QuestionID" = id)
  return(surveyDetailsHeadingUnnested)
}

#parses through nested Question column found in Detail view of survey
parseQuestionDetails = function(df){
  surveyDetailsQuestionsUnnested = surveyDetailsUnnested %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
  surveyDetailsQuestionsUnnested = surveyDetailsQuestionsUnnested %>% rename("QuestionID" = id,"AnswerID" = id1)
  
  #opened ended questions or 'other' which opens up a text entry option for the user.  This doesn't fall under
  #normail columns.  They have their own.  This pulls them into a normal table.
  for(i in 1:dim(surveyDetailsUnnested)[1]){
    if(!is.na(surveyDetailsUnnested$answers.other.id[i])){
      surveyDetailsQuestionsUnnested = add_row(surveyDetailsQuestionsUnnested,QuestionID = surveyDetailsUnnested$id[i], AnswerID = surveyDetailsUnnested$answers.other.id[i], text = surveyDetailsUnnested$answers.other.text[i])
    }
  }
  return(surveyDetailsQuestionsUnnested)
}

buildDetailTable = function(df1,df2){
  detailTable = left_join(surveyDetailsHeadingUnnested,surveyDetailsQuestionsUnnested) %>% select(SurveyID,QuestionID,heading,AnswerID,text) %>%
    rename("Heading" = heading, "Text" = text)
}


########################################################
#working test area
surveyDetailsUnnested = surveyDetailsDF %>% select(pages.questions) %>% unnest()
surveyDetailsQuestionsUnnested = surveyDetailsUnnested %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
########################################################

#below will be used to actually run the code
surveyDetailsDF = data.frame()
surveyResponsesDF = data.frame()
getSurveyDetails(url,surveyID)
getSurveyResponses(url,surveyID)
