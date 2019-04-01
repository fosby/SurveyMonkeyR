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
  surveyDetailsDF = as.data.frame(responseJSON)
  surveyDetailsUnnested = surveyDetailsDF %>% select(pages.questions) %>% unnest()
  headingData = parseHeadingDetails(surveyDetailsUnnested)
  questionData = parseQuestionDetails(surveyDetailsUnnested)
  otherData = parseOtherDetails(surveyDetailsUnnested)
  questionData = left_join(questionData,otherData)
  #keep function or pull up code from that function??
  detailTable = buildDetailTable(headingData,questionData)
  return(detailTable)
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
  df = df %>% select(headings,id) %>% unnest() %>% mutate(SurveyID = surveyID)
  df = df %>% rename("QuestionID" = id)
  return(df)
}

#parses through nested Question column found in Detail view of survey
parseQuestionDetails = function(df){
  df = df %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
  df = df %>% rename("QuestionID" = id,"AnswerID" = id1)
  return(df)

}

parseOtherDetails = function(df){
  #opened ended questions or 'other' which opens up a text entry option for the user.  This doesn't fall under
  #normail columns.  They have their own.  This pulls them into a normal table.
  tempDF = data.frame("QuestionID"=character(),"OtherID"=character(),"OtherText"=character())
  for(i in 1:dim(df)[1]){
    if(!is.na(df$answers.other.id[i])){
      #surveyDetailsQuestionsUnnested = add_row(surveyDetailsQuestionsUnnested,QuestionID = surveyDetailsUnnested$id[i], AnswerID = surveyDetailsUnnested$answers.other.id[i], text = surveyDetailsUnnested$answers.other.text[i])
      tempDF = add_row(tempDF,QuestionID = df$id[i], OtherID = df$answers.other.id[i], OtherText = df$answers.other.text[i]) 
    }
  }
  return(tempDF)
}

buildDetailTable = function(df1,df2){
  detailTable = left_join(df1,df2) %>% select(SurveyID,QuestionID,heading,AnswerID,text,OtherID,OtherText) %>%
    rename("Heading" = heading, "AnswerText" = text)
  return(detailTable)
}


# ########################################################
# #working test area
# surveyDetailsUnnested = surveyDetailsDF %>% select(pages.questions) %>% unnest()
# surveyDetailsQuestionsUnnested = surveyDetailsUnnested %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
surveyResponsesUnnested = surveyResponsesDF %>% select(data.pages) %>% unnest()
surveyResponsesUnnested = surveyResponsesUnnested %>% select(questions) %>% unnest()
# ########################################################

#below will be used to actually run the code
# surveyDetailsDF = data.frame()
# surveyResponsesDF = data.frame()
detailTable = getSurveyDetails(url,surveyID)
#getSurveyResponses(url,surveyID)
