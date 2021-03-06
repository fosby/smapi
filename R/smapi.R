library(tidyverse)
library(httr)
library(jsonlite)


smResponse = function(surveyID,startDate = (Sys.Date()-1),accessToken){
  url = "https://api.surveymonkey.com/v3/surveys"
  if(tolower(startDate) == "all"){
    print("Retrieving every record...")
    urlResponses = paste(url, surveyID, "responses/bulk", sep = "/")
  } else if (!is.na(as.Date(startDate,format = "%Y-%m-%d"))){
    urlResponses = paste(url, surveyID, paste("responses/bulk?start_created_at=",startDate,sep = ""), sep = "/")
  } else{
    stop("Please check your date argument.  It should follow the format of YYYY-MM-DD.")
    return(NULL)
  }

  getSurveyDetails = function(url,surveyID){
    response = GET(
      paste(url, surveyID, "details", sep = "/"),
      add_headers(
        "Authorization" = paste("Bearer ", accessToken, sep = ""),
        "Content-Type" = "application/json"
      )
    )
    if(response$status_code == 200){
      responseText = content(response, "text",encoding = "UTF-8")
      responseJSON = fromJSON(responseText,flatten = TRUE)
      surveyDetailsDF = as.data.frame(responseJSON$pages)
      surveyDetailsUnnested = surveyDetailsDF %>% select(questions) %>% unnest()
      headingData = parseHeadingDetails(surveyDetailsUnnested)
      questionData = parseQuestionDetails(surveyDetailsUnnested)
      otherData = parseOtherDetails(surveyDetailsUnnested)
      questionData = left_join(questionData,otherData,by="QuestionID")
      #keep function or pull up code from that function??
      detailTable = buildDetailTable(headingData,questionData)
      detailTable$SurveyID = as.character(detailTable$SurveyID)
      return(detailTable)
    } else if (response$status_code == 404){
      stop("HTTP 404.  Appears surveyID is incorrect or does not exist.  Please review.")
    } else if (response$status_code == 401){
      stop("HTTP 401.  Appears accessToken is incorrect.  Please review.")
    } else{
      stop("Something is amiss.  Please double check input arguments you passed in.  Bad internet??? :)")
    }
  }

  getSurveyResponses = function(url,surveyID,headers){
    response = GET(
      url,
      add_headers(
        "Authorization" = paste("Bearer ", accessToken, sep = ""),
        "Content-Type" = "application/json"
      )
    )
    responseText = content(response, "text",encoding = "UTF-8")
    responseJSON = fromJSON(responseText,flatten = TRUE)
    if(responseJSON$total == 0){
      stop("No surveys during this time frame to return!")
      return(NULL)
    }else{
      surveyResponsesDF = as.data.frame(responseJSON)
      tryCatch({
        if(is.null(responseJSON$links$`next`)){
          #if(`next` %in% paste(unlist(responseJSON$links))){
          tempResponseData = parseSurveyResponses(surveyResponsesDF)
          responseData <<- rbind(responseData,tempResponseData)
          return(responseData)
        } else {
          tempResponseData = parseSurveyResponses(surveyResponsesDF)
          responseData <<- rbind(responseData,tempResponseData)
          if(tolower(startDate) == "all"){
            getSurveyResponses(responseJSON$links$`next`,surveyID)
          } else{
            getSurveyResponses(paste(responseJSON$links$`next`,"&start_created_at=",startDate,sep=''),surveyID)
          }

        }},
        error = function(e){
          tempResponseData = parseSurveyResponses(surveyResponsesDF)
          responseData <<- rbind(responseData,tempResponseData)
          return(responseData)
        })
    }}
  #parses through nested Heading column found in Detail view of survey
  parseHeadingDetails = function(df){
    df = df %>% select(headings,id) %>% unnest() %>% mutate(SurveyID = surveyID)
    df = df %>% rename("QuestionID" = id)
    return(df)
  }

  #parses through nested Question column found in Detail view of survey
  parseQuestionDetails = function(df){
    df = df %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
    df = df %>% rename("QuestionID" = id,"ChoiceID" = id1)
    return(df)
  }

  parseOtherDetails = function(df){
    #opened ended questions or 'other' which opens up a text entry option for the user.  This doesn't fall under
    #normail columns.  They have their own.  This pulls them into a normal table.
    tempDF = data.frame("QuestionID"=character(),"OtherID"=character(),"OtherText"=character())
    tryCatch({
      for(i in 1:dim(df)[1]){
        if(!is.na(df$answers.other.id[i])){
          #surveyDetailsQuestionsUnnested = add_row(surveyDetailsQuestionsUnnested,QuestionID = surveyDetailsUnnested$id[i], AnswerID = surveyDetailsUnnested$answers.other.id[i], text = surveyDetailsUnnested$answers.other.text[i])
          tempDF = add_row(tempDF,QuestionID = df$id[i], OtherID = df$answers.other.id[i], OtherText = df$answers.other.text[i])
        }
      }
      tempDF$QuestionID = as.character(tempDF$QuestionID)
      return(tempDF)
    },
    error = function(e){
      return(tempDF)
    })
  }

  buildDetailTable = function(df1,df2){
    detailTable = left_join(df1,df2,by="QuestionID") %>% select(SurveyID,QuestionID,heading,ChoiceID,text,OtherID,OtherText) %>%
      rename("QuestionText" = heading, "ChoiceText" = text)
    return(detailTable)
  }

  parseSurveyResponses = function(df){
    surveyResponsesUnnested = df %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,data.pages) %>% unnest()
    surveyResponsesUnnested = surveyResponsesUnnested %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,questions) %>% unnest() %>% rename("QuestionID" = id)
    surveyResponsesUnnested = surveyResponsesUnnested %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,QuestionID,answers) %>% unnest()



    #surveyResponsesUnnested = df %>% select(total_time,ip_address,id,collector_id,date_created,date_modified,survey_id,pages) %>% unnest()
    #surveyResponsesUnnested = surveyResponsesUnnested %>% select(total_time,ip_address,id,collector_id,date_created,date_modified,survey_id,questions) %>% unnest() %>% rename("QuestionID" = id)
    #surveyResponsesUnnested = surveyResponsesUnnested %>% select(total_time,ip_address,id,collector_id,date_created,date_modified,survey_id,QuestionID,answers) %>% unnest()
    return(surveyResponsesUnnested)
  }

  buildFinalTable = function(df1,df2){
    if("TRUE" %in% grepl("other_id",names(responseTable))){
      finalTable = left_join(df1,df2, by = c("data.survey_id" = "SurveyID","QuestionID" = "QuestionID","choice_id" = "ChoiceID"))
      distinctView = distinct(finalTable, data.id,data.survey_id,QuestionID,other_id,choice_id,text) %>% filter(is.na(choice_id))
      finalTable$OtherID = as.character(finalTable$OtherID)
      finalTable = left_join(finalTable,distinctView, by = c("data.id","data.survey_id" = "data.survey_id","QuestionID" = "QuestionID","OtherID" = "other_id"))
      finalTable = finalTable %>%
        select(data.survey_id,data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,QuestionID,QuestionText,choice_id.x,ChoiceText,OtherID,OtherText,text.y,other_id) %>%
        subset(is.na(other_id)) %>%
        rename("SurveyID" = data.survey_id,"TotalTime" = data.total_time,"IpAddress" = data.ip_address, "RespondentID" = data.id, "CollectorID" = data.collector_id, "StartDate" = data.date_created, "EndDate" = data.date_modified,
               "ChoiceID" = choice_id.x, "OtherResponse" = text.y) %>%
        select(-other_id)
      return(finalTable)
    } else{
      finalTable = left_join(df1,df2, by = c("data.survey_id" = "SurveyID","QuestionID" = "QuestionID","choice_id" = "ChoiceID"))
      distinctView = distinct(finalTable, data.id,data.survey_id,QuestionID,choice_id,text) %>% filter(is.na(choice_id))
      finalTable = left_join(finalTable,distinctView, by = c("data.id","data.survey_id" = "data.survey_id","QuestionID" = "QuestionID"))
      finalTable = finalTable %>%
        select(data.survey_id,data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,QuestionID,QuestionText,choice_id.x,ChoiceText,text.y) %>%
        rename("SurveyID" = data.survey_id,"TotalTime" = data.total_time,"IpAddress" = data.ip_address, "RespondentID" = data.id, "CollectorID" = data.collector_id, "StartDate" = data.date_created, "EndDate" = data.date_modified,
               "ChoiceID" = choice_id.x,"OtherResponse" = text.y)
      return(finalTable)
    }
  }


  # ########################################################
  # #working test area
  # surveyDetailsUnnested = surveyDetailsDF %>% select(pages.questions) %>% unnest()
  # surveyDetailsQuestionsUnnested = surveyDetailsUnnested %>% select(answers.choices,id) %>% filter(answers.choices != "NULL") %>% unnest()
  #surveyResponsesUnnested = surveyResponsesDF %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,data.pages) %>% unnest()
  #surveyResponsesUnnested = surveyResponsesUnnested %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,questions) %>% unnest() %>% rename("QuestionID" = id)
  #surveyResponsesUnnested = surveyResponsesUnnested %>% select(data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,data.survey_id,QuestionID,answers) %>% unnest()
  # finalTable = left_join(responseTable,detailTable, by = c("data.survey_id" = "SurveyID","QuestionID" = "QuestionID","choice_id" = "ChoiceID"))
  # distinctView = distinct(finalTable, data.id,data.survey_id,QuestionID,other_id,choice_id,text) %>% filter(is.na(choice_id))
  # finalTable = left_join(finalTable,distinctView, by = c("data.id","data.survey_id" = "data.survey_id","QuestionID" = "QuestionID","OtherID" = "other_id"))
  # finalTable = finalTable %>%
  #   select(data.survey_id,data.total_time,data.ip_address,data.id,data.collector_id,data.date_created,data.date_modified,QuestionID,QuestionText,choice_id.x,ChoiceText,OtherID,OtherText,text.y,other_id) %>%
  #   subset(is.na(other_id)) %>%
  #   rename("SurveyID" = data.survey_id,"TotalTime" = data.total_time,"IpAddress" = data.ip_address, "RespondentID" = data.id, "CollectorID" = data.collector_id, "StartDate" = data.date_created, "EndDate" = data.date_modified,
  #          "ChoiceID" = choice_id.x, "OtherResponse" = text.y) %>%
  #   select(-other_id)
  #!is.na(as.Date(startDate,format = "%Y-%m-%d"))
  # ########################################################

  #below will be used to actually run the code
  # surveyDetailsDF = data.frame()
  # surveyResponsesDF = data.frame()
  responseData = data.frame()
  detailTable = getSurveyDetails(url,surveyID)
  responseTable = getSurveyResponses(urlResponses,surveyID)
  #no surveys stop function
  if(is.null(responseTable)){
    return()
  }
  finalTable = buildFinalTable(responseTable,detailTable)
  rm(detailTable,responseTable)
  return(finalTable)
}
