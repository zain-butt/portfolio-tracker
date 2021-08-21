#############################################################################################################################
############################### USE CASE: AUTOMATED DAILY EMAIL WHICH INCLUDES TABLE COMPARING ##############################
###############################  PORTFOLIO PERFORMANCE RELATIVE TO BENCHMARKS AS ATTACHMENT    ##############################
#############################################################################################################################
#############################################################################################################################

## OPTIONAL: Send as email by adding below code to tracker.R after the webshot is saved in your path
## NOTE: CARRIED OVER "name" varibale from tracker.R to KEEP IT FLEXIBLE

# Keeping attachements flexible 

attachmentPath <- paste("C:/FOLDER/",name,".png",sep="") ##REPLACE WITH SAME PATH AS OUTPUT FOR TRACKER
attachmentName <- paste(name,".png",sep="")

# Compose email

email <- compose_email(
  body = md(glue::glue(
    "Please see attached for today's table.")))

create_smtp_creds_key(
  id = "gmail",
  user = "user@gmail.com",              ##REPLACE
  provider = "gmail",                   ##REPLACE
  overwrite = TRUE
)

email_w_attachments <- email %>% 
  add_attachment(file = attachmentPath, filename=attachmentName)

email_w_attachments %>% 
  smtp_send(
    from = "user@gmail.com",            ##REPLACE
    to = "johnsmith@gmail.com",         ##REPLACE 
    subject = "Tracker Table",
    credentials = creds_key("gmail"))
