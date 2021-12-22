
library(mailR) #needs Java 64bit (https://www.java.com/en/download/manual.jsp) and rJava library

#A code to run the markdown file and generate the pdf


MailFrom = 'henrique.anatole@ccamlr.org'
Attachment="C:/R_projects/fmc_daily_updates/saved_reports/FMC_daily_updates.pdf"
MailTo=c('henrique.anatole@ccamlr.org',
         'claire.vanwerven@ccamlr.org')

send.mail(from=MailFrom,
          to=MailTo,
          subject='R Script Done!',
          body="This is a test. It's working!Thank you Ian!",
          attach.files = Attachment,
          smtp=list(host.name = "Mail.ccamlr.org", port = 25),
          send=T)



