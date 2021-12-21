
library(mailR) #needs Java 64bit (https://www.java.com/en/download/manual.jsp) and rJava library

Attachment="\\\\lena/CCAMLR/Science/Projects/CCAML_R/CCAML_R.png"
MailTo=c('henrique.anatole@ccamlr.org')

send.mail(from=MailFrom,
          to=MailTo,
          subject='R Script Done!',
          body="This is a test. It's working!Thank you Ian!",
          attach.files = Attachment,
          smtp=list(host.name = "Mail.ccamlr.org", port = 25),
          send=T)


