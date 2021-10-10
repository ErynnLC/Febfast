#install.packages('tinytex')
#tinytex:install_tinytex()

#libraries
library(readxl)
library(ggplot2)
library(rmarkdown)
library(gridExtra)

#import
d <- read_excel("C:/Users/David/Desktop/test/ffmock.xlsx")
dsub <- head(d)

#reporting
#under the assumption there is no duplicate ID
for (participant in unique(dsub$ParticipantID)) {
  #take one participant
  extract <- dsub[dsub$ParticipantID == participant,]
  
  #visualise
  d_graph1 <- extract[,c(16,17,18,19,20)]
  td_graph1 <- as.data.frame(t(d_graph1))
  td_graph1$V2 <- colnames(d_graph1)
  graph1 <- ggplot(td_graph1,aes(x=V2,y=V1)) + geom_bar(stat = "identity")
  
  graph2 <- graph1 + coord_flip()
  
  #generate report
  render("C:/Users/David/Desktop/test/reports/ffreportingprototype_template.Rmd",
           output_file = 
              paste0("FEBFAST_", participant, ".pdf"))
}