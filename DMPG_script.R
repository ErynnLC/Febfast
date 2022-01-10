# Decision-Making Profile Generation
# 10.01.2022

# Load packages
library(readxl)
library(rmarkdown)
library(fmsb)
library(pdftools)

# Import data
d <- read_excel("./DMPG_data.xlsx")

# File prefix variable
prefix <- "P_"

# Advanced legend labels
advlegend_participant <- "You"
advlegend_sample <- "Fellow febfast Participants"

# Radar labels
radarlabels <- c("Persistence",
                 "Tolerance of        \nUncertainty        ",
                 "Risk Taking       ",
                 "Thoughtfulness",
                 "Reward Driven",
                 "            Emotion Driven\n            Positive",
                 "     Emotion Driven\n     Negative")

# Radar colours
radargrid_colour <- rgb(0.41, 0.41, 0.41, 0.7)
participant_colour <- rgb(0, 0.6, 0.51)
participant_colourinner <- rgb(0, 0.6, 0.51, 0.5)
sample_colour <- rgb(0.98, 0.68, 0.26, 0.8)
sample_colourinner <- rgb(0.98, 0.68, 0.26, 0.4)

# Class conversions and averages
d$SUPPS_Persev <- as.double(d$SUPPS_Persev)
d$TolUncert_Score <- as.double(d$TolUncert_Score)
d$RiskQuestion <- as.double(d$RiskQuestion)
d$SUPPS_Premed <- as.double(d$SUPPS_Premed)
d$BAS_D_Score <- as.double(d$BAS_D_Score)
d$SUPPS_PU <- as.double(d$SUPPS_PU)
d$SUPPS_NU <- as.double(d$SUPPS_NU)
davg <- c(mean(d$SUPPS_Persev), mean(d$TolUncert_Score),
          mean(d$RiskQuestion), mean(d$SUPPS_Premed),
          mean(d$BAS_D_Score), mean(d$SUPPS_PU),
          mean(d$SUPPS_NU))

# Column names for radar
dlist <- c("SUPPS_Persev", "TolUncert_Score",
           "RiskQuestion", "SUPPS_Premed",
           "BAS_D_Score", "SUPPS_PU", "SUPPS_NU")

# Main for loop to generate reports
for (participant in unique(d$ParticipantID)) {
  # Extract one row (participant)
  extract <- d[d$ParticipantID == participant,]
  
  # Prepare for fmsb function
  dradar <- extract[,dlist]
  dradar <- rbind(rep(16,7), rep(4,7), dradar)
  dradar[1,2] <- 60
  dradar[2,2] <- 12
  dradar[1,3] <- 10
  dradar[2,3] <- 0
  colnames(dradar) <- c("Persistence",
                        "Tolerance of Uncertainty",
                        "Risk Taking", "Thoughtfulness",
                        "Reward Driven",
                        "Emotion Driven Positive",
                        "Emotion Driven Negative")
  
  # Advanced profile check
  if (extract$Advanced == 1) {
    
    # Prepare for fmsb function (advanced)
    dradar <- rbind(dradar[1:2,], davg, dradar[3,])
    
  }
  
  # Build report from rmd template
  render("./Reports/DMPG_template.Rmd",
         output_file = 
           paste0(prefix, participant, ".pdf"))
  
  # Merge with second page (new folder)
  pdf_combine(c(paste0("./reports/", prefix, participant,
                       ".pdf"), "./reports/infopage.pdf"),
              output = 
                paste0("./combinedreports/", prefix,
                       participant, ".pdf"))
}
