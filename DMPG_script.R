# Decision-Making Profile Generation
# 21.01.2022

# Load packages
library(readxl)
library(rmarkdown)
library(fmsb)
library(pdftools)

# Import study data and norming data
d <- read_excel("./DMPG_data.xlsx")
dn <- read_excel("./DMPG_norm.xlsx")

# File prefix variable
prefix <- "P_"

# Advanced legend labels
advlegend_participant <- "You"
advlegend_sample <- "Fellow febfast participants"

# Radar labels
radarlabels <- c("Persistence",
                 "Tolerance of        \nUncertainty        ",
                 "Risk-Taking       ",
                 "Planning",
                 "       Reward Drive",
                 "            Emotion Driven\n            Impulsivity")

# Radar colours
radargrid_colour <- rgb(0.41, 0.41, 0.41, 0.7)
participant_colour <- rgb(0, 0.6, 0.51)
participant_colourinner <- rgb(0, 0.6, 0.51, 0.5)
sample_colour <- rgb(0.98, 0.68, 0.26, 0.8)
sample_colourinner <- rgb(0.98, 0.68, 0.26, 0.4)

# Center labels
radarcenterlabels <- c("0", "", "50\n", "", "100\n")

# Class conversions
d$SUPPS_Persev <- as.double(d$SUPPS_Persev)
d$TolUncert_Score <- as.double(d$TolUncert_Score)
d$RiskQuestion <- as.double(d$RiskQuestion)
d$SUPPS_Premed <- as.double(d$SUPPS_Premed)
d$BAS_D_Score <- as.double(d$BAS_D_Score)
d$SUPPS_PU <- as.double(d$SUPPS_PU)
d$SUPPS_NU <- as.double(d$SUPPS_NU)
dn$SUPPS_Persev <- as.double(dn$SUPPS_Persev)
dn$TolUncert_Score <- as.double(dn$TolUncert_Score)
dn$RiskQuestion <- as.double(dn$RiskQuestion)
dn$SUPPS_Premed <- as.double(dn$SUPPS_Premed)
dn$BAS_D_Score <- as.double(dn$BAS_D_Score)
dn$SUPPS_PU <- as.double(dn$SUPPS_PU)
dn$SUPPS_NU <- as.double(dn$SUPPS_NU)

# EDI variable
d$SUPPS_EDI <- with(d, (SUPPS_PU + SUPPS_NU)/2)
dn$SUPPS_EDI <- with(dn, (SUPPS_PU + SUPPS_NU)/2)

# Sample averages
davg <- data.frame(mean(d$SUPPS_Persev), mean(d$TolUncert_Score),
          mean(d$RiskQuestion), mean(d$SUPPS_Premed),
          mean(d$BAS_D_Score), mean(d$SUPPS_EDI))

colnames(davg) <- c("SUPPS_Persev",
                    "TolUncert_Score",
                    "RiskQuestion",
                    "SUPPS_Premed",
                    "BAS_D_Score",
                    "SUPPS_EDI")

s_SUPPS_Persev <- ((sum(davg$SUPPS_Persev > dn$SUPPS_Persev))/nrow(dn)) * 100
s_TolUncert_Score <- ((sum(davg$TolUncert_Score > dn$TolUncert_Score))/nrow(dn)) * 100
s_RiskQuestion <- ((sum(davg$RiskQuestion > dn$RiskQuestion))/nrow(dn)) * 100
s_SUPPS_Premed <- ((sum(davg$SUPPS_Premed > dn$SUPPS_Premed))/nrow(dn)) * 100
s_BAS_D_Score <- ((sum(davg$BAS_D_Score > dn$BAS_D_Score))/nrow(dn)) * 100
s_SUPPS_EDI <- ((sum(davg$SUPPS_EDI > dn$SUPPS_EDI))/nrow(dn)) * 100

davg <- c(s_SUPPS_Persev, s_TolUncert_Score,
          s_RiskQuestion, s_SUPPS_Premed,
          s_BAS_D_Score, s_SUPPS_EDI)

# Main for loop to generate reports
for (participant in unique(d$ParticipantID)) {
  
  # Extract one row (participant)
  extract <- d[d$ParticipantID == participant,]
  
  # Subset norming data based on participant demo
  if (extract$Gender == "Man") {
    dn_sub <- dn[dn$Gender == "Man",]
    if (extract$Age >= 18 & extract$Age <= 34) {
      dn_sub <- dn_sub[dn_sub$Age >= 18 & dn_sub$Age <= 34,]
    } else if (extract$Age >= 35 & extract$Age <= 64) {
      dn_sub <- dn_sub[dn_sub$Age >= 35 & dn_sub$Age <= 64,]
    }
  } else if (extract$Gender == "Woman") {
    dn_sub <- dn[dn$Gender == "Woman",]
    if (extract$Age >= 18 & extract$Age <= 34) {
      dn_sub <- dn_sub[dn_sub$Age >= 18 & dn_sub$Age <= 34,]
    } else if (extract$Age >= 35 & extract$Age <= 64) {
      dn_sub <- dn_sub[dn_sub$Age >= 35 & dn_sub$Age <= 64,]
    }
  } else {
    if (extract$Age >= 18 & extract$Age <= 34) {
      dn_sub <- dn[dn$Age >= 18 & dn$Age <= 34,]
    } else if (extract$Age >= 35 & extract$Age <= 64) {
      dn_sub <- dn[dn$Age >= 35 & dn$Age <= 64,]
    }
  }
  
  # Computer percentages based on subset norming data
  p_SUPPS_Persev <- ((sum(extract$SUPPS_Persev > dn_sub$SUPPS_Persev))/nrow(dn_sub)) * 100
  p_TolUncert_Score <- ((sum(extract$TolUncert_Score >= dn_sub$TolUncert_Score))/nrow(dn_sub)) * 100
  p_RiskQuestion <- ((sum(extract$RiskQuestion > dn_sub$RiskQuestion))/nrow(dn_sub)) * 100
  p_SUPPS_Premed <- ((sum(extract$SUPPS_Premed > dn_sub$SUPPS_Premed))/nrow(dn_sub)) * 100
  p_BAS_D_Score <- ((sum(extract$BAS_D_Score > dn_sub$BAS_D_Score))/nrow(dn_sub)) * 100
  p_SUPPS_EDI <- ((sum(extract$SUPPS_EDI > dn_sub$SUPPS_EDI))/nrow(dn_sub)) * 100

  # Prepare for fmsb function
  dradar <- data.frame(p_SUPPS_Persev, p_TolUncert_Score,
                       p_RiskQuestion, p_SUPPS_Premed,
                       p_BAS_D_Score, p_SUPPS_EDI)
  dradar <- rbind(rep(100,7), rep(0,7), dradar)
  colnames(dradar) <- c("Persistence",
                        "Tolerance of Uncertainty",
                        "Risk-Taking",
                        "Planning",
                        "Reward Drive",
                        "Emotion Driven Impulsivity")
  
  # Advanced profile check
  if (extract$Advanced == 1) {
    
    # Prepare for fmsb function (advanced)
    dradar <- rbind(dradar[1:2,], davg, dradar[3,])
  }
  
  # Build report from rmd template
  render("./reports/DMPG_template.Rmd",
         output_file = 
           paste0(prefix, participant, ".pdf"))
  
  # Merge with second page (new folder)
  pdf_combine(c(paste0("./reports/", prefix, participant,
                       ".pdf"), "./reports/infopage.pdf"),
              output = 
                paste0("./combinedreports/", prefix,
                       participant, ".pdf"))
}
