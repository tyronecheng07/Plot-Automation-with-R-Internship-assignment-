##############################################

# Script of Automation for plotting Fremdueberwachung's data
# Author: Kuan Yew Cheng
# Date created: 24.08.2020

##############################################

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(akvoColR)

################### Beschreibung ###########################

# Diese Funktion zeichnet die Kurven mit allen relevanten Informationen.
# Durch rmplot werden jeweils eine Kurve nur mit 1 Parameter gegen Datum erstellt
# sowie eine gesamte Darstellung der allen Kurven für die entsprechende Parameter
# in einer Seite erzeugt.
# Wichig: Die Bearbeitung der Daten verfolgt die Excel-Datei (Fremdueberwachung_vorbereitet.xlsx)
# Bitte nehmen diese Datei als Vorlage

#################### Hinweise ###############################

# rmplot #
# Variable selbst definieren:
# 1. mypath: Der Zielpfad für die Excel-Datei
# 2. mysheet: Das Zielblatt im Excel-Datei
# 3. probe_quelle: Benennung der Bildname nach Stoffkonzentration_" ? ", z.B. aus Rohwasser oder Reinwasser

#################### Funktion ###############################

rmplot <- function(mypath, mysheet, probe_quelle){

  # load excel data for sheet and reformat the dates
  my_df <- readxl::read_excel(paste0(mypath),
                              sheet = mysheet,
                              col_types = c(
                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                              )
  ) %>% mutate(
    DATUM = janitor::excel_numeric_to_date(DATUM)
  )

  # prepare the lists of value for the loop function
  column_list <- list("SUMME_PAK", "SUMME_BTEX", "KOHLENWASSERSTOFFINDEX", "CYANIDE_GESAMT", "PHENOLINDEX")
  name_list <- list("Summe PAK", "Summe BTEX", "Kohlenwasserstoffindex","Cyanide gesamt", "Phenolindex")
  grenze_list <- list(0.01, 1, 100, 0.005, 10)

  # loop for plotting a graph of a parameter against date
  # column_list: values of our header that we can find in the excel file
  # name_list: values that rename our y-axis
  # value_list: values that used in creating horizontal line (Bestimmungsgrenze)
  for (i in column_list){
    for(j in name_list){
      for(k in value_list){
        if (match(i, column_list) == match(j ,name_list) && match(j ,name_list) == match(k, grenze_list) && match(i, grenze_list) == match(k, value_list)){
          sp <- ggplot(my_df, aes_string(x=my_df$DATUM, y= i), na.omit = TRUE)+
            geom_line(size = 1, color=akvo_cols()[sample(1:9, 1)], na.omit=TRUE)+
            geom_point(size = 2, shape=19, color=akvo_cols()[sample(1:9, 1)])+
            xlab("Datum")+
            ylab (paste(j, expression("[µg/l]"), sep=" "))+
            theme(text = element_text(size = 12, family = "Open Sans"))+
            theme_bw()

          # add a horizontal line
          sp + geom_hline(aes(yintercept=k, linetype = "Bestimmungsgrenze"), color= 'blue') +
            scale_linetype_manual(name = NULL, values = c(2, 2),
                                  guide = guide_legend(override.aes = list(color = c("blue")))) +
            theme(legend.title = element_text(size = 5),
                  legend.text  = element_text(size = 5))


          # save png file
          ggsave(file=paste0(wd,"./", j, "_test.png"),
                 device = "png", scale = 1.33:1, dpi=300)
        } else {
          print("error")
        }
      }
    }
  }

  ################## # laying multiple plots in a page #####################

  # convertion of unit from miligram to microgram
  my_df_2 <- my_df %>%
    mutate(CYANIDE_GESAMT_ug = CYANIDE_GESAMT * 1000)

  # rename columns and remove unwanted column
  colnames(my_df_2)[colnames(my_df_2) %in% c("SUMME_PAK", "SUMME_BTEX", "KOHLENWASSERSTOFFINDEX","CYANIDE_GESAMT_ug",  "PHENOLINDEX")] <- c(
    "Summe PAK", "Summe BTEX", "Kohlenwasserstoffindex", "Phenolindex", "Cyanide gesamt")

  # transform the table from wide to long
  my_df_3 <- my_df_2[c(-5)] %>% gather(key = Parameter, value = value, -DATUM)

  # plot data
  ggplot(data = my_df_3, aes(x=DATUM, y= value, color=Parameter))+
    geom_line(size = 1.5)+
    geom_point(size = 3, shape=19)+
    xlab("Datum")+
    ylab (expression("Stoffkonzentrationen [µg/l]"))+
    akvoColR::scale_color_akvo() +
    theme_bw()+
    facet_wrap(Parameter~.)

  # save png file
  ggsave(file=paste0(wd,"./","Stoffkonzentrationen_", probe_quelle, ".png"),
         device = "png", height=10, width=10,  dpi=300, limitsize = FALSE, units="in")
}

############# Beispiel #######################

# setting working directory
setwd("C:/Users/Praktikant/Desktop/Praktikant Cheng/Projekt/SD_Rummelsburg_Suite")
wd <- getwd()

# Variables
mypath <- "C:/Users/Praktikant/Desktop/Praktikant Cheng/Projekt/SD_Rummelsburg_Suite/Input_Data/Daten_verarbeitet/Fremdueberwachung_vorbereitet.xlsx"
mysheet <- "GWRA_Brunnen_Roh"
probe_quelle <- "Brunnen"

rmplot(mypath, mysheet, probe_quelle)

#############################################