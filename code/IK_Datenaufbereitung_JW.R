
# WD ----------------------------------------------------------------------
rm(list=ls(all=T))

path <- ("") 

setwd(paste(path,"Datensatz", sep=""))

# optional: Datensatz einlesen
# df_jw15 <- read.table("IK_15_12_JW.txt", sep = "\t", dec = ",", header = TRUE)

# Datensatz einlesen-------------------------------------------------------
df <- read.csv("IKroh3011.csv", 
               header = TRUE, 
               sep = ";", 
               dec =".")

df [df == -9] <- NA # Sicherstellen, dass jede -9 wirklich als NA kodiert ist

# Personen filtern die Filterfrage mit "ja" beantwortet haben und mindestens bis Seite 5 (Schlaf und Physis) gekommen sind
df <- df[(df$FI01 == 1) & (!is.na(df$FI01)) & (df$MAXPAGE >=5),]


# zu löschende Variablen entfernen (in Excel rot markiert) -------------------------------------------------------
delete <- names(df) %in% c("MODE", "SERIAL", "STARTED", "GE02_CN", 
                           "PF09", "TIME001", "TIME002", "TIME003", "TIME004", "TIME005", 
                           "TIME006", "TIME007", "TIME008", "TIME009", 
                           "TIME010", "TIME011", "TIME012", "TIME013", 
                           "TIME014", "TIME015", "TIME016", "TIME017", 
                           "TIME018", "TIME019", "TIME020", "TIME021", 
                           "TIME022", "TIME023", "TIME024", "TIME025", 
                           "TIME026", "TIME027", "TIME028", "TIME029", 
                           "TIME030", "TIME031", "TIME_SUM", "MAILSENT", 
                           "LASTDATA", "Q_VIEWER", "LASTPAGE", "MAXPAGE", 
                           "MISSREL", "TIME_RSI", "DEG_TIME")

df <- df[!delete]


# Variablen umbenennen (evtl. abweichend vom Protokoll aber effizienter)------------------------------------------------------
library(dplyr)

df2 <- df %>% rename("ID" = "CASE",
                      "BC01" = "BC01_01",
                      "BC02" = "BC01_02",
                      "BC03" = "BC01_03",
                      "BC04" = "BC01_04",
                      "BC05" = "BC01_05",
                      "BC06" = "BC01_06",
                      "BC07" = "BC01_07",
                      "BC08" = "BC01_08",
                      "BC09" = "BC01_09",
                      "BC10" = "BC01_10",
                      "BC11" = "BC01_11",
                      "BC12" = "BC01_12",
                      "BC13" = "BC01_13",
                      "BC14" = "BC01_14",
                      "BC15" = "BC01_15",
                      "BC16" = "BC01_16",
                      "BC17" = "BC01_17",
                      "BC18" = "BC01_18",
                      "BC19" = "BC01_19",
                      "BC20" = "BC01_20",
                      "BC21" = "BC01_21",
                      "BC22" = "BC01_22",
                      "BC23" = "BC01_23",
                      "BC24" = "BC01_24",
                      "BC25" = "BC01_25",
                      "BC26" = "BC01_26",
                      "BC27" = "BC01_27",
                      "BC28" = "BC01_28",
                      "VAZ" = "BD09",
                      "REALAZ" = "BD02",
                      "ATAG" = "BD08",
                      "SCHICHTA" = "BD03_01",
                      "NACHTA" = "BD03_02",
                      "SAMSTAGA" = "BD03_03",
                      "SONNTAGA" = "BD03_04",
                      "UEBERA" = "BD03_05",
                      "KLIMA" = "BD04",
                      "FILTER10" = "FI01",
                      "PHYGES" = "GE01",
                      "PSYE" = "GE02x06",
                      "SCHLAF" = "GE03",
                      "PHAUS" = "KO01",
                      "PFAM" = "KO02",
                      "PFREU" = "KO03",
                      "PBEK" = "KO04",
                      "AFUR" = "KO05",
                      "AKOL" = "KO06",
                      "AUNT" = "KO07",
                      "ABEZ" = "KO08",
                      "SONST" = "KO09",
                      "SONST_JA" = "KO09_02",
                      "PHAUS_B" = "KO14",
                      "PFAM_B" = "KO15",
                      "PFREU_B" = "KO16",
                      "PBEK_B" = "KO17",
                      "AFUR_B" = "KO18",
                      "AKOL_B" = "KO19",
                      "AUNT_B" = "KO20",
                      "ABEZ_B" = "KO21",
                      "SONST_B" = "KO22",
                      "PHAUS_H" = "KO23",
                      "PFAM_H" = "KO24",
                      "PFREU_H" = "KO25",
                      "PBEK_H" = "KO26",
                      "AFUR_H" = "KO27",
                      "AKOL_H" = "KO28",
                      "AUNT_H" = "KO29",
                      "ABEZ_H" = "KO30",
                      "SONST_H" = "KO31",
                      "SEX" = "PF01",
                      "AGE" = "PF02_01",
                      "FAMST" = "PF03",
                      "SCHUL" = "PF07",
                      "AUSBILD" = "PF11",
                      "GELD" = "PF08",
                      "HAUSANZ" = "PF10_01",
                      "PW01" = "PW01_01",
                      "PW02" = "PW01_02",
                      "PW03" = "PW01_03",
                      "PW04" = "PW01_04",
                      "PW05" = "PW01_05",
                      "PW06" = "PW01_06",
                      "PW07" = "PW01_07")


# Kodierungen bearbeiten --------------------------------------------------
df2$VAZ <- factor(df2$VAZ, 
                  levels = c(1, 2, 6, 3, 7, 4, 9, 10, 5, 8, -9), 
                  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, NA))

df2$REALAZ <- factor(df2$REALAZ, 
                     levels = c(1, 2, 6, 3, 7, 4, 5, 8, 9, -9),
                     labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, NA))

df2$ATAG <- factor(df2$ATAG, 
                   levels = c(1, 7, 2, 3, 4, 5, 6, -9),
                   labels = c(1, 2, 3, 4, 5, 6, 7, NA))

#df2$Filter10 <- factor(df2$Filter10, levels = c(1, 2),
                       #labels = c(1, 0))

df2[,c("PSYE", "GO02_01")] <- lapply(df2[,c("PSYE", "GO02_01")], 
                                     FUN = factor, 
                                     levels = c(1, 2),
                                     labels = c(0,1))

df2[,c("PHAUS", "PFAM", "PFREU", "PBEK", "AFUR", "AKOL", "AUNT", "ABEZ")] <- lapply(df2[,c("PHAUS", "PFAM", "PFREU", "PBEK", "AFUR", "AKOL", "AUNT", "ABEZ")], 
                                                                                    FUN = factor, 
                                                                                    levels =c(1, 2), 
                                                                                    labels = c(1,0))
df2$SONST <- factor(df2$SONST, 
                    levels = c(2, 3), 
                    labels = c(1, 0))
df2$SONST_H <- factor(df2$SONST_H, 
                      levels = c(1, 2, 3, 5, NA), 
                      labels = c(1, 2, 3, 4))


df2[,c("PF04", "PF06_01", "PF06_02", "PF06_03", "PF06_04", "PF06_05", "PF06_06")] <- lapply(df[,c("PF04", "PF06_01", "PF06_02", "PF06_03", "PF06_04", "PF06_05", "PF06_06")], 
                                                                                            FUN = factor, 
                                                                                            levels = c(1, 2), 
                                                                                            labels = c(0,1))

df2$SCHUL <- factor(df2$SCHUL, 
                    levels = c(1, 2, 3, 9, 8, -9), 
                    labels = c(0, 1, 2, 3, 9, NA))
df2$AUSBILD <- factor(df2$AUSBILD, 
                      levels = c(1, 10, 5, 11, 12, 13, 14, 8, -9), 
                      labels = c(0, 1, 2, 3, 4, 5, 6, 9, NA))
  
df2$FAMST <- factor(df2$FAMST, 
                    levels = c(1, 2, 3, 5, NA), 
                    labels = c(1, 2, 3, 4))


# Neue Variablen erstellen ------------------------------------------------
  # Copingscore
  df2$PCS <- (df2$BC02 + df2$BC07 + df2$BC10 + 
                df2$BC12 + df2$BC14 + df2$BC17 + 
                df2$BC23 + df2$BC25)/8
  
  df2$ECS <- (df2$BC05 + df2$BC09 + df2$BC13 + 
                df2$BC15 + df2$BC18 + df2$BC20 + 
                df2$BC21 + df2$BC22 + df2$BC24 + 
                df2$BC26 + df2$BC27 + df2$BC28)/12
  
  df2$VCS <- (df2$BC01 + df2$BC03 + df2$BC04 + 
                df2$BC06 + df2$BC08 + df2$BC11 + 
                df2$BC16 + df2$BC19)/8

  # Konfliktscore
  df2$PHAUS_KS <- df2$PHAUS_B*df2$PHAUS_H
  df2$PFAM_KS <- df2$PFAM_B*df2$PFAM_H
  df2$PFREU_KS <- df2$PFREU_B*df2$PFREU_H
  df2$PBEK_KS <- df2$PBEK_B*df2$PBEK_H
  df2$AFUR_KS <- df2$AFUR_B*df2$AFUR_H
  df2$AKOL_KS <- df2$AKOL_B*df2$AKOL_H
  df2$AUNT_KS <- df2$AUNT_B*df2$AUNT_H
  df2$ABEZ_KS <- df2$ABEZ_B*df2$ABEZ_H
  df2$SONST_KS <- df2$SONST_B*as.integer(df2$SONST_H)
  df2$GKS <- rowSums(df2[,c("PHAUS_KS", "PFAM_KS", "PFREU_KS", 
                            "PBEK_KS", "AFUR_KS", "AKOL_KS", 
                            "AUNT_KS", "ABEZ_KS", "SONST_KS")], na.rm = T)

  # Personencode
  df2$PersCode <- paste(df2$IC01_01, df2$IC02_01, df2$IC03_01, 
                        df2$IC04_01, df2$IC05_01, df2$IC07_01, sep = "")
  df2$PersCode <- na_if(df2$PersCode, "NA") # Alle NAs in Missings umwandeln

  # Anzahl an Kindern
  df2$KIND <- ifelse(df2$PF04 == 1, yes = df2$PF04_02, 
                    no = (ifelse(df2$PF04 == 0, yes = 0, no = NA)))

  # Wohlbefindenscore
  df2$WARWICKS <- rowSums(df2[ ,c("PW01", "PW02", "PW03", 
                                 "PW04", "PW05", "PW06", "PW07")], na.rm = T)
  
  df2$WARWICKS[df2$WARWICKS == 0] <- NA # 0 geht nicht -> also NA setzen
  
  # OECD-Aquivalenzeinkommen
  df2$OECD <- 1.0 + ((df2$HAUSANZ - df2$KIND - 1) * 0.5) + (df2$KIND * 0.3)

  # transformierter Warwick 
 
  df2$WARWICKS[df2$WARWICKS == 7] <- 7.00
  df2$WARWICKS[df2$WARWICKS == 8] <- 9.51 
  df2$WARWICKS[df2$WARWICKS == 9] <- 11.25 
  df2$WARWICKS[df2$WARWICKS == 10] <- 12.40 
  df2$WARWICKS[df2$WARWICKS == 11] <- 13.33
  df2$WARWICKS[df2$WARWICKS == 12] <- 14.08 
  df2$WARWICKS[df2$WARWICKS == 13] <- 14.75
  df2$WARWICKS[df2$WARWICKS == 14] <- 15.32
  df2$WARWICKS[df2$WARWICKS == 15] <- 15.84
  df2$WARWICKS[df2$WARWICKS == 16] <- 16.36 
  df2$WARWICKS[df2$WARWICKS == 17] <- 16.88 
  df2$WARWICKS[df2$WARWICKS == 18] <- 17.43 
  df2$WARWICKS[df2$WARWICKS == 19] <- 17.98 
  df2$WARWICKS[df2$WARWICKS == 20] <- 18.59
  df2$WARWICKS[df2$WARWICKS == 21] <- 19.25
  df2$WARWICKS[df2$WARWICKS == 22] <- 19.98
  df2$WARWICKS[df2$WARWICKS == 23] <- 20.73
  df2$WARWICKS[df2$WARWICKS == 24] <- 21.54 
  df2$WARWICKS[df2$WARWICKS == 25] <- 22.35
  df2$WARWICKS[df2$WARWICKS == 26] <- 23.21
  df2$WARWICKS[df2$WARWICKS == 27] <- 24.11
  df2$WARWICKS[df2$WARWICKS == 28] <- 25.03
  df2$WARWICKS[df2$WARWICKS == 29] <- 26.02
  df2$WARWICKS[df2$WARWICKS == 30] <- 27.03
  df2$WARWICKS[df2$WARWICKS == 31] <- 28.13 
  df2$WARWICKS[df2$WARWICKS == 32] <- 29.31
  df2$WARWICKS[df2$WARWICKS == 33] <- 30.70
  df2$WARWICKS[df2$WARWICKS == 34] <- 32.55 

  

# t0 und t1 trennen -------------------------------------------------------
  # nach QUESTNNR (Fragebogennummer) trennen
df_t0 <- subset(df2, QUESTNNR == "Interpersonelle_Konflikte")
df_t1 <- subset(df2, QUESTNNR == "Interpersonelle_Konflikte_T1")
colnames(df_t1) <- paste(colnames(df_t1), "_t1", sep ="") # "_t1" bei allen Variablen vom Befragungszeitpunkt t1 anhängen

  # Variablen entfernen die in t1 nicht enthalten sind
df_t1 <- df_t1[,colSums(is.na(df_t1))<nrow(df_t1)] # -> Somit auch alle Variablen wo ausschließlich Missings enthalten sind entfernen

drop <- c("QUESTNNR_t1", "ID_t1", "FILTER10_t1", 
          "IC01_01_t1", "IC02_01_t1", "IC03_01_t1", 
          "IC04_01_t1", "IC05_01_t1", "IC07_01_t1", 
          "PF07_08_t1", "PF11_08_t1") 

  #restliche doppelte Variablen entfernen
df_t1 <- df_t1[,!(names(df_t1) %in% drop)] 



# Zeitpunkte zusammenfügen ------------------------------------------------
  # Personencode für das Zusammenfügen in Großbuchstaben umwandeln 
df_t0$PersCode <- toupper(df_t0$PersCode)
df_t1$PersCode_t1 <- toupper(df_t1$PersCode_t1)
PersCode_f <- subset(df_t1, !(PersCode_t1 %in% df_t0$PersCode)) #Datensatz mit fehlerhaften Personencodes -> Was machen wir mit den?
  # (für später) sort(df_t0$PersCode) # Datensatz des ersten Zeitpunktes alphabetisch sortieren um besser abzugleichen


# Schritt 9) Datensätze anhand Personencode zusammenführen
df_t0 <- df_t0[order(df_t0$PersCode),] #sortieren
df_t1 <- df_t1[order(df_t1$PersCode_t1),] #sortieren
df_t1 <- rename(df_t1, PersCode = PersCode_t1) #Variablennamen müssen für das zusammenführen übereinstimmen

# Personencodes korrigieren (Annahme: Tippfehler) und Indikatorvariable für Korrektur
df_t1$PersCode[df_t1$PersCode == "HIGEB14"] <- "HIGEH14"
df_t1$PersCode[df_t1$PersCode == "IRGEW3"] <- "IRGRW3"
df_t1$PersCode[df_t1$PersCode == "PEPAS27"] <- "PEPAD27"
df_t1$PersCode[df_t1$PersCode == "PETHS14"] <- "PETHS13"


df_merge <- merge(df_t0, df_t1, by = "PersCode", all.x = T) #zusammenführen

df_merge$PC_KOR <- as.factor(ifelse(df_merge$PersCode == "HIGEH14" | df_merge$PersCode== "IRGRW3"|
                         df_merge$PersCode == "PEPAD27" | df_merge$PersCode == "PETHS13", yes = 1, no = 0))

# Personencodes entfernen -------------------------------------------------
  # Personencodes und Antworten zu Personencodes aus t0 entfernen

drop2 <- c("IC01_01", "IC02_01", 
           "IC03_01", "IC04_01", "IC05_01", "IC07_01", "PersCode")

df_merge <- df_merge[,!(names(df_merge) %in% drop2)]


# Letzte Anpassungen (u.a. Variablentypen ---------------------------------

  # nominal und metrische Variablentypen anpassen 
chrVar <- c("ID", "QUESTNNR", "PF07_08", "PF11_08", "SONST_JA", "SONST_JA_t1")
numVar <- c("FINISHED", "MISSING", "PCS", "ECS", "VCS", "PHAUS_KS", 
            "PFAM_KS", "PFREU_KS", "PBEK_KS", "AFUR_KS", "AKOL_KS", 
            "AUNT_KS", "ABEZ_KS", "SONST_KS", "GKS", "AGE", "KIND", 
            "HAUSANZ", "WARWICKS", "FINISHED_t1", "MISSING_t1", "PHAUS_KS_t1", 
            "PFAM_KS_t1", "PFREU_KS_t1", "PBEK_KS_t1", 
            "AFUR_KS_t1", "AKOL_KS_t1", "AUNT_KS_t1", "ABEZ_KS_t1", 
            "SONST_KS_t1", "GKS_t1", "WARWICKS_t1")


df_merge2 <- df_merge

df_merge2[chrVar] <- lapply(df_merge2[chrVar], as.character)
df_merge2[numVar] <- lapply(df_merge2[numVar], as.numeric)

  # alle restlichen Variablen die nicht nominal oder metrisch sind als ordinal transformieren

df_merge2[!names(df_merge2) %in% c(numVar, chrVar)] <- lapply(df_merge2[!names(df_merge2) %in% c(numVar, chrVar)], 
                                                              as.factor)


# Datensatz in Dataframe transformieren ------------------------------------
df_jw <- as.data.frame(df_merge2)


# Datensatz als CSV exportieren (optional) --------------------------------
# write.csv2(df_jw, "df_jw.csv", row.names = FALSE)


# Abgleich mit Lorenz (SAS) -----------------------------------------------
# df_ld2 <- read.table("df_ld_1312.txt", sep = "\t", header = T, dec = ".")
# df_ld2 <- read.csv("InterKon13_12_22_LD.csv", sep = ",", header = T, dec = ".")
 

# SoSci Fehler - Datensatz filtern nach Personen die Coping ausgefüllt haben --------
library(tidyverse)

df_cope10 <- df_jw %>%
  drop_na(c("PCS", "ECS", "VCS"))


# Datensatz in R Format für Analyse speichern -----------------------------


save(df_cope10, file = "df_151.RDa") 
