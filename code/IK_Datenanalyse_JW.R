
# WD ----------------------------------------------------------------------
rm(list = ls(all = T))

path <-
  ("~/Desktop/Uni Bremen/Master/3.Semester/Forschungsprojekt/")

setwd(paste(path, "Datensatz", sep = ""))

load("df_151.Rda")
# Libraries ----------------------------------------------------------------
library(dplyr)
library(table1)
library(flextable)
library(magrittr)
# Datensatz einlesen ------------------------------------------------------
df_jw <- df_cope10
# Tabellen 1 erstellen -------------------------------------------------------
df_jw$PF11_08[df_jw$PF11_08 == "Ausbildung mit Weiterbildung in meinem Fachbereich "] <-
  "" # Sonstige berufliche Ausbildung auflösen
df_jw$AUSBILD[df_jw$AUSBILD == 9] <- 2
df_jw$SCHUL[df_jw$SCHUL == 9] <- 3
df_jw$PF07_08[df_jw$PF07_08 == "Matura mit Fachausbildung"] <- ""
df_jw$SCHUL[df_jw$SCHUL == 1] <- 2
df_jw$FREMDKIND <-
  ifelse(df_jw$PF06_04 == 1 & df_jw$KIND == 0,
         yes = 1,
         no = 0)
  
  df_jw$OECD <- ifelse(df_jw$KIND < df_jw$HAUSANZ, yes = 1.0 + 
                         ((df_jw$HAUSANZ - df_jw$KIND - df_jw$FREMDKIND - 1) * 0.5) + 
                         ((df_jw$KIND + df_jw$FREMDKIND) * 0.3 * as.numeric(df_jw$PF06_04)), no = 1.0)
  
  df_jw$GELD_MET <- as.character(factor(df_jw$GELD, 
                                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 
                                                   9, 10, 11), 
                                        labels = c(225.5, 600.5, 875, 1250, 1750, 
                                                   2250, 2750, 3250, 3750, 
                                                   4500, 5500))) %>% as.numeric()
  df_jw$OECD_GELD <- df_jw$GELD_MET/df_jw$OECD

df_jw <- df_jw %>% 
    mutate(
      SEX = factor(SEX, c("1", "2", "3"), c("männlich", "weiblich¹", "divers")), 
      SCHUL = factor(SCHUL, c("2", "3"),
                     c("Realschulabschluss oder vergleichbare Abschlüsse²", 
                       "Abitur oder vergleichbare Abschlüsse")), 
      AUSBILD = factor(AUSBILD, c("0", "1", "2", "3", "4", "5", "6"),
                       c("Keinen Abschluss bzw. noch in Ausbildung", 
                         "Keinen Berufsabschluss und nicht in Ausbildung",
                         "Lehre, also beruflich-betriebliche Ausbildung", 
                         "Berufliche-schulische Ausbildung", 
                         "Fachschule z.B. Meister-, Technikerschule", 
                         "Fachhochschule", "Universität oder Hochschule")),
      GELD = factor(GELD, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), 
                    c("unter 451€", "451 bis unter 750 €", "750 bis unter 1000€", 
                      "1.000 bis unter 1.500€", "1.500 bis unter 2000€", "2.000 bis unter 2.500€", 
                      "2.500 bis unter 3.000€", "3.000 bis unter 3.500€", "3.500 bis unter 4000€",
                      "4.000 bis unter 5.000€", "5000€ und mehr")),
      FAMST = factor(FAMST, c("1", "3", "4"), c("verheiratet", "geschieden oder verwitwet", 
                                                "ledig")),
      UEBERA = factor(UEBERA, c("1", "2", "3"),
                      c("Nie", "Manchmal", "Regelmäßig")))
  
  
  label(df_jw$AGE) <- "Alter"
  label(df_jw$SEX) <- "Geschlecht"
  label(df_jw$SCHUL) <- "Schulausbildung"
  label(df_jw$AUSBILD) <- "Berufliche Ausbildung"
  label(df_jw$GELD) <- "Nettoeinkommen"
  label(df_jw$HAUSANZ) <- "Anzahl der Haushaltsmitglieder"
  label(df_jw$FAMST) <- "Familienstand"
  label(df_jw$KIND) <- "Anzahl an Kindern"
  label(df_jw$UEBERA) <- "Überstunden (Häufigkeit)³"
  label(df_jw$OECD_GELD) <- "Nettoäquivalenzeinkommen (Neue OECD-Skala)"
  
  caption <- "Tabelle 2. Cluster Charakteristiken"
  footnote <- "¹ inkl. divers (n=1), ² inkl. Hauptschulabschluss (n=1),  ³ Fehlende Werte (n=1)"
  units(df_jw$OECD_GELD) <- "Euro"
  
rndr <- function(x, name, ...) {
    if (!is.numeric(x)) return(render.categorical.default(x))
    what <- switch(name,
                   AGE = c("Mean (SD)", "Median [Min, Max]"), 
                   OECD_GELD = c("Mean (SD)", "Median [Min, Max]"),
                   KIND = "Mean (SD)",
                   HAUSANZ  = "Mean (SD)")
    parse.abbrev.render.code(c("", what))(x)
}
  
Tabelle1 <- table1 (~ AGE + OECD_GELD + SCHUL + AUSBILD + 
                      FAMST + UEBERA + HAUSANZ + KIND | SEX, 
                    data = df_jw, 
                    overall = "Gesamt", 
                    caption = caption,
                    footnote = footnote,
                    render = rndr)


Tabelle1

# k-means Cluster ---------------------------------------------------------
df_cope11 <- df_jw[c("ID", "PCS", "ECS", "VCS")]

# wss.fit2 <- (nrow(df_cope11[,2:4])-1)*sum(apply(df_cope11[,2:4], 2, var))
# for (i in 2:15){
#   wss.fit2[i] <- sum(kmeans(df_cope11[,2:4], centers=i)$withinss)
# } # optional: optimale Anzahl an Clustern (wird bei uns aber inhaltlich gewählt => 3)

# plot(1:15, wss.fit2, type="b", xlab="Anzahl Cluster",
#     ylab ="Fehlerquadratsumme")

# Schritt 3b) k-Means Cluster bilden

set.seed(123)
fit.2 <- kmeans(df_cope11[,2:4], 3, nstart = 25)

df_cope11$cluster <- fit.2$cluster

aggregate(df_cope11[,2:4], by=list(fit.2$cluster), FUN=mean)

#1       1 2.959016 2.135246 1.809426
#2       2 2.896341 2.526423 2.350610
#3       3 2.109694 1.879252 1.790816


summary(as.factor(df_cope11$cluster)) # Anzahl der Personen in den Clustern


# Tabelle 2 erstellen -----------------------------------------------------
df_cope10_147$cluster <- df_cope11$cluster #Clusterzuordnung
df_cope10_147$OECD_GELD <- df_jw$OECD_GELD

df_jw$cluster <- df_cope11$cluster

df_jw <- df_jw %>% mutate(cluster = factor(cluster, c("1","2","3"), c("Problem", "High", "Low")))

Tabelle2 <- table1 (~ AGE + OECD_GELD + SCHUL + AUSBILD + 
                      FAMST + UEBERA + HAUSANZ + KIND | cluster, 
                    data = df_jw, 
                    overall = "Gesamt", 
                    footnote = footnote,
                    render = rndr)

Tabelle2


# Tabelle 3 erstellen -----------------------------------------------------
Tabelle3 <- table1 (~ GKS + WARWICKS | cluster, 
                    data = df_jw, 
                    overall = "Gesamt", 
                    caption = caption)

Tabelle3


# Tabelle 4 (t1) erstellen ------------------------------------------------
df_t1 <- df_jw[which(!is.na(df_jw$WARWICKS_t1)),]

Tabelle4 <- table1 (~ AGE + OECD_GELD + SCHUL + AUSBILD + 
                      FAMST + UEBERA + HAUSANZ + KIND | cluster, 
                    data = df_t1, 
                    overall = "Gesamt",
                    footnote = footnote,
                    render = rndr)

Tabelle4



# Tabelle 5 (t1) erstellen ------------------------------------------------
Tabelle5 <- table1 (~ GKS_t1 + WARWICKS_t1 | cluster, 
                    data = df_jw, 
                    overall = "Gesamt", 
                    caption = caption)

Tabelle5



# Regressionsanalyse ------------------------------------------------------
library(lmtest)
library(sandwich)
library(stargazer)
library(sjPlot)
coeftest(model_t0, vcov = vcovHC(model_t0, type = "HC3"))

  #t0 Modell

df_cope10_147$cluster <- as.factor(df_cope10_147$cluster)

model1_t0 <- lm(WARWICKS ~ GKS*cluster + AGE + as.numeric(OECD_GELD) + 
                   as.numeric(PHYGES) +  as.numeric(SCHLAF) + SEX, 
                 data = df_cope10_147) # adjustiert (t0)

model1_T0 <- lm(WARWICKS ~ GKS*cluster, data = df_cope10_147) # unadjustiert (t0)



  #t1 Modell 

df_cope10_147$GKS_DIFF <- df_cope10_147$GKS_t1 - df_cope10_147$GKS
df_cope10_147$WARWICKS_DIFF <- df_cope10_147$WARWICKS_t1 - df_cope10_147$WARWICKS

model2_t1 <- lm(WARWICKS_DIFF ~ GKS_DIFF*cluster, data = df_cope10_147)

model2_T1 <- lm(WARWICKS_t1 ~ GKS_t1*cluster + WARWICKS + 
                  GKS, data = df_cope10_147)

  #Interaktionsplot darstellen

library(interactions)
interact_plot(model = model_t0_ia, pred = GKS, modx = cluster)


# Sensitivitätsanalysen ---------------------------------------------------
  #Copingstrategien in adaptiv und maladaptiv aufteilen

df_cope10_2 <- df_cope10_147

df_cope10_2 <- df_cope10_2 %>% mutate_at(c(5:32), as.numeric)

df_cope10_2$ADAPT <-(df_cope10_2$BC22 + df_cope10_2$BC27 + df_cope10_2$BC02 +
                       df_cope10_2$BC07 + df_cope10_2$BC14 + df_cope10_2$BC25 +
                       df_cope10_2$BC20 + df_cope10_2$BC24 + df_cope10_2$BC12 + 
                       df_cope10_2$BC17 + df_cope10_2$BC10 + df_cope10_2$BC23 +
                       df_cope10_2$BC05 + df_cope10_2$BC15 + df_cope10_2$BC18 + 
                       df_cope10_2$BC28) / 16

df_cope10_2$MALADAPT <-  ((df_cope10_2$BC01 + df_cope10_2$BC19 + df_cope10_2$BC13 +
                             df_cope10_2$BC26 + df_cope10_2$BC09 + df_cope10_2$BC21 +
                             df_cope10_2$BC06 + df_cope10_2$BC16 + df_cope10_2$BC03 + 
                             df_cope10_2$BC08 + df_cope10_2$BC04 + df_cope10_2$BC11)/ 12)


df_cope10_2$CS <- ifelse(df_cope10_2$ADAPT > df_cope10_2$MALADAPT, yes = 1, no = 2)

df_cope10_2$CS <- as.factor(df_cope10_2$CS)

df_cope10_2$SEX[df_cope10_2$SEX == 3] <- 2 

model_s1 <- lm(WARWICKS ~ GKS*CS + AGE + as.numeric(OECD_GELD) + as.numeric(PHYGES) + 
                      as.numeric(SCHLAF) + SEX, data = df_cope10_2)

model_s2 <- lm(WARWICKS_DIFF ~ GKS_DIFF*CS, data = df_cope10_147)

interact_plot(model = model_t0_ia_2, pred = GKS, modx = CS)

  #Metrische Scores ins Modell einbauen

# model_t0_metr <- lm(WARWICKS ~ GKS +PCS + ECS + VCS + AGE + as.numeric(OECD_GELD) + 
#                      as.numeric(PHYGES) + as.numeric(SCHLAF) + SEX, data = df_cope10_147)

# stargazer(model_t1, type = "html", dep.var.labels = c("Wohlbefinden (Warwicks)"),
#          covariate.labels = c("Konflikte (t¹) (Score)", "Cluster [High]", "Cluster[Low]", 
#                               "Wohlbefinden", "Konflikte (t⁰)", "Konflikte x Cluster [High]",
#                               "Konflikte x Cluster [Low]"), decimal.mark = ".", ci = TRUE, 
#          style = "default", keep.stat = c("n","rsq","adj.rsq", "f"), out = "regression_ad_t1.html",
#          star.cutoffs = NA, single.row = TRUE, digits = 3, ci.separator = " ; ", 
#          notes.label = "Anmerkungen:", notes = "KI = Konfidenzintervall, p < 0.05", 
#          notes.append = F)

# library(modelsummary)
# 
# modelsummary(
#   model_t1,
#   fmt = 3,
#   estimate = "{estimate} [{conf.low}, {conf.high}]",
#   conf_level = 0.95,
#   gof_map = c("nobs", "r.squared"), statistic = NULL,
#   )

tab_model(model_t1, digits = 3, show.p = F,
          pred.labels = c("Konflikte (t¹) (Score)", "Cluster [High]", "Cluster[Low]",
                          "Konflikte (t¹) x Cluster [High]","Konflikte (t¹) x Cluster [Low]"), 
          ci.hyphen = " ; ", dv.labels = c("Wohlbefinden (t¹) (Score)"),
          string.ci = c("95% KI"), string.est = c("B¹"), string.pred = c("A. Variable"),
          show.fstat = T, rm.terms = c("(Intercept)", "WARWICKS", "GKS"), file = "tabmodel_t1.doc")

tab_model(model_t0_ia, digits = 3, show.p = F,
          pred.labels = c("Konflikte (t⁰) (Score)", "Cluster [High]", "Cluster [Low]",
                          "Konflikte (t⁰) (Score) x Cluster [High]","Konflikte (t⁰) (Score) x Cluster [Low]"), 
          ci.hyphen = " ; ", dv.labels = c("Wohlbefinden (t⁰) (Score)"),
          string.ci = c("95% KI"), string.est = c("B¹"), string.pred = c("A. Variable"),
          show.fstat = T, rm.terms = c("(Intercept)", "AGE", "as.numeric(OECD_GELD)", "as.numeric(PHYGES)", 
                                       "as.numeric(SCHLAF)", "SEX2"), file = "tabmodel_t0.doc")

tab_model(model_t1_2, digits = 3, show.p = F,
          pred.labels = c("Konflikte Abs. Differenz (Score)", "Cluster [High]", "Cluster [Low]",
                          "Konflikte Abs. Differenz (t⁰) (Score) x Cluster [High]","Konflikte Abs. Differenz (t⁰) (Score) x Cluster [Low]"), 
          ci.hyphen = " ; ", dv.labels = c("Wohlbefinden Abs. Differenz (Score)"),
          string.ci = c("95% KI"), string.est = c("B¹"), string.pred = c("A. Variable"),
          show.fstat = T, rm.terms = c("(Intercept)"))#, file = "tabmodel_t1_2.doc")

tab_model(model_t0_ia_3, digits = 3, show.p = F,
          pred.labels = c("Konflikte Abs. Differenz (Score)", "Cluster [Adaptiv]",
                          "Konflikte Abs. Differenz (Score) x Cluster [Adaptiv]"), 
          ci.hyphen = " ; ", dv.labels = c("Wohlbefinden Abs. Differenz (Score)"),
          string.ci = c("95% KI"), string.est = c("B¹"), string.pred = c("A. Variable"),
          show.fstat = T, rm.terms = c("(Intercept)"))#, file = "tabmodel_t1_3.doc")


# 3d Plot -----------------------------------------------------------------
library(rgl)

library(magick)

# df_cope10_147$cluster_col <- ifelse(df_cope10_147$cluster == 1, yes = "blue", no = ifelse(df_cope10_147$cluster == 2, yes = "darkorange", no = "black"))
# df <- df_cope10_147
# plot3d(df$PCS, df$ECS, df$VCS, col = df$cluster_col, type = "s", radius = .1/2,
# xlab = "PCS", ylab ="ECS", zlab = "VCS")
# legend3d("topright", legend = c("Problem", "High", "Low"), pch = 16, col = c("blue", "darkorange","black"), cex = 1, inset=c(0.1, 0.2))
# play3d( spin3d( axis = c(0, 1, 0), rpm = 5), duration = 10)
#scatter3d(x=df$VCS, y=df$ECS, z=df$PCS, groups = as.factor(df$cluster), surface =F, grid=F, 
        #  ellipsoid = F, surface.col = c("red", "green", "black"), 
        #  axis.col = c("black", "black","black"), xlab = "Vermeidend (Score)", 
        #  ylab = "Emotionsorientiert (Score)", zlab = "Problemfokussiert (Score)")
# movie3d(
#   movie="3dAnimatedScatterplot5",
#   spin3d( axis = c(0, 1, 0), rpm = 2),
#   duration = 60,
#   dir = "~/Desktop",
#   type = "gif",
#   clean = TRUE
# )
#rgl.snapshot(filename = "3dplotcluster.png")

library(ggplot2)

df <- df %>% mutate(cluster = factor(cluster, c("1","2","3"), c("Problem", "High", "Low")))

ggplot(df, aes(x = cluster, y=GKS, fill = cluster)) + stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(outlier.size = 2)+ scale_fill_manual(values = c("royalblue3", "darkorange", "grey"))+
  xlab("Cluster") + ylab("Gesamtkonfliktscore (t0)") + theme_classic() + 
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=12), axis.text=element_text(size=16), 
        axis.title=element_text(size=14,face="bold")) + geom_jitter(color="black", size=0.4, alpha=0.9) 

plot_model(model_t1, type = "pred", ci.lvl= NA, terms =c("GKS_t1[1, 50, 100]", "cluster"), 
           colors = c("navyblue", "darkorange", "darkgrey"), line.size = 1.5) + 
  theme_classic() + 
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=0), 
                          axis.text=element_text(size=18), 
                          axis.title=element_text(size=18,face="bold")) + 
  xlab("Gesamtkonfliktscore (t1)") + 
  ylab("Wohlbefinden (Score) (t1)")

plot_model(model_t0_ia_3, type = "int", ci.lvl= NA, terms =c("GKS_DIFF", "CS"), 
           colors = c("blue", "red"), line.size = 1.5) + 
  theme_classic() + 
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=0), 
        legend.position = c(.1, .1),
        axis.text=element_text(size=18), 
        axis.title=element_text(size=18,face="bold")) + 
  xlab("Gesamtkonfliktscore (Veränderung)") + 
  ylab("Wohlbefinden (Score) (Veränderung)")

        

boxplot(df$WARWICKS ~ df$cluster, xlab = "Cluster", ylab = "Wohlbefinden", col="lightblue")


df_147 <- df_cope10_147[which(!is.na(df_cope10_147$SEX) & !is.na(df_cope10_147$GELD)),]
model_ua_147 <- lm(WARWICKS ~ GKS*cluster, data = df_147)
tab_model(model_t0_ia, model_ua_147,  auto.label = TRUE, digits = 3, show.p = F,
          pred.labels = c("Konflikte (t⁰) (Score)", "Cluster [High]", "Cluster [Low]",
                          "Konflikte (t⁰) (Score) x Cluster [High]","Konflikte (t⁰) (Score) x Cluster [Low]"), 
          ci.hyphen = " ; ", dv.labels = c("Modell 1", "Modell 2"),
          string.ci = c("95% KI"), string.est = c("B¹"), string.pred = c("A. Variable"),
          rm.terms = c("(Intercept)", "AGE", "as.numeric(OECD_GELD)", "as.numeric(PHYGES)", 
                                      "as.numeric(SCHLAF)", "SEX2"),file = "tabmodel_ua_a2.doc")


# Sensitivitätsanalysen 2.0 (21.02.2023) ----------------------------------
  #Konfliktbelastung anstatt Score

df_t1 <- df_cope10_147[which(df_cope10_147$WARWICKS_t1 != 0),]

v1 <- c(
  "PHAUS_B",
  "PFAM_B",
  "PFREU_B",
  "PBEK_B",
  "AFUR_B",
  "AKOL_B",
  "AUNT_B",
  "ABEZ_B",
  "SONST_B"
)
df_t1[v1] <- lapply(df_t1[v1], as.character)
df_t1[v1] <- lapply(df_t1[v1], as.numeric)
df_t1$GKB <- rowSums(df_t1[, c(
  "PHAUS_B",
  "PFAM_B",
  "PFREU_B",
  "PBEK_B",
  "AFUR_B",
  "AKOL_B",
  "AUNT_B",
  "ABEZ_B",
  "SONST_B"
)], na.rm = T)

v2 <- c(
  "PHAUS_B_t1",
  "PFAM_B_t1",
  "PFREU_B_t1",
  "PBEK_B_t1",
  "AFUR_B_t1",
  "AKOL_B_t1",
  "AUNT_B_t1",
  "ABEZ_B_t1",
  "SONST_B_t1"
)
df_t1[v2] <- lapply(df_t1[v2], as.character)
df_t1[v2] <- lapply(df_t1[v2], as.numeric)
df_t1$GKB_t1 <- rowSums(df_t1[, c("PHAUS_B_t1", "PFAM_B_t1", "PFREU_B_t1",
                                  "PBEK_B_t1", "AFUR_B_t1", "AKOL_B_t1",
                                  "AUNT_B_t1", "ABEZ_B_t1", "SONST_B_t1")], na.rm = T)

df_t1$GKB_DIFF <- df_t1$GKB_t1 - df_t1$GKB

#interact_plot(model = model1_sens, pred = GESAMT_B, modx = cluster)


model_s3 <- lm(WARWICKS_DIFF ~ GKB_DIFF*cluster, data = df_t1) #Längsschnittmodell

  #Warwicks-Verteilung prüfen
ggplot(data = df_cope10_147, aes (x=(as.numeric(WARWICKS)))) + 
  geom_histogram(aes (y = ..density..), color = "1", fill = "grey", bins = 15) + 
  geom_density()

df_t1 <- df_cope10_147[which(!is.na(df_cope10_147$WARWICKS_t1)),]
t.test(df_t1$WARWICKS[which(df_t1$GKS != 0)], df_t1$WARWICKS_t1[which(df_t1$GKS != 0)])
  #t = 0.22102, df = 73.872, p-value = 0.8257 --> nicht signifikant

  #nicht-Streiter ausschließen
df_Kon <- df_cope10_147[which(df_cope10_147$GKS != 0), ]

model2_Kon <- lm(WARWICKS ~ GKS*cluster + AGE + as.numeric(OECD_GELD) + 
                   as.numeric(SCHLAF) + SEX, data = df_Kon)

summary(model2_Kon) #Querschnitt

model3_Kon <- lm(WARWICKS_DIFF ~ GKS_DIFF*cluster, data = df_Kon) #Längsschnitt

summary(model3_Kon)

  #Analyse: Modelle Privat/Beruflich

df_cope10_147$BERUF_K <- rowSums(df_cope10_147[,c("AFUR_KS", "AKOL_KS", 
                                        "AUNT_KS", "ABEZ_KS")], na.rm = T)

df_cope10_147$BERUF_K_t1 <- rowSums(df_cope10_147[,c("AFUR_KS_t1", "AKOL_KS_t1", 
                                                  "AUNT_KS_t1", "ABEZ_KS_t1")], na.rm = T)

df_cope10_147$BERUF_K_DIFF <- df_cope10_147$BERUF_K_t1 - df_cope10_147$BERUF_K

df_cope10_147$PRIVAT_K <- rowSums(df_cope10_147[,c("PHAUS_KS", "PFAM_KS", "PFREU_KS", 
                                         "PBEK_KS")], na.rm = T)

df_cope10_147$PRIVAT_K_t1 <- rowSums(df_cope10_147[,c("AFUR_KS_t1", "AKOL_KS", 
                                                     "AUNT_KS", "ABEZ_KS")], na.rm = T)

df_cope10_147$PRIVAT_K_DIFF <- df_cope10_147$PRIVAT_K_t1 - df_cope10_147$PRIVAT_K

model_beruf <- lm(WARWICKS_DIFF ~ BERUF_K_DIFF*cluster, data = df_cope10_147)

model_privat <- lm(WARWICKS_DIFF ~ PRIVAT_K_DIFF*cluster, data = df_cope10_147)

  #t-Test für Coping (--> Copen Streiter mehr als nicht-Streiter? Nein!)
t.test(df_cope10_147$PCS[df_cope10_147$GKS == 0], 
       df_cope10_147$PCS[df_cope10_147$GKS != 0])
  #p-value = 0.6992

t.test(df_cope10_147$ECS[df_cope10_147$GKS == 0], 
       df_cope10_147$ECS[df_cope10_147$GKS != 0])
  #p-value = 0.34

t.test(df_cope10_147$VCS[df_cope10_147$GKS == 0], 
       df_cope10_147$VCS[df_cope10_147$GKS != 0])
  #p-value = 0.5276

#Analyse ohne korrigierten Personencode

df_pcdf_kor <- df_cope10_147[which(df_cope10_147$PC_KOR != 1 & !is.na(df_cope10_147$WARWICKS_DIFF)),]
model4_PC_KOR <- lm(WARWICKS_DIFF ~ GKS_DIFF*cluster, 
                    data = df_pckor) 

summary(model4_PC_KOR)


# Weitere Deskription (25.02.2023) ----------------------------------------

  #Korrelationstabelle mit Hauptvariablen
library(apaTables)
library(apa)

subset_cor <- subset(df_cope10_147, select = c(GKS, WARWICKS, cluster, PCS, ECS, VCS))
subset_cor$cluster <- as.character(subset_cor$cluster) %>% as.numeric(subset_cor$cluster)
cor(subset_cor)

apa.cor.table(subset_cor, filename = "jw.doc",
              show.sig.stars = FALSE, cor.method = "spearman")

subset_ttest <- subset(df_cope10_147, cluster == 1 | cluster == 2)
subset_ttest2 <- subset(df_cope10_147, cluster == 1 | cluster == 3)
subset_ttest3 <- subset(df_cope10_147, cluster == 2 | cluster == 3)
lm_anova <- lm(WARWICKS ~ cluster, df_cope10_147)
apa.aov.table(lm_anova, filename = "jw_anova.doc")


