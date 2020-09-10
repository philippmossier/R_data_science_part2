# -------------------------------Aufgabe 2.1
# working directory muss sich dort befinden wo auch alle dateien abgespeichert sind

# 1. Laden Sie die Datei "Mathe_Noten.xlsx", welche 
#    die Noten einer Mathematikprüfung enthält, in Ihr Arbeitsverzeichnis. 
#    Öffnen Sie die Datei, machen Sie sich damit vertraut, wie die Daten eingegeben worden sind.
#    Die Note 0 bedeutet, dass die Person bei der Prüfung nicht anwesend war.

datensatz <- read.csv2("./Mathe_Noten.csv",  header = FALSE, sep=",")

# 2. Laden Sie die Daten aus "Mathe_Noten.xlsx" in einen Data Frame.

d <- data.frame(datensatz)
col_headings <- c('note', 'geschlecht')
names(d) <- col_headings
print(head(d))


# 3. Berechnen Sie den Notendurchschnitt und speichern Sie diesen in einer Variablen ab.

durchschnittsnote <- mean(d$note[d$note!=0])
print(durchschnittsnote)

# 4. Erstellen Sie ein Histogramm um die Verteilung der Noten darzustellen. Wählen Sie sinnvolle 
#    Achsenbeschriftungen sowie einen sinnvollen Diagrammtitel. 
#    Speichern Sie das Diagramm als jpeg ab.

# table gibt eine schnelle uebersicht der daten
dTable <- table(d)
print(dTable)

# Erstelle und speichere Histogramm

jpeg(filename="Histogramm.jpg",width=480,height=480)

hist(d$note[d$note != 0],
     main="Notenverteilung" , 
     xlab="Note", 
     ylab="Anzahl", 
     col = "light green")

dev.off()

# 5. Erstellen Sie einen Boxplot, um die Spannweiten von m und w gegeneinander zu vergleichen. 
#    Wählen Sie auch hier sinnvolle Namen für die Achsenbeschriftung sowie den Titel und speichern
#    Sie diesen Boxplot als jpeg ab.

# Erstelle und speichere Boxplot

jpeg(filename="Boxplot.jpg",width=480,height=480)

boxplot(d$note[d$note!=0] ~ d$geschlecht[d$note!=0], 
        xlab="Geschlecht", 
        ylab="Note", 
        main="Boxplot Vergleich Frau & Mann Notendurchscnitt",
        col = "light green")

dev.off()

# ________________________________________ FRAGE __________________________________________________

# Was lässt sich über die Leistungen der Männer verglichen mit derer der Frauen sagen? Versuchen 
# Sie einerseits eine Aussage aufgrund des boxplots zu treffen, andererseits 
# erstellen Sie bitte 2 sogenannte subsets, also Data Frames, einmal eines, 
# welches nur die Noten der Männer enthält, und eines, welches nur die Noten der Frauen enthält, 
# und berechnen jeweils das arithmetische Mittel der Noten.

# ________________________________________ ANTWORT ________________________________________________

## Visualisierung bzw Analyse der Daten (mean, median, spannweite)

wNotenStatsMitNull <- summary(d$note[d$geschlecht == "w"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.500   1.143   2.250   5.000 

mNotenStatsMitNull <- summary(d$note[d$geschlecht == "m"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    1.00    1.28    2.00    5.00 

wNotenStatsOhneNull <- summary(d$note[d$geschlecht == "w" & d$note != 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.500   2.286   3.000   5.000 

mNotenStatsOhneNull <- summary(d$note[d$geschlecht == "m" & d$note != 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.182   3.000   5.000

alleNotenStatsMitNull <- summary(d$note)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.243   2.000   5.000 

alleNotenStatsOhneNull <- summary(d$note[d$note != 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.207   3.000   5.000


# Spannweite ergibt sich aus den min und max Weten ( siehe oben )
# Andere Moeglichkeit die Spannweite zu berechnen:

wSelector <- d$note[d$geschlecht == "w" & d$note != 0]
mSelector <- d$note[d$geschlecht == "m" & d$note != 0]
wSpannweite <- max(wSelector) - min(wSelector)
mSpannweite <- max(mSelector) - min(mSelector)

# dataframe subset nach Geschlecht

wSubset <- subset(d, d$geschlecht == "w")
mSubset <- subset(d, d$geschlecht == "m")
wSubsetOhneNullen <- subset(d, d$geschlecht == "w" & d$note != 0)
mSubsetOhneNullen <- subset(d, d$geschlecht == "m" & d$note != 0)


# Maenner haben mit 2.2182 einen minimal besseren Notendurchscnitt als die Frauen mit 2.286
# Beruecksichtigt man jedoch die Nullen beider Geschlechter dann weisen Frauen mit 1.143 
# einen besseren Notendurchschnitt auf als die Maenner mit 1.28.



