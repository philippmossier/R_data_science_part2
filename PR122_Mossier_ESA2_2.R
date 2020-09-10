# ------------------- Aufgabe 2.1
# working directory muss sich dort befinden wo auch alle dateien abgespeichert sind


### 1. Lesen Sie die Dateien aus der Datei "kstand.dat"
###    (Tage, an denen bei einer gewissen Anzahl an Beschäftigten der letzte Krankenstand begann) 
###   in einen Data Frame und ermöglichen Sie den direkten Zugriff auf die Variablennamen.

df <- read.table("./kstand.dat", header=T);
print(head(df));



### 2. Zählen Sie mit einem geeigneten Befehl aus, wie häufig jede Kategorie auftritt. 
###    Weiters ermitteln Sie die relativen Häufigkeiten, sowohl aus Dezimalzahl, 
###    als auch in prozentueller Darstellung.

# gibt einen guten Ueberblick ueber die Daten des dataFrame
dt <- table(df)
print(dt)

percent <- function(x, digits = 0, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# rel und abs Haeufigkeit per Wochentag

df_mo <- subset(df, df$WOCHENTAG == "MO")
df_mo_absH <- nrow(df_mo)
df_mo_relH <- df_mo_absH / nrow(df)
df_mo_relH_proz <- percent(df_mo_relH)


df_di <- subset(df, df$WOCHENTAG == "DI")
df_di_absH <- nrow(df_di)
df_di_relH <- df_di_absH / nrow(df)
df_di_relH_proz <- percent(df_di_relH)

df_mi <- subset(df, df$WOCHENTAG == "MI")
df_mi_absH <- nrow(df_mi)
df_mi_relH <- df_mi_absH / nrow(df)
df_mi_relH_proz <- percent(df_mi_relH)


df_do <- subset(df, df$WOCHENTAG == "DO")
df_do_absH <- nrow(df_do)
df_do_relH <- df_do_absH / nrow(df)
df_do_relH_proz <- percent(df_do_relH)


df_fr <- subset(df, df$WOCHENTAG == "FR")
df_fr_absH <- nrow(df_fr)
df_fr_relH <- df_fr_absH / nrow(df)
df_fr_relH_proz <- percent(df_fr_relH)


df_sa <- subset(df, df$WOCHENTAG == "SA")
df_sa_absH <- nrow(df_sa)
df_sa_relH <- df_sa_absH / nrow(df)
df_sa_relH_proz <- percent(df_sa_relH)


df_so <- subset(df, df$WOCHENTAG == "SO")
df_so_absH <- nrow(df_so)
df_so_relH <- df_so_absH / nrow(df)
df_so_relH_proz <- percent(df_so_relH)



### 3. Stellen Sie die Wochentage in der richtigen Reihenfolge dar und erstellen einen entsprechend 
###    "richtig geordneten" Data Frame.

# sort function which takes a datatFrame and a order as input and creates a sorted dataFrame

z <- df$WOCHENTAG;
df_sorted <- data.frame(
  with(
    df,
    df[order(
      z != "MO",
      z != "DI",
      z != "MI",
      z != "DO",
      z != "FR",
      z != "SA",
      z != "SO"
    ),]));
# dataFrame header gets overwritten, so we need to name the header new
names(df_sorted) <- "WOCHENTAG";
print(df_sorted)

# OPTIONAL; only prints first or last 6 entries/rows of the sorted dataframe
print(head(df_sorted));
print(tail(df_sorted));



### 4. Legen Sie Vektoren für die absoluten und relativen Häufigkeiten bzw. die Prozentwerte an. 
###    Die relativen Häufigkeiten sollen auf 2 Nachkommastellen gerundet werden, 
###    die Prozentwerte sollen auf ganze Zahlen gerundet werden.

vectorAbsH <- c(df_mo_absH, 
                df_di_absH, 
                df_mi_absH, 
                df_do_absH, 
                df_fr_absH, 
                df_sa_absH, 
                df_so_absH)

vectorRelH <- c(df_mo_relH, 
                df_di_relH, 
                df_mi_relH, 
                df_do_relH, 
                df_fr_relH,
                df_sa_relH, 
                df_so_relH)

vectorProz <- c(df_mo_relH_proz, 
                df_di_relH_proz, 
                df_mi_relH_proz,
                df_do_relH_proz,
                df_fr_relH_proz,
                df_sa_relH_proz,
                df_so_relH_proz)



### 5. Hängen Sie die 3 Vektoren zusammen und lassen Sie sich die Tabelle ausgeben. 
###    Speichern Sie diese Tabelle in einer Datei ab.

dfZus <- data.frame(Absolute_Haeufigkeit=vectorAbsH, Relative_Haeufigkeit=vectorRelH, Rel_Haeufigkeit_Prozent=vectorProz )

write.csv(dfZus, file = "tabelleVecZusammen.csv", quote = F)



### 6. Erstellen Sie ein Balkendiagramm und ein Kreisdiagramm für die absoluten Häufigkeiten. 
###    Überlegen Sie, wie Sie das Kreisdiagramm so beschriften und darstellen können, dass man mit dem 
###    Diagramm sinnvoll arbeiten kann. Speichern Sie das Kreisdiagramm als Datei ab.

wochentage = c("MO", "DI", "MI", "DO", "FR", "SA", "SO")

barplot(vectorAbsH, 
        names.arg=wochentage,  
        main="Absolute Haeufigkeit der Wochentage"
        )

jpeg(filename="PieChart.jpg",width=480,height=480)

pie(vectorAbsH,
    labels = wochentage,  
    main="Absolute Haeufigkeit der Wochentage"
    )

dev.off()

