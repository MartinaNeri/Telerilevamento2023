1. Installazione e caricamento dei pacchetti necessari
Questa parte installa e importa pacchetti utili per la manipolazione dei dati raster, la visualizzazione grafica e le analisi statistiche:

R
Copia codice
install.packages("raster")    # Per lavorare con immagini raster
install.packages("ggplot2")   # Per creare grafici
install.packages("patchwork") # Per combinare i grafici
install.packages("viridis")   # Palette di colori adatta per daltonici
install.packages("RStoolbox") # Contiene la funzione rasterPCA

library(raster)
library(ggplot2)
library(patchwork)
library(viridis)
library(RStoolbox)

Motivo: Caricare pacchetti è essenziale per accedere a funzioni specializzate necessarie per lavorare con immagini satellitari e condurre analisi spaziali.
Utilità: Pacchetti come raster facilitano la manipolazione delle immagini geospaziali, ggplot2 consente una rappresentazione visiva chiara dei dati,
e RStoolbox contiene strumenti per l'analisi avanzata (come la PCA) che rendono possibile studiare la variazione di dati multidimensionali in modo più approfondito.

###2. Impostazione della working directory
Definisce la directory in cui si trovano le immagini:

R
Copia codice
setwd("C:/lab/Esame_telerilevamento")
Descrizione: Definire la "working directory" permette di stabilire il percorso dei file, 
ossia il luogo in cui si trovano e dove verranno salvati i risultati.
Motivo: Una directory ben definita facilita il caricamento e il salvataggio dei file, assicurando una corretta gestione dei dati.
Rilevanza ecologica: Organizzare i file dati in modo sistematico è fondamentale in un'analisi scientifica 
poiché rende riproducibile l’analisi nel tempo, migliorando la gestione a lungo termine delle risorse ecologiche monitorate.

####3. Caricamento delle immagini satellitari per il 2017 e il 2024
Creazione della lista dei file: Trova i file relativi all’anno specificato.

R
Copia codice
rlist_2017 <- list.files(pattern = "2017-09-01")
rlist_2024 <- list.files(pattern = "2024-08-25")
Importazione dei file come oggetti raster: Li importa come immagini raster.

R
Copia codice
import_2017 <- lapply(rlist_2017, raster)
import_2024 <- lapply(rlist_2024, raster)
Combinazione delle bande in un'unica immagine: stack() crea un unico oggetto con tutte le bande.

R
Copia codice
img_2017 <- stack(import_2017)
img_2024 <- stack(import_2024)
Visualizzazione delle immagini:

R
Copia codice
plot(img_2017)
plot(img_2024)
Descrizione: Le immagini satellitari relative al 2017 e al 2024 vengono importate nel software. Le bande principali includono la banda rossa e l’infrarosso vicino (NIR), che sono particolarmente utili per il monitoraggio della vegetazione.
Motivo: Le immagini di anni diversi consentono di effettuare confronti temporali che sono essenziali per individuare cambiamenti nella copertura vegetale e in altre caratteristiche ambientali.
Rilevanza ecologica: Il confronto temporale delle immagini aiuta a monitorare i cambiamenti nella copertura vegetale, offrendo indicazioni importanti su fenomeni come deforestazione, desertificazione o degrado delle risorse naturali.


####4. Generazione delle immagini in PDF per confronto visivo
plotRGB() visualizza le immagini in colori compositi utilizzando diverse combinazioni di bande, simulando una vista "naturale" o una vista in "falsi colori".

R
Copia codice
pdf("img_2017.pdf")
par(mfrow = c(1, 2))
plotRGB(img_2017, 3, 2, 1, stretch = "lin") # Visualizzazione RGB
plotRGB(img_2017, 4, 3, 2, stretch = "lin") # Falsi colori
dev.off()
Descrizione: Questa fase produce composizioni visive in formato PDF, sia in colori veri (RGB naturale) sia a falsi colori, dove l’infrarosso vicino (NIR) sostituisce il canale rosso.
Motivo: Le immagini RGB permettono di avere una rappresentazione visiva naturale, mentre le immagini a falsi colori (infrarosso) mettono in risalto la vegetazione, essenziale per la valutazione della salute delle piante.
Rilevanza ecologica: L’infrarosso riflette maggiormente la vegetazione attiva, per cui le immagini a falsi colori sono utili per rilevare le aree di vegetazione e per verificare eventuali zone degradate, supportando interventi mirati di conservazione.


####5. Calcolo degli indici di vegetazione
####5.1 DVI (Differenza Vegetazione)
Calcola la differenza tra la banda dell’infrarosso vicino (NIR) e la banda rossa per identificare la densità di vegetazione:

R
Copia codice
DVI_2017 <- img_2017[[4]] - img_2017[[3]]
DVI_2024 <- img_2024[[4]] - img_2024[[3]]
DVI_fin <- DVI_2024 - DVI_2017
Scopo: Il DVI serve a evidenziare la presenza e la salute della vegetazione, poiché l’infrarosso vicino viene riflesso dalla vegetazione viva.
Descrizione: Il DVI viene calcolato sottraendo i valori della banda rossa (RED) dalla banda dell’infrarosso vicino (NIR).
Motivo: Questo indice sfrutta la differente risposta della vegetazione tra queste due bande per stimare la densità di copertura vegetale.
Rilevanza ecologica: Il DVI è un indicatore diretto della presenza di vegetazione. Un DVI elevato corrisponde a una vegetazione densa, mentre valori bassi possono indicare la presenza di suoli nudi o di vegetazione scarsa, aiutando a identificare zone in cui la vegetazione potrebbe essere compromessa.

####5.2 NDVI (Normalized Difference Vegetation Index)
Calcola l’NDVI, un indice comunemente usato per quantificare la copertura vegetale e la salute della vegetazione:

R
Copia codice
NDVI_2017 <- DVI_2017 / (img_2017[[4]] + img_2017[[3]])
NDVI_2024 <- DVI_2024 / (img_2024[[4]] + img_2024[[3]])
Scopo: L’NDVI normalizza i valori tra -1 e 1, permettendo di differenziare aree con vegetazione densa (valori vicini a 1) e aree senza vegetazione (valori vicini a 0).
Descrizione: L'NDVI è calcolato come il rapporto normalizzato tra (NIR - RED) e (NIR + RED), risultando in una scala che va da -1 a 1.
Motivo: Questo indice standardizzato permette di confrontare la vegetazione su aree e periodi differenti, correggendo le variazioni dovute alle condizioni atmosferiche e alle caratteristiche del suolo.
Rilevanza ecologica: Valori di NDVI vicino a 1 indicano vegetazione vigorosa e sana, mentre valori vicini a 0 indicano superfici prive di vegetazione. Confrontando l'NDVI tra 2017 e 2024, è possibile monitorare lo stato di salute della vegetazione, individuando tendenze di deterioramento.

####6. Analisi della perdita di copertura vegetale
Soglia per la perdita di NDVI: Un valore di -0,2 identifica una perdita significativa.
Calcolo della perdita: Il codice identifica i pixel con valori di NDVI che scendono sotto questa soglia e li conta.
R
Copia codice
differenza_ndvi <- NDVI_2024 - NDVI_2017
perdita <- differenza_ndvi < soglia
numero_pixel_perduti <- cellStats(perdita, sum)
percentuale_area_perduta <- (numero_pixel_perduti / ncell(perdita)) * 100
Descrizione: In questa fase, viene calcolata la differenza tra l’NDVI del 2017 e quello del 2024 per quantificare la perdita di vegetazione.
Motivo: Sottraendo l'NDVI del 2024 a quello del 2017, è possibile visualizzare le aree dove la vegetazione è diminuita.
Rilevanza ecologica: Questa analisi consente di individuare rapidamente le zone in cui la vegetazione ha subito un degrado significativo. La perdita di vegetazione può essere associata a vari fattori, tra cui la deforestazione, l’urbanizzazione o il cambiamento climatico, fornendo informazioni utili per le politiche di conservazione.


#####7. Calcolo dell’EVI (Enhanced Vegetation Index)
EVI: Un indice simile all’NDVI ma più sensibile alle aree con elevata biomassa.
R
Copia codice
EVI_2017 <- G * ((img_2017[[4]] - img_2017[[3]]) / (img_2017[[4]] + C1 * img_2017[[3]] - C2 * img_2017[[1]] + L))
EVI_2024 <- G * ((img_2024[[4]] - img_2024[[3]]) / (img_2024[[4]] + C1 * img_2024[[3]] - C2 * img_2024[[1]] + L))
Descrizione: L’EVI è un indice avanzato che migliora l’NDVI, riducendo gli effetti atmosferici e aumentando la sensibilità nelle aree con vegetazione densa. Viene calcolato considerando non solo le bande NIR e RED ma anche la banda BLU.
Motivo: L’EVI è stato progettato per evitare la saturazione dei valori in zone con vegetazione molto densa.
Rilevanza ecologica: L’EVI è particolarmente utile nelle aree forestali tropicali o in habitat con biomassa elevata, dove l’NDVI può perdere di sensibilità. Questo indice permette un’analisi più accurata della copertura vegetale e fornisce una base più solida per valutazioni di salute delle foreste.



####8. Analisi PCA (Analisi delle Componenti Principali)
L’analisi PCA evidenzia variazioni strutturali nei dati, permettendo di ridurre la complessità del dataset.

R
Copia codice
NDVI_diff <- NDVI_2024 - NDVI_2017
DVI_diff <- DVI_2024 - DVI_2017
box <- stack(NDVI_diff, DVI_diff)
sr <- sampleRandom(box, 10000)
pca <- prcomp(sr)
pci <- predict(box, pca, index = 1:2)
Descrizione: La PCA riduce la dimensionalità del dataset, combinando le bande di riflettanza in nuove componenti che spiegano la variazione massima nei dati.
Motivo: Questa tecnica riduce la complessità dei dati, aiutando a individuare pattern significativi che potrebbero non essere evidenti analizzando le singole bande.
Rilevanza ecologica: La PCA consente di evidenziare cambiamenti strutturali nella copertura del suolo e di identificare fenomeni come la deforestazione o la desertificazione. Le componenti principali possono rivelare variazioni di suolo che richiedono interventi di gestione o di conservazione.


####9. T-test per NDVI
Scopo: Il T-test verifica se c’è una differenza statisticamente significativa nei valori di NDVI tra 2017 e 2024.
R
Copia codice
t_test_result <- t.test(ndvi_2017_values, ndvi_2024_values, paired = TRUE)
Descrizione: Viene eseguito un test statistico T per valutare se la differenza media nell'NDVI tra il 2017 e il 2024 è significativa.
Motivo: Il test T stabilisce la significatività statistica dei cambiamenti, distinguendo tra variazioni reali e fluttuazioni casuali.
Rilevanza ecologica: Un cambiamento statisticamente significativo nell'NDVI potrebbe suggerire un impatto ecologico rilevante, come una riduzione significativa della copertura vegetale. Questo risultato può influenzare la pianificazione di interventi conservativi o di politiche ambientali.

####10. Analisi del Land Cover
K-means clustering: Segmenta l’immagine in due classi (vegetazione buona vs vegetazione ridotta/assente).
R
Copia codice
k_cluster_2017 <- kmeans(single_nr_2017, centers = 2)
img_2017_class <- setValues(img_2017[[1]], k_cluster_2017$cluster)
Scopo: Differenziare le aree di vegetazione in buona salute da quelle degradate per visualizzare e quantificare i cambiamenti di copertura vegetale.
Descrizione: Viene applicato l’algoritmo K-means per segmentare l’immagine in diverse classi, suddividendo l’area in categorie (come vegetazione densa, vegetazione degradata, suolo nudo, ecc.).
Motivo: Il clustering K-means classifica automaticamente i pixel in gruppi basati su somiglianze di riflettanza, creando una mappa dettagliata della copertura del suolo.
Rilevanza ecologica: Questa mappatura delle classi del suolo fornisce una panoramica della diversità ecologica e identifica le aree a rischio di degrado. Questi dati sono utili per la gestione delle risorse naturali e per la definizione di strategie di recupero ecologico.


####11. Visualizzazione e confronto della copertura vegetale
Il codice finale crea grafici che confrontano le percentuali di copertura vegetale per ciascun anno e le esporta in un PDF:

R
Copia codice
ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2017, fill = copertura_vegetale)) + geom_bar(...)
Descrizione: Questa fase crea grafici che confrontano visivamente la copertura vegetale per ciascun anno.
Motivo: La rappresentazione grafica offre un modo intuitivo per analizzare le variazioni nella copertura vegetale.
Rilevanza ecologica: Fornisce un'immagine chiara del grado di perdita o guadagno di vegetazione. La comunicazione dei risultati in un formato visivo accessibile è essenziale per sensibilizzare il pubblico e facilitare la presa di decisioni.


