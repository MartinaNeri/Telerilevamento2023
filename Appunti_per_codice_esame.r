Calcolo delle differenze annuali dell'NDVI
Il codice:

r
Copia codice
diff_list <- lapply(1:(length(ndvi_list) - 1), function(i) {
  ndvi_list[[i + 1]] - ndvi_list[[i]]
})
Spiegazione passo-passo
1. lapply
lapply è una funzione in R che applica una funzione a ciascun elemento di una lista e restituisce una lista dei risultati. La sua sintassi di base è:

r
Copia codice
lapply(X, FUN)
Dove:

X è una lista o un vettore su cui applicare la funzione.
FUN è la funzione da applicare a ciascun elemento della lista X.
2. La lista di input 1:(length(ndvi_list) - 1)
1:(length(ndvi_list) - 1) genera una sequenza di numeri da 1 fino a uno meno della lunghezza di ndvi_list. Supponiamo che ndvi_list contenga NDVI per gli anni dal 2018 al 2024, quindi ha 7 elementi. Quindi 1:(7 - 1) genera la sequenza 1, 2, 3, 4, 5, 6.

3. La funzione anonima
La funzione anonima è definita come:

r
Copia codice
function(i) {
  ndvi_list[[i + 1]] - ndvi_list[[i]]
}
Questa funzione prende un indice i come input e calcola la differenza tra gli elementi di ndvi_list alla posizione i + 1 e i.

4. Calcolo delle differenze
ndvi_list[[i + 1]]: Accede all'elemento di ndvi_list all'indice i + 1. Ad esempio, se i è 1, allora ndvi_list[[1 + 1]] è ndvi_list[[2]].
ndvi_list[[i]]: Accede all'elemento di ndvi_list all'indice i. Ad esempio, se i è 1, allora ndvi_list[[1]] è ndvi_list[[1]].
La differenza ndvi_list[[i + 1]] - ndvi_list[[i]] rappresenta la variazione di NDVI tra l'anno i + 1 e l'anno i.

5. Restituzione di diff_list
lapply applica questa funzione anonima a ciascun elemento della sequenza 1:(length(ndvi_list) - 1). Quindi, per ogni i da 1 a 6, calcola la differenza tra gli NDVI di anni consecutivi e memorizza queste differenze in una lista chiamata diff_list.
