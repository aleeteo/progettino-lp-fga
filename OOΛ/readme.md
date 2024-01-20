# readme ool.lisp
## Descrizione del Programma

`ool.lisp` è un'estensione per Lisp che introduce funzionalità di programmazione orientata agli oggetti. Questo include la creazione di classi, gestione di istanze, campi e metodi, mirando a migliorare la modularità e la riusabilità del codice in Lisp.

## Funzioni Principali

1. **`def-class`**: Crea e inserisce in memoria una classe. Richiede il nome della classe, i genitori e le parti (campi e metodi).

2. **`make`**: Crea un'istanza di una classe. Verifica che la classe data sia valida e che i campi dell'istanza siano corretti.

3. **`is-class`**: Verifica che un nome di classe sia presente in memoria e che corrisponda a una classe definita.

4. **`is-instance`**: Controlla se un valore è un'istanza di una classe data, considerando anche le sottoclassi.

5. **`field`**: Recupera il valore di un campo di un'istanza, verificando che l'istanza e il nome del campo siano validi.

6. **`field*`**: Estrae il valore di una catena di attributi di un'istanza.

7. **`get-fields`** e **`get-methods`**: Funzioni ausiliarie per ottenere campi e metodi formattati da una classe o da genitori di una classe.

8. **`type-check`**: Controlla il tipo di un campo, assicurando che il valore sia dello stesso tipo o di un sottotipo del tipo specificato.

9. **`verify-instance-fields`** e **`verify-class-field`**: Assicurano che i campi di un'istanza o di una definizione di classe siano corretti e non vadano in conflitto con i campi dei genitori.

10. **`valid-method-structure`**: Verifica che la struttura di un metodo sia corretta, controllando che sia una lista con una lunghezza e una struttura specificate.

11. **`field-class`** (variante): Simile a `field`, ma operante sulle classi. Trova un campo in una classe, considerando anche i campi ereditati dai genitori.

12. **`find-field`** (variante): Cerca un campo specifico all'interno di una lista di campi. Viene utilizzata sia per le istanze che per le classi.

13. **`class-field-type`**: Recupera il tipo di un campo specifico in una classe, considerando l'ereditarietà dei campi dai genitori.

14. **`find-field-type`**: Funzione ausiliaria utilizzata da `class-field-type` per trovare il tipo di un campo specifico.

15. **`deep-member`** (variante): Una versione più profonda della funzione `member` di Lisp, che può cercare all'interno di liste annidate.

