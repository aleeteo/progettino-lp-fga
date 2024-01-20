# readme oop.pl
Ecco una descrizione dettagliata del programma `oop.pl` e delle sue funzioni, basata sui commenti nel codice:

#### Descrizione del Programma
`oop.pl` è un'estensione per Prolog che introduce caratteristiche di programmazione orientata agli oggetti. Permette la definizione di classi, la creazione di istanze e la gestione di campi e metodi, integrando concetti OOP nel contesto del paradigma logico di Prolog.

#### Funzioni Principali
1. **`def_class`**: Definisce una classe e la memorizza nella base di conoscenza di Prolog. Accetta un nome di classe, genitori e parti (campi e metodi).

2. **`make`**: Crea un'istanza di una classe e la memorizza nella base di conoscenza.

3. **`is_class`**: Controlla se un nome passato è quello di una classe presente nella base di dati.

4. **`is_instance`**: Verifica se un nome passato sia il nome di un'istanza di una classe.

5. **`inst`**: Recupera un'istanza dato il nome con cui è stata creata.

6. **`field`**: Estrae il valore di un campo da una classe o un'istanza.

7. **`fieldx`**: Estrae il valore da una classe o un'istanza percorrendo una catena di attributi.

8. **`is_superclass`**: Controlla le gerarchie tra classi.

9. **`list_of_atoms`**: Verifica che ogni componente di una lista sia un atomo.

10. **`list_of_terms`**: Controlla che ogni componente di una lista sia un termine (non atomo), specificamente un campo o un metodo.

11. **`get_field_value`**: Restituisce il valore di un campo dato il suo nome.

12. **`get_class_parts`**: Restituisce campi (e metodi) di una classe data.

13. **`get_parents_parts`**: Restituisce le parti (campi e metodi) dei genitori di una classe.

14. **`has_type`**: Verifica che i campi di una classe rispettino un tipo specifico di dato.

15. **`get_class_field`**: Restituisce, se presente, il campo di una classe dato il suo nome.

16. **`check_type`**: Verifica che un argomento rispetti il tipo di dato specificato per un campo.

17. **`create_method`**: Prepara e salva un metodo nella base di conoscenza.

18. **`create_head`**: Utilizzato da `create_method` per creare l'intestazione del metodo con gli argomenti passati.

19. **`substring_replace`**: Rimpiazza una data sottostringa con un'altra, restituendo la stringa modificata.

20. **`substring_replace_scroll`**: Serve a `substring_replace` per scorrere tutte le occorrenze nella stringa di partenza.
