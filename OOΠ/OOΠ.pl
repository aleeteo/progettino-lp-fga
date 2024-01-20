%%%% -*- Mode: Prolog -*-
%%%% Melon Cristino 899647
%%%% oop.pl --

%%% def_class definisce la struttura di una classe
%%% e la memorizza nella "base di conoscenza" di prolog
def_class(Class_name, Parents):-
    def_class(Class_name, Parents, []).

def_class(Class_name, Parents, Parts):-
    atom(Class_name),
    list_of_atoms(Parents),
    list_of_terms(Parts),

    retractall(class(Class_name, _, _)),

    assertz(class(Class_name, Parents, Parts)).

%%% make crea una nuova istanza di una classe
%%% e la memorizza nella base di conoscenza
make(Instance_name, Class_name):-
    make(Instance_name, Class_name, []).

make(Instance_name, Class_name, Fields):-
    is_class(Class_name),
    retractall(instance(Instance_name, _, _)),

    has_type(instance(Instance_name, Class_name, Fields)),

    assertz(instance(Instance_name, Class_name, Fields)).

%%% is_class controlla che il nome passato
%%% sia il nome di una classe presente nella base dati
is_class(Class_name):-
    class(Class_name, _, _).

%%% is_instance controlla che il nome passato
%%% sia il nome di ua istanza di una classe
is_instance(Value):-
    atom(Value), !,
    instance(Value, _, _).

is_instance(Value, Class_name):-
    atom(Value), !,
    instance(Value, Parents, _),
    is_class(Class_name),
    is_superclass(Class_name, Parents).

%%% inst recupera un'istanza dato il
%%% nome con cui è stata creata da make
inst(Instance_name, Instance):-
    atom(Instance_name),
    call(instance(Instance_name, X, Y)),
    Instance = instance(Instance_name, X, Y).

%%% field estrae il valore di un campo da una classe
field(Instance, Field_name, Result):-
    instance(Instance, _, Fields),
    get_field_value(Field_name, Fields, Result).

field(Instance, Field_name, Result):-
    instance(Instance, Class, _),
    get_class_parts(Class, Parts),
    get_field_value(Field_name, Parts, Result).

field(instance(Inst_name, _, _), Field_name, Result):-
    instance(Inst_name, _, Fields),
    get_field_value(Field_name, Fields, Result).

field(instance(_, Class_name, _), Field_name, Result):-
    get_class_parts(Class_name, Parts),
    get_field_value(Field_name, Parts, Result).

field(instance(_, _, Fields), Field_name, Result):-
    get_field_value(Field_name, Fields, Result).

%%% fieldx estrae il valore da una classe
%%% percorrendo una catena di attributi
fieldx(Instance, [X | Xs], Result):-
    field(Instance, X, Y),
    length(Xs, L),
    L > 0, !,
    fieldx(Y, Xs, Result).

fieldx(Instance, [X | Xs], Result):-
    field(Instance, X, Result),
    length(Xs, L),
    L = 0.

fieldx(instance(Instance_name, _, _), X, Result):-
    fieldx(Instance_name, X, Result).

%%% is_superclass controlla le gerarchie tra classi
is_superclass(Class, Class).

is_superclass(Class, Superclass):-
    member(Class, Superclass).

is_superclass(Class, [X | Xs]):-
    class(X, Parents, _),
    is_superclass(Class, Parents), !,
    is_superclass(Class, Xs).

%%% list_of_atoms controlla che ogni
%%% componente della lista sia un atomo
list_of_atoms([]).

list_of_atoms([X | Xs]):-
    atom(X),
    list_of_atoms(Xs).

%%% list_of_terms controlla che ogni
%%% componente della lista sia un termine (non atomo)
%%% nello specifico viene controllato se un termine
%%% sia un field o un method
list_of_terms([]).

list_of_terms([X | Xs]):-
     compound(X),
     functor(X, method, _),

     arg(1, X, Name),
     arg(2, X, Args),
     arg(3, X, Body),
     create_method(Name, Args, Body),
     list_of_terms(Xs).

list_of_terms([X | Xs]):-
    compound(X),
    list_of_terms(Xs).

%%% get_field_value restituisce il valore
%%% di un field di cui è dato il nome
get_field_value(_, [], _):- fail.

get_field_value(Field_name, [X | Xs], Value):-
    arg(1, X, Field_name),
    arg(2, X, Value).

get_field_value(Field_name, [X | Xs], Value):-
    get_field_value(Field_name, Xs, Value).

%%% get_class_parts restituisce i field (e metodi)
%%% di una classe data
get_class_parts(Class_name, Parts):-
    class(Class_name, Parents, X),
    get_parents_parts(Parents, Y),
    append(X, Y, Parts), !.

get_class_parts(Class, []):- !.

get_class_parts([], []):- !.

%%% get_parents_parts restituisce (se ci sono)
%%% le parti di una lista di classi (parents)
get_parents_parts([X | Xs], Result):-
    get_class_parts(X, Y),
    get_parents_parts(Xs, Parts),
    append(Parts, Y, Result).

get_parents_parts([], []).

%%% has_type verifica se i field di una
%%% classe richiedono un tipo soecifico di dato
%%% ed in caso verifica che i field dell'istanza li rispettino

has_type(instance(Instance_name, Class_name, Fields)):-
    get_class_parts(Class_name, Parts),
    has_type(Fields, Parts).

has_type([], _):- !.

has_type(_, []):- !.

has_type([Field_name = Value | Fields], Parts) :-
    get_class_field(Parts, Field_name, Result),
    functor(Result, _, 3),
    arg(3, Result, Type),
    check_type(Type, Value),
    has_type(Fields, Parts).

has_type([Field_name = Value | Fields], Parts) :-
    get_class_field(Parts, Field_name, Result),
    functor(Result, _, 2),
    has_type(Fields, Parts).

%%% get_class_field restituisce (se c'è)
%%% il field con Field_name passato

get_class_field([], _, []):- !.

get_class_field([P | Parts], Field_name, P):-
    functor(P, field, _),
    arg(1, P, Field_name).

get_class_field([_ | Parts], Field_name, P):-
    get_class_field(Parts, Field_name, P).


%%% check_type verifica che l'argomento passato
%%% rispetti il tipo di dato del field del oggetto
check_type(Type, Arg):-
    string_lower(Type, X),
    atom_string(Call, X),
    call(Call, Arg), !.

%%% create_method prepara il metodo e lo salva
%%% nella base di conoscenza,
create_method(Name, Args, Body):-
    term_string(Name, X),
    %%term_string(Args, Y),
    term_string(Body, Z),

    create_head(Args, "(this", Head),

    string_concat(X, Head, Method_head),

    string_concat(Method_head, Z, Method_head_body),

    substring_replace(Method_head_body, "this", "This", Method_atomic),
    term_to_atom(Method, Method_atomic),

    assertz(Method).

%%% create_head è un predicato utilizzato da create_method
%%% per creare l'intestazione del metodo con gli argomenti passati.
create_head([], Str, Res):-
    string_concat(Str, "):-", Res).

create_head([X | Xs], Str, Res):-
    string_concat(Str, ", ", Con1),
    term_to_atom(X, A),
    atom_concat(Con1, A, Con2),
    create_head(Xs, Con2, Res).

%%% substring_replace rimpiazza una data sottostringa con
%%% un'altra sottostringa, per poi restituire la stringa modificata
substring_replace(String, Find, Replace, New_string):-
    substring_replace_scroll(String, 1, Find, Replace, Res),
    substring_replace(Res, Find, Replace, New_string), !.

substring_replace(String, _, _, String).

%%% substring_replace_scroll serve a substring_replace
%%% per scorrere tutte le occorrenze nella stringa di partenza

substring_replace_scroll(String, N, Find, Replace, Res):-
    call_nth(sub_atom(String, Before, _, After, Find), N),
    sub_atom(String, 0, Before, _, Left),
    sub_atom(String, _, After, 0, Right),
    atomic_list_concat([Left, Replace, Right], Res).

%%%%End of file -- oop.pl
