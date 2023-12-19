%%%% -*- Mode: Prolog -*-
%%%% OOΠ.pl

%%% def class definisce la struttura di una classe e la memorizza nella
%%% “base di conoscenza” di Prolog.
def_class(Class_name, Parents)
def_class(Class_name, Parents, Parts)

%%% make: crea una nuova istanza di una classe.
make(Instance_name, Class_name, [_])

%%% is_class: ha successo se l’atomo passatogli `e il nome di una classe.
is_class(Class_name)

%%% is_instance: ha successo se l’oggetto passatogli `e l’istanza di una classe.
is_instance(Value)
is_instance(Value, Class_name)

%%% inst: recupera un’istanza dato il nome con cui `e stata creata da make.
inst(Instance_name, Instance)

%%% field: estrae il valore di un campo da una classe.
field(Instance, Field_name, Result)

%%% fieldx: estrae il valore da una classe percorrendo una catena di attributi.
fieldx(Instance, Field_name, Result)

%%% end of file -- OOΠ.pl
