/** <module> Redis protocol parser and writer.

@author Can Bican
@license GPL

*/


:- module(resp,[resp_write/2, resp_parse/2]).

unicode_size(Char,Size) :-
	\+ integer(Char),
	char_code(Char,Code),
	unicode_size(Code,Size),
	!.
unicode_size(0,0).
unicode_size(Code,1) :- Code =< 127.
unicode_size(Code,2) :- Code > 127 , Code =< 2047.
unicode_size(Code,3) :- Code > 2047 , Code =< 65535.
unicode_size(Code,4) :- Code > 65535 , Code =< 1114111.

unicode_byte_length(String,Length) :-
	string_codes(String,Codes),
	unicode_byte_length_list(Codes,Length).

unicode_byte_length_list([],0) :- !.
unicode_byte_length_list([Code|Codes],Length) :-
	unicode_size(Code,L0),
	unicode_byte_length_list(Codes,L),
	Length is L0 + L.

get_chars(_,[],0) :- !.
get_chars(Stream,[First|Rest],Count) :-
	get_char(Stream,First),
	unicode_size(First,SS),
	C is Count - SS,
	get_chars(Stream,Rest,C),
	!.

%! resp_write(Stream, List) is nondet.
%
%  Write contents of the List to the Stream in Redis protocol format.
%

resp_write(Stream,Array) :-
	resp_write(Stream,Array,false).

resp_write(_,[],true) :-
	!.

resp_write(Stream,[],_) :-
	format(Stream,"*0~w",["\r\n"]),
	!.
resp_write(Stream,[nil],_) :-
	format(Stream,"*-1~w",["\r\n"]),
	!.
resp_write(Stream,[First|Rest],IsFirst) :-
	is_list([First|Rest]),
	length([First|Rest],Length),
	( IsFirst == true ;
	format(Stream,"*~w~w",[Length,"\r\n"])),
	resp_write_one(Stream,First),
	resp_write(Stream,Rest,true),
	!.
resp_write(Stream,Term,_) :-
	resp_write_one(Stream,Term),
	!.

resp_write_one(Stream,string(String)) :-
	format(Stream,"+~w~w",[String,"\r\n"]).
resp_write_one(Stream,error(String)) :-
	format(Stream,"-~w~w",[String,"\r\n"]).
resp_write_one(Stream,integer(String)) :-
	format(Stream,":~w~w",[String,"\r\n"]).
resp_write_one(Stream,bulk(nil)) :-
	format(Stream,"$~w~w",["-1","\r\n"]).
resp_write_one(Stream,bulk(String)) :-
	unicode_byte_length(String,Length),
	format(Stream,"$~w~w~w~w",[Length,"\r\n",String,"\r\n"]).
resp_write_one(Stream,Array) :-
	is_list(Array),
	resp_write(Stream,Array).

resp_writef_array(Stream,Array) :-
	format(Stream,"~w",[Array]).

resp_write_one_array([],"") :- !.
resp_write_one_array([First|Rest],ResultString) :-
	new_memory_file(MF),
	open_memory_file(MF, write, Stream),
	resp_write_one(Stream,First),
	close(Stream),
	memory_file_to_string(MF,FirstResult,utf8),
	free_memory_file(MF),
	resp_write_one_array(Rest,ResultRest),
	swritef(ResultString,"%w%w",[FirstResult,ResultRest]).

%! resp_parse(Stream, List) is nondet.
%
% Parse the input from Stream as a List of predicates.
%
resp_parse(Stream,Result) :-
	read_line_to_codes(Stream,[TypeCode|ArgumentCodes]),
	char_code(Type,TypeCode),
	atom_chars(Arguments,ArgumentCodes),
	resp_parse(Type,Arguments,Stream,Result).
	% resp_parse(Stream,RestResult).
resp_parse(_,[]) :- !.

resp_parse('+',Argument,_,string(Argument)) :- !.
resp_parse(':',Argument,_,integer(ArgumentInteger)) :-
	atom_number(Argument,ArgumentInteger),
	!.
resp_parse('-',Argument,_,error(Argument)) :- !.
resp_parse('$',Argument,_,bulk(nil)) :-
	atom_number(Argument,Count),
	Count < 0,
	!.
resp_parse('$',Argument,Stream,bulk(Result)) :-
	atom_number(Argument,Count),
	get_chars(Stream,ResultChars,Count),
	string_codes(Result,ResultChars),
	get_code(Stream,_),
	get_code(Stream,_),
	!.
resp_parse('*',Argument,Stream,Result) :-
	atom_number(Argument,Count),
	resp_parse_array(Stream,Count,Result),
	!.

resp_parse_array(_,-1,[nil]) :- !.
resp_parse_array(_,0,[]) :- !.
resp_parse_array(Stream,Count,[First|Rest]) :-
	read_line_to_codes(Stream,[TypeCode|ArgumentCodes]),
	char_code(Type,TypeCode),
	atom_chars(Arguments,ArgumentCodes),
	resp_parse(Type,Arguments,Stream,First),
	Count1 is Count - 1,
	resp_parse_array(Stream,Count1,Rest),
	!.
