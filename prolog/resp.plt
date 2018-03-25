:- begin_tests(resp).
:- use_module(resp).

resp_test_case(writef('+OK\r\n',[]),string('OK')).
resp_test_case(writef('+\r\n',[]),string('')).
resp_test_case(writef('+中文\r\n',[]),string('中文')).
resp_test_case(writef('-Error message\r\n',[]),error('Error message')).
resp_test_case(writef(':-1\r\n',[]),integer(-1)).
resp_test_case(writef(':0\r\n',[]),integer(0)).
resp_test_case(writef(':1456061893587000000\r\n',[]),integer(1456061893587000000)).
resp_test_case(writef('$-1\r\n',[]),bulk(nil)).
resp_test_case(writef('$0\r\n\r\n',[]),bulk("")).
resp_test_case(writef('$6\r\n中文\r\n',[]),bulk("中文")).
resp_test_case(writef('$17\r\n你好！\n 换行\r\n',[]),bulk("你好！\n 换行")).
resp_test_case(writef('*-1\r\n',[]),[nil]).
resp_test_case(writef('*0\r\n',[]),[]).
resp_test_case(writef('*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n',[]),[bulk("foo"),bulk("bar")]).
resp_test_case(writef('*3\r\n:1\r\n:2\r\n:3\r\n',[]),[integer(1),integer(2),integer(3)]).
resp_test_case(writef('*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$6\r\nfoobar\r\n',[]),[integer(1),integer(2),integer(3),integer(4),bulk("foobar")]).
resp_test_case(writef('*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Foo\r\n-Bar\r\n',[]),[[integer(1),integer(2),integer(3)],[string('Foo'),error('Bar')]]).
resp_test_case(writef('*3\r\n$3\r\nfoo\r\n$-1\r\n$3\r\nbar\r\n',[]),[bulk("foo"),bulk(nil),bulk("bar")]).
resp_test_case(writef('*3\r\n$3\r\nfoo\r\n$17\r\n你好！\n 换行\r\n$3\r\nbar\r\n',[]),[bulk("foo"),bulk("你好！\n 换行"),bulk("bar")]).

string_format(String,Format,Arguments) :-
	new_memory_file(Handle),
	open_memory_file(Handle,write,S),
	format(S,Format,Arguments),
	close(S),
	open_memory_file(Handle,read,S2,[free_on_close(true)]),
	read_string(S2,5000,String).

test(resp_parse,[
    setup(new_memory_file(RespStream)),
    cleanup(free_memory_file(RespStream)),
    forall(resp_test_case(RespFormat,PrologFormat))]) :-
	open_memory_file(RespStream, write, S),
	format(S,"~@",[RespFormat]),
	close(S),
	open_memory_file(RespStream, read, S2, [free_on_close(true)]),
	resp_parse(S2,Expected),
	close(S2),
	assertion(Expected == PrologFormat),
	!.

test(resp_write,[
    setup(new_memory_file(RespStream)),
    cleanup(free_memory_file(RespStream)),
    forall(resp_test_case(RespFormat,PrologFormat))]) :-
	open_memory_file(RespStream, write, S),
	resp_write(S,PrologFormat),
	close(S),
	open_memory_file(RespStream, read, S2, [free_on_close(true)]),
	read_string(S2,5000,Result),
	close(S2),
	string_format(Expected,"~@",[RespFormat]),
	assertion(Expected == Result),
	!.

:- end_tests(resp).
