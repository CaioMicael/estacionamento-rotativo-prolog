% main.pl
% Orquestra menu e fluxo

:- ['kb.pl','rules.pl','ui.pl','explain.pl'].

start :-
  banner, menu.

banner :-
  format("~n=== Sistema Especialista - Estacionamento Rotativo ===~n"),
  format("Desenvolvido por: Caio Krieger e Felipe Macedo~n~n").

menu :-
  format("1) Executar consulta~n2) Sair~n> "),
  read(Opt),
  ( Opt = 1 -> run_case, cleanup_all, menu
  ; Opt = 2 -> format("Saindo...~n")
  ; format("Opcao invalida.~n"), menu ).

run_case :-
  coletar_observacoes,
  ( meta(Result) ->
      explicar(Result),
      format("~nRESULTADO GERAL: ~w~n", [Result])
  ; format("~nNao foi possivel concluir a consulta. Revise as respostas inseridas.~n")
  ),
  true.

cleanup_all :-
  retractall(obs(_)),
  retractall(vehicle_record(_,_,_,_,_)),
  retractall(fired(_)).
