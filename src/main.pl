% main.pl
% Orquestra menu e fluxo

:- ['kb.pl','rules.pl','ui.pl','explain.pl'].

start :-
  banner, menu.

banner :-
  format("~n========================================================~n"),
  format("=== Sistema Especialista - Gerenciamento de Estacionamento ===~n"),
  format("========================================================~n~n"),
  format("Bem-vindo ao sistema de calculo de tarifas de estacionamento!~n"),
  format("Este sistema calcula automaticamente valores considerando:~n"),
  format("- Tarifas progressivas por tempo de permanencia~n"),
  format("- Adicional noturno (20:00-06:00)~n"),
  format("- Taxa de pernoite~n"),
  format("- Isencoes para mensalistas e PNE~n~n"),
  format("Desenvolvido por: @CaioMicael e @FelipeMacedoK~n~n").

menu :-
  format("~n=== Menu Principal ===~n"),
  format("1) Nova consulta de estacionamento~n"),
  format("2) Sair do sistema~n~n"),
  format("Digite sua opcao [1-2]: "),
  read(Opt),
  ( Opt = 1 -> format("~nIniciando nova consulta...~n"), run_case, cleanup_all, menu
  ; Opt = 2 -> format("~nObrigado por usar nosso sistema!~n")
  ; format("~nOpcao invalida! Por favor digite 1 ou 2.~n"), menu ).

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
