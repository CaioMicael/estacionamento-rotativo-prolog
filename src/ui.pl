% ui.pl
% Perguntas guiadas para popular fatos em tempo de execução

:- dynamic obs/1.
:- dynamic vehicle_record/5.

pergunta_s_n(Campo, Texto) :-
    format("~w (s/n): ", [Texto]),
    read(Ans0),
    downcase_atom(Ans0, Ans),
    ( Ans == s -> assertz(obs(Campo)), true
    ; Ans == n -> true
    ; format("Entrada invalida. Digite s ou n.~n"), pergunta_s_n(Campo, Texto)
    ).

coletar_observacoes :-
    format("Insercao de registros de estacionamento.~n"),
    format("Informe a capacidade do estacionamento (numero inteiro). Deixe em branco para default.~n"),
    read(Cap),
    ( Cap = [] -> true ; assertz(obs(capacity(Cap))) ),

    collect_vehicles.

collect_vehicles :-
  format("\nDeseja inserir um registro de veiculo? (s/n): "),
  read(R), downcase_atom(R, Ans),
  ( Ans == s -> collect_one, collect_vehicles ; Ans == n -> true ; format("Entrada invalida.~n"), collect_vehicles ).

collect_one :-
  format("Placa (atom): "), read(Plate),
  format("Mensalista? (s/n): "), read(M), downcase_atom(M, Mlow),
  ( Mlow == s -> Mens = sim ; Mens = nao ),
  format("PNE (isencao)? (s/n): "), read(P), downcase_atom(P, Plow),
  ( Plow == s -> PNE = sim ; PNE = nao ),
  % entrada: dia e hora
  format("Entrada - dia (inteiro): "), read(EDay),
  format("Entrada - hora (HH:MM) (ex: 08:30): "), read(ETimeAtom), atom_string(ETimeAtom, ETimeStr),
  parse_time_hm(ETimeStr, EH, EM),
  EntryMin is EDay * 24 * 60 + EH*60 + EM,
  % saida
  format("Saida - dia (inteiro): "), read(XDay),
  format("Saida - hora (HH:MM) (ex: 15:45): "), read(XTimeAtom), atom_string(XTimeAtom, XTimeStr),
  parse_time_hm(XTimeStr, XH, XM),
  ExitMin is XDay * 24 * 60 + XH*60 + XM,
  assertz(vehicle_record(Plate, EntryMin, ExitMin, Mens, PNE)),
  format("Registro inserido para ~w.~n", [Plate]).

parse_time_hm(TimeStr, H, M) :-
  ( sub_string(TimeStr, Before, 1, After, ':') ->
      sub_string(TimeStr, 0, Before, _, Hs), sub_string(TimeStr, _, After, 0, Ms),
      number_string(H, Hs), number_string(M, Ms)
  ; % caso o usuário insira um atom HH:MM como leitura padrão
    ( atom(TimeStr) -> atom_string(TimeStr, S), parse_time_hm(S,H,M) ; format("Formato de hora invalido. Use HH:MM. Exemplo: 08:30~n"), fail )
  ).

cleanup :- retractall(obs(_)).

% fim ui.pl
