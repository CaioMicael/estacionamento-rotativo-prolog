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
    format("~n=== Configuracao do Estacionamento ===~n"),
    format("Por favor, informe os dados abaixo:~n~n"),
    get_valid_capacity,
    collect_vehicles.

% Helper para garantir entrada de capacidade válida
get_valid_capacity :-
    format("Digite a capacidade total de vagas (numero inteiro positivo): "),
    read(Cap),
    (number(Cap), Cap > 0) ->
        assertz(obs(capacity(Cap))),
        format("Capacidade configurada para ~w vagas.~n~n", [Cap])
    ;   format("~nErro: Por favor digite um numero inteiro positivo.~n~n"),
        get_valid_capacity.

collect_vehicles :-
  format("~n=== Registro de Veiculos ===~n"),
  format("Opcoes: [s] Inserir novo veiculo, [n] Finalizar registros~n"),
  format("Deseja inserir um registro de veiculo? [s/n]: "),
  read(R), downcase_atom(R, Ans),
  ( Ans == s -> collect_one, collect_vehicles
  ; Ans == n -> format("~nFinalizando coleta de registros...~n")
  ; format("~nEntrada invalida! Por favor digite 's' para sim ou 'n' para nao.~n~n"), 
    collect_vehicles ).

collect_one :-
  format("~n--- Novo Registro de Veiculo ---~n"),
  format("Placa do veiculo (Ex: ABC1234): "), read(Plate),
  
  format("~nTipo de usuario:~n"),
  format("Tipo de cliente:~n"),
  format("1) Avulso (paga por uso)~n"),
  format("2) Mensalista (R$300,00/mes)~n"),
  format("3) PNE (isento)~n"),
  format("Escolha o tipo [1-3]: "), read(Tipo),
  ( Tipo = 1 -> Mens = nao, PNE = nao
  ; Tipo = 2 -> Mens = sim, PNE = nao
  ; Tipo = 3 -> Mens = nao, PNE = sim
  ; format("Opcao invalida. Considerando como avulso.~n"),
    Mens = nao, PNE = nao
  ),
  
  % entrada: dia e hora
  format("~nDados de Entrada:~n"),
  format("Dia (0=hoje, 1=amanha, etc): "), read(EDay),
  format("Hora (formato HH:MM, ex: 08:30): "), read(ETimeAtom), atom_string(ETimeAtom, ETimeStr),
  ( parse_time_hm(ETimeStr, EH, EM) -> 
    EntryMin is EDay * 24 * 60 + EH*60 + EM
  ; format("~nErro: Formato de hora invalido! Use HH:MM (ex: 08:30).~n"),
    fail
  ),
  
  % saida
  format("~nDados de Saida:~n"),
  format("Dia (0=hoje, 1=amanha, etc): "), read(XDay),
  format("Hora (formato HH:MM, ex: 15:45): "), read(XTimeAtom), atom_string(XTimeAtom, XTimeStr),
  ( parse_time_hm(XTimeStr, XH, XM) ->
    ExitMin is XDay * 24 * 60 + XH*60 + XM
  ; format("~nErro: Formato de hora invalido! Use HH:MM (ex: 15:45).~n"),
    fail
  ),
  
  % Validar que saída é após entrada
  ( ExitMin > EntryMin ->
    assertz(vehicle_record(Plate, EntryMin, ExitMin, Mens, PNE)),
    format("~nRegistro inserido com sucesso para o veiculo ~w!~n", [Plate]),
    ( (Mens = sim ; PNE = sim) -> 
        format("Status: Isento de pagamento~n")
    ; format("Status: Pagante normal~n")
    )
  ; format("~nErro: A data/hora de saida deve ser posterior a entrada!~n"),
    fail
  ).

parse_time_hm(TimeStr, H, M) :-
  ( sub_string(TimeStr, Before, 1, After, ':') ->
      sub_string(TimeStr, 0, Before, _, Hs), sub_string(TimeStr, _, After, 0, Ms),
      number_string(H, Hs), number_string(M, Ms)
  ; % caso o usuário insira um atom HH:MM como leitura padrão
    ( atom(TimeStr) -> atom_string(TimeStr, S), parse_time_hm(S,H,M) ; format("Formato de hora invalido. Use HH:MM. Exemplo: 08:30~n"), fail )
  ).

cleanup :- retractall(obs(_)).

% fim ui.pl
