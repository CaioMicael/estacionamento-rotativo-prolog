% explain.pl
% Imprime a trilha (regras acionadas) e explica o resultado

:- dynamic fired/1.

explicar(summary(Totals, SumTotal, Occupancy)) :-
  format("~n=== Resumo do Estacionamento ===~n~n"),
  findall(1, member(vehicle_summary(_,_,_,_,_), Totals), Ones),
  length(Ones, NumVehicles),
  
  format("Resumo:~n"),
  format("- Veiculos: ~d~n", [NumVehicles]),
  format("- Total: R$ ~2f~n", [SumTotal]),
  format("- Ocupacao: ~w%~n", [Occupancy]),
  nl,
  format("~n=== Detalhamento por Veiculo ===~n"),
  forall(member(vehicle_summary(Plate,Break,Total,Duration,Rules), Totals), (
    % Converter duração para horas e minutos para exibição
    Hours is Duration // 60,
    Mins is Duration mod 60,
    
    format("~nVeiculo: ~w~n", [Plate]),
    format("- Tempo total: ~w horas e ~w minutos~n", [Hours, Mins]),
    
    % Se for isento, mostrar motivo
    ( member(exempt(_), Break) -> 
        ( member(exempt(exemption_mensalista), Rules) -> 
            format("- Status: Isento (Mensalista)~n")
        ; member(exempt(exemption_pne), Rules) -> 
            format("- Status: Isento (PNE)~n")
        ; format("- Status: Isento~n")
        )
    ; % Se não for isento, mostrar breakdown de custos
        format("- Status: Pagante~n"),
        format("- Detalhamento do valor:~n"),
        forall(member(band(Name,Min,Rate,Cost), Break), 
          format("  * Faixa ~w: ~w min a R$~2f/hora = R$~2f~n", [Name,Min,Rate,Cost])),
        
        % Verificar taxas adicionais
        ( member(night_fee(Fee), Rules) -> 
            format("  * Adicional noturno = R$~2f~n", [Fee]) ; true ),
        ( member(pernoite_fee(Fee), Rules) -> 
            format("  * Taxa de pernoite = R$~2f~n", [Fee]) ; true ),
        ( member(daily_cap_applied(Cap), Rules) -> 
            format("  * Teto diario aplicado = R$~2f~n", [Cap]) ; true ),
            
        format("- Valor total: R$~2f~n", [Total])
    )
  )),
  nl,
  format("~nInformacoes do Sistema:~n"),
  format("- Tarifas:~n"),
  faixa(f1,L1,R1), faixa(f2,L2,R2), faixa(f3,_,R3),
  format("  * Primeira hora (~w min): R$~2f/h~n", [L1,R1]),
  format("  * 1-3 horas (~w min): R$~2f/h~n", [L2,R2]),
  format("  * Acima de 3h: R$~2f/h~n", [R3]),
  format("- Taxas especiais:~n"),
  night_fee(NF), pernoite_fee(PF), daily_cap(DC),
  format("  * Adicional noturno (20:00-06:00): R$~2f~n", [NF]),
  format("  * Taxa de pernoite: R$~2f~n", [PF]),
  format("  * Teto diario: R$~2f~n", [DC]),
  format("- Arredondamento: blocos de 15 minutos.~n").

format_vehicle_summaries(Totals) :-
  format("~n=== Detalhes por Veiculo ===~n"),
  forall(member(vehicle_summary(Plate,Break,Total,Duration,Rules), Totals), (
    Hours is Duration // 60,
    Mins is Duration mod 60,
    format("~nVeiculo: ~w~n", [Plate]),
    format("Tempo: ~w horas e ~w minutos~n", [Hours, Mins]),
    ( member(exempt(pne), Break) -> format("Status: PNE (Isento)~n")
    ; member(monthly(_), Break) -> format("Status: Mensalista~n")
    ; format("Status: Cliente normal~n"),
      forall(member(band(Name,Min,Rate,Cost), Break), 
             format("* Faixa ~w: ~w min a R$~2f/h = R$~2f~n", [Name,Min,Rate,Cost]))
    ),
    format("Valor: R$~2f~n", [Total])
  )).

% fim explain.pl
