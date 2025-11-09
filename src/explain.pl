% explain.pl
% Imprime a trilha (regras acionadas) e explica o resultado

:- dynamic fired/1.

explicar(summary(Totals, SumTotal, Occupancy)) :-
  format("~n=== Detalhes da Consulta ===~n"),
  format("Veiculos processados: ~w~n", [Totals]),
  format("Total geral (R$): ~2f~n", [SumTotal]),
  format("Ocupacao media (percentual estimado): ~w%~n", [Occupancy]),
  nl,
  format("[Regras/Eventos acionados por veiculo]~n"),
  forall(member(vehicle_summary(Plate,Break,Total,Duration,Rules), Totals), (
    format("\nVeiculo: ~w~n", [Plate]),
    format("- Duracao (min): ~w~n", [Duration]),
    format("- Breakdown por faixa:~n"),
    forall(member(band(Name,Min,Rate,Cost), Break), format("   * ~w -> ~w min @ R$~2f/h = R$~2f~n", [Name,Min,Rate,Cost])),
    ( member(exempt(_), Break) -> format("- Isento do pagamento (mensalista/PNE).~n") ; format("- Total deste veiculo: R$~2f~n", [Total]) ),
    format("- Regras disparadas: ~w~n", [Rules])
  )),
  nl,
  format("Observacoes:~n"),
  format("- As tarifas sao por hora e convertidas para custo por minuto.~n"),
  format("- Arredondamento: blocos de 15 minutos (ceil).~n"),
  format("- Teto diario aplicado por dia completo de permanencia.~n").

% helper para pretty print do summary
format_vehicle_summaries(Totals) :-
  forall(member(vehicle_summary(Plate,Break,Total,Duration,Rules), Totals), (
    format("~nVeiculo: ~w~n", [Plate]),
    format("- Duracao: ~w min~n", [Duration]),
    ( member(exempt(_), Break) -> format("- Isento\n") ; format("- Total: R$~2f~n", [Total]) ),
    format("- Regras: ~w~n", [Rules])
  )).

% fim explain.pl
