% rules.pl
% Regras do dominio e meta principal

:- dynamic obs/1.
:- dynamic fired/1.
:- dynamic vehicle_record/5.

% meta/1: calcula resultados para todos os veículos inseridos
% Resultado: summary(TotaisPorVeiculo, SomaTotal, OcupacaoMedia)
meta(summary(Totals, SumTotal, OccupancyMean)) :-
  % recolher todos os registros
  findall(V, vehicle_record(V,_,_,_,_), Plates),
  Plates \= [],
  maplist(calc_vehicle, Plates, VehicleResults),
  % VehicleResults = [res(Plate,Breakdown,Total,DurationMins,RulesFired), ...]
  aggregate_results(VehicleResults, Totals, SumTotal, AvgDuration),
  compute_occupancy(AvgDuration, OccupancyMean),
  SumTotal >= 0.

/* ===== Predicados auxiliares ===== */

calc_vehicle(Plate, res(Plate, Breakdown, Total, Duration, FiredRules)) :-
  retractall(fired(_)),
  vehicle_record(Plate, Entry, Exit, Mensalista, PNE),
  compute_duration(Entry, Exit, Duration),
  ( (Mensalista == sim ; PNE == sim) ->
      Total = 0.0,
      Breakdown = [exempt(Motivo) | []],
      (Mensalista == sim -> assertz(fired(exemption_mensalista)); true),
      (PNE == sim -> assertz(fired(exemption_pne)); true)
  ; % senão calcular normalmente
    block_minutes(B),
    round_up(Duration, B, Rounded),
    assertz(fired(rounding_block(Rounded,B))),
    compute_cost_by_bands(Rounded, Breakdown, PartialCost),
    apply_daily_cap(Rounded, PartialCost, CostAfterCap),
    apply_night_pernoite(Entry, Exit, CostAfterCap, Total)
  ),
  findall(R, fired(R), FiredRules).

% compute_duration: espera Entry e Exit no formato minutes_since_epoch(Days*24*60 + minutes)
compute_duration(Entry, Exit, Duration) :-
  Duration is max(0, Exit - Entry).

% round_up(Duration, Block, Rounded)
round_up(Duration, Block, Rounded) :-
  R is (Duration + Block - 1) // Block * Block,
  Rounded = R.

% compute_cost_by_bands(DurationMin, BreakdownList, Cost)
compute_cost_by_bands(Duration, Breakdown, Cost) :-
  % alocar minutos por faixa
  findall(f(F,L,RH), faixa(F,L,RH), Faixas0),
  sort_faixas(Faixas0, Faixas),
  allocate_minutes(Faixas, Duration, Alloc),
  compute_cost_alloc(Alloc, Breakdown, Cost).

sort_faixas(Faixas0, Faixas) :-
  % ordenar por limite ascendente, mantendo faixa com limite 0 por último
  map_list_to_pairs(faixa_sort_key, Faixas0, Pairs),
  keysort(Pairs, Sorted),
  pairs_values(Sorted, Faixas).

faixa_sort_key(f(F,L,R), Key) :-
  (L =:= 0 -> Key = 999999 ; Key = L).

allocate_minutes([], _, []).
allocate_minutes([f(Name,Lim,Rate)|T], Remaining, [alloc(Name,Use,Lim,Rate)|AllocT]) :-
  ( Lim =:= 0 -> Use = Remaining
  ; Use is min(Remaining, Lim)
  ),
  Rem2 is Remaining - Use,
  ( Rem2 < 0 -> Rem3 = 0 ; Rem3 = Rem2 ),
  allocate_minutes(T, Rem3, AllocT).

compute_cost_alloc(Alloc, Breakdown, Cost) :-
  % rates por hora -> converter para por minuto
  compute_cost_alloc(Alloc, 0.0, [], Cost, Breakdown).
compute_cost_alloc([], AccCost, AccB, AccCost, AccB).
compute_cost_alloc([alloc(Name,Use,Lim,Rate)|T], AccCost, AccB, CostOut, Breakdown) :-
  PerMin is Rate / 60.0,
  ThisCost is Use * PerMin,
  format(atom(Desc), "~w: ~w min @ R$~2f/h => R$~2f", [Name, Use, Rate, ThisCost]),
  append(AccB, [band(Name,Use,Rate,ThisCost)], NB),
  Acc2 is AccCost + ThisCost,
  compute_cost_alloc(T, Acc2, NB, CostOut, Breakdown).

% apply_daily_cap(DurationMin, CostIn, CostOut)
apply_daily_cap(Duration, CostIn, CostOut) :-
  daily_cap(Cap),
  Days is ceiling(Duration / (24*60)),
  Max is Cap * Days,
  ( CostIn > Max -> CostOut = Max, assertz(fired(daily_cap_applied(Max))) ; CostOut = CostIn ).

% apply_night_pernoite: checa se incide taxa noturna e pernoite
apply_night_pernoite(Entry, Exit, CostIn, CostOut) :-
  ( pernoite(Entry,Exit) ->
      pernoite_fee(F),
      Cost1 is CostIn + F,
      assertz(fired(pernoite_fee(F)))
  ; Cost1 = CostIn ),
  ( night_period_overlap(Entry,Exit) ->
      night_fee(NF),
      Cost2 is Cost1 + NF,
      assertz(fired(night_fee(NF)))
  ; Cost2 = Cost1 ),
  CostOut is round_to_2_decimals(Cost2).

% pernoite se saída estiver em dia posterior (diferença > 24h? para simplicidade: se exit_day > entry_day)
pernoite(Entry, Exit) :-
  EntryDay is Entry // (24*60),
  ExitDay is Exit // (24*60),
  ExitDay > EntryDay.

% night_period_overlap: verifica se há interseção entre [Entry,Exit] e qualquer periodo 20:00..06:00
night_period_overlap(Entry, Exit) :-
  % converter minutos desde epoch para minuto do dia (0..1439) e dias
  EntryDay is Entry // (24*60), EntryM is Entry mod (24*60),
  ExitDay is Exit // (24*60), ExitM is Exit mod (24*60),
  % se atravessa dias, iterar por cada dia
  night_start(NS), night_end(NE),
  between(EntryDay, ExitDay, D),
  DayStart is D * 24 * 60,
  PeriodStart is DayStart + NS,
  ( NE =< NS -> PeriodEnd is DayStart + 24*60, PeriodEnd2 is (D+1)*24*60 + NE, % night spans midnight
    ( overlap_interval(Entry,Exit,PeriodStart,PeriodEnd) ; overlap_interval(Entry,Exit,DayStart,PeriodEnd2) )
  ; PeriodEnd is DayStart + NE, overlap_interval(Entry,Exit,PeriodStart,PeriodEnd) ).

overlap_interval(A1,A2,B1,B2) :-
  MaxStart is max(A1,B1), MinEnd is min(A2,B2), MaxStart < MinEnd.

round_to_2_decimals(Val, Out) :- Out is round(Val*100)/100.
round_to_2_decimals(Val) :- round_to_2_decimals(Val, _).

% aggregate_results: soma totais e prepara lista de totais por veiculo
aggregate_results(Results, Totals, SumTotal, AvgDuration) :-
  aggregate_results(Results, [], 0.0, 0, SumTotal, Totals, AvgDuration).
aggregate_results([], AccT, AccSum, Count, AccSum, RevT, Avg) :- reverse(AccT, RevT), (Count =:= 0 -> Avg = 0 ; Avg is AccSum / Count).
aggregate_results([res(Plate,Break,Total,Duration,Rules)|T], AccT, AccSum, Count, SumOut, TotalsOut, Avg) :-
  append(AccT, [vehicle_summary(Plate,Break,Total,Duration,Rules)], NewAcc),
  AccSum2 is AccSum + Total,
  Count2 is Count + 1,
  aggregate_results(T, NewAcc, AccSum2, Count2, SumOut, TotalsOut, Avg).

% compute_occupancy: usa average duration (min) e capacidade para calcular ocupacao media (simples)
compute_occupancy(AvgDurationMin, OccupancyPerc) :-
  ( obs(capacity(C)) -> true ; default_capacity(C) ),
  % ocupacao media = (tempo medio por veiculo / (24*60)) * 100 / (1/C) ???
  % Simplificamos: ocupacao média por vaga = (AvgDurationMin / (24*60)) * numero_veiculos_media
  % Como não temos numero_veiculos_media, apresentamos ocupacao relativa ao periodo de 24h: percentagem de um dia ocupado por vaga
  OccupancyPerc is round((AvgDurationMin / (24*60)) * 100).

% pretty helper to avoid false negatives
between(A,B,X) :- A =< X, X =< B.

% fallback para caso não hajam registros
meta(_):- fail.

% fim de rules.pl
