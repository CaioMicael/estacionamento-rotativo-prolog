% rules.pl
% Regras principais de cálculo e resumo

:- dynamic obs/1.
:- dynamic fired/1.
:- dynamic vehicle_record/5.
:- discontiguous meta/1.

% Meta: calcula os resultados gerais
meta(summary(Totals, SumTotal, OccupancyMean)) :-
  findall(V, vehicle_record(V,_,_,_,_), Plates),
  Plates \= [],
  maplist(calc_vehicle, Plates, Results),
  aggregate_results(Results, Totals, SumTotal, AvgDur),
  compute_occupancy(AvgDur, OccupancyMean),
  SumTotal >= 0.

meta(_) :- fail.

% Cálculo por veículo
calc_vehicle(Plate, res(Plate, Breakdown, Total, Duration, FiredRules)) :-
  retractall(fired(_)),
  vehicle_record(Plate, Entry, Exit, Mensalista, PNE),
  compute_duration(Entry, Exit, Duration),
  ( PNE == sim ->
      Total = 0.0,
      Breakdown = [exempt(pne)],
      assertz(fired(exemption_pne))
  ; Mensalista == sim ->
      monthly_fee(Fee),
      Total = Fee,
      Breakdown = [monthly(Fee)],
      assertz(fired(monthly_subscriber))
  ; block_minutes(B),
    round_up(Duration, B, Rounded),
    assertz(fired(rounding_block(Rounded,B))),
    compute_cost_by_bands(Rounded, Breakdown, Partial),
    apply_daily_cap(Rounded, Partial, CostAfterCap),
    apply_night_pernoite(Entry, Exit, CostAfterCap, Total)
  ),
  findall(R, fired(R), FiredRules).

compute_duration(Entry, Exit, D) :- D is max(0, Exit - Entry).
round_up(D, Block, R) :- R is ((D + Block - 1) // Block) * Block.

% Cálculo por faixas
compute_cost_by_bands(Duration, Breakdown, Cost) :-
  findall(f(F,L,RH), faixa(F,L,RH), F0),
  sort_faixas(F0, F),
  allocate_minutes(F, Duration, A),
  compute_cost_alloc(A, Breakdown, Cost).

sort_faixas(F0, F) :-
  map_list_to_pairs(faixa_sort_key, F0, P),
  keysort(P, S),
  pairs_values(S, F).

faixa_sort_key(f(_,L,_), K) :- (L =:= 0 -> K = 999999 ; K = L).

allocate_minutes([], _, []).
allocate_minutes([f(N,L,R)|T], Rem, [alloc(N,U,L,R)|A]) :-
  (L =:= 0 -> U = Rem ; U is min(Rem, L)),
  Rem2 is max(0, Rem - U),
  allocate_minutes(T, Rem2, A).

compute_cost_alloc(Alloc, Breakdown, Cost) :-
  compute_cost_alloc(Alloc, 0.0, [], Cost, Breakdown).

compute_cost_alloc([], Acc, AccB, Acc, AccB).
compute_cost_alloc([alloc(N,U,_,R)|T], Acc, B, CostOut, Breakdown) :-
  C is U * (R / 60.0),
  append(B, [band(N,U,R,C)], NB),
  Acc2 is Acc + C,
  compute_cost_alloc(T, Acc2, NB, CostOut, Breakdown).

% Limites e taxas adicionais
apply_daily_cap(Duration, CostIn, CostOut) :-
  daily_cap(Cap),
  Days is ceiling(Duration / (24*60)),
  Max is Cap * Days,
  ( CostIn > Max ->
      CostOut = Max, assertz(fired(daily_cap_applied(Max)))
  ; CostOut = CostIn ).

apply_night_pernoite(Entry, Exit, CostIn, CostOut) :-
  ( pernoite(Entry,Exit) ->
      pernoite_fee(F), C1 is CostIn + F, assertz(fired(pernoite_fee(F)))
  ; C1 = CostIn ),
  ( night_period_overlap(Entry,Exit) ->
      night_fee(NF), C2 is C1 + NF, assertz(fired(night_fee(NF)))
  ; C2 = C1 ),
  round_to_2_decimals(C2, CostOut).

pernoite(E, S) :- E // (24*60) < S // (24*60).

night_period_overlap(E, S) :-
  number(E), number(S),
  night_start(NS), night_end(NE),
  EMin is E mod (24*60),
  SMin is S mod (24*60),
  ( (NE =< NS, (EMin >= NS ; SMin =< NE))
  ; (NE > NS, EMin < NE, SMin > NS) ).

overlap_interval(A1,A2,B1,B2) :-
  number(A1), number(A2), number(B1), number(B2),
  max(A1,B1,Start), min(A2,B2,End), Start < End.

round_to_2_decimals(V, O) :- O is round(V*100)/100.

% Agregação e ocupação
aggregate_results(R, T, S, A) :- aggregate_results(R, [], 0.0, 0, S, T, A).
aggregate_results([], Acc, Sum, Count, Sum, Rev, Avg) :-
  reverse(Acc, Rev),
  (Count =:= 0 -> Avg = 0 ; Avg is Sum / Count).
aggregate_results([res(P,B,T,D,R)|L], Acc, Sum, Count, SOut, Tout, Avg) :-
  append(Acc, [vehicle_summary(P,B,T,D,R)], NAcc),
  Sum2 is Sum + T,
  Count2 is Count + 1,
  aggregate_results(L, NAcc, Sum2, Count2, SOut, Tout, Avg).

compute_occupancy(AvgMin, Occ) :-
  ( obs(capacity(C)) -> true ; default_capacity(C) ),
  Occ is round((AvgMin / (24*60)) * 100).

between(A,B,X) :- A =< X, X =< B.
max(A,B,A) :- A >= B.
max(A,B,B) :- B > A. 