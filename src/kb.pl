% kb.pl
% Base de conhecimento: faixas, tarifas e constantes do dominio

:- dynamic vehicle_record/5.

% Unidades: minutos
block_minutes(15).   % arredondamento por blocos de 15 minutos

% Tarifas (por hora) aplicadas por faixa
% faixa(Id, LimiteMinutos, RatePerHour).
% LimiteMinutos = 0 significa sem limite superior (restante)
faixa(f1, 60, 6.0).     % até 60min -> R$6.00/h
faixa(f2, 180, 4.5).    % 61-180min -> R$4.50/h
faixa(f3, 0, 3.0).      % >180min -> R$3.00/h

% Teto diário (por 24h)
daily_cap(30.0).       % R$30 por dia

% Taxas fixas
night_fee(5.0).        % adicional noturno se tiver periodo entre 20:00-06:00
pernoite_fee(10.0).    % taxa de pernoite (quando saída em dia posterior)

% Horarios noturnos (em minutos desde meia-noite)
night_start(20*60).    % 20:00
night_end(6*60).       % 06:00

% Isencoes: mensalistas e PNE sao isentos do pagamento por uso

% Capacidade default (se usuario nao informar)
default_capacity(50).

% Mensagens e limites sao definidas aqui para facilitar ajustes
