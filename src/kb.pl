% kb.pl
:- dynamic vehicle_record/5.

block_minutes(15).   % bloco de 15 minutos

% Tarifas por hora
faixa(f1, 60, 6.0).     % 1ª hora: R$6,00/h
faixa(f2, 180, 4.5).    % 2ª-3ª hora: R$4,50/h
faixa(f3, 0, 3.0).      % Demais: R$3,00/h

% Teto diário (por 24h)
daily_cap(30.0).       % R$30 por dia

% Taxas fixas
night_fee(5.0).        % adicional noturno se tiver periodo entre 20:00-06:00
pernoite_fee(10.0).    % taxa de pernoite (quando saída em dia posterior)

% Horarios noturnos (em minutos desde meia-noite)
night_start(20*60).    % 20:00
night_end(6*60).       % 06:00

% Valor da mensalidade
monthly_fee(300.0).    % R$300,00 por mês

% Apenas PNE é isento do pagamento por uso

% Capacidade default (se usuario nao informar)
default_capacity(50).

% Mensagens e limites sao definidas aqui para facilitar ajustes
