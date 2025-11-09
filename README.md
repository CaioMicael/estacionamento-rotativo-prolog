Caio Micael (@CaioMicael)
Felipe Macedo (@FelipeMacedoK)

Sistema Especialista - Estacionamento Rotativo

Estrutura:

estacionamento-rotativo-prolog/
  src/
    main.pl      % menu + orquestração do fluxo
    kb.pl        % fatos, tabelas e domínios
    rules.pl     % regras e meta/1 (resultado principal)
    ui.pl        % perguntas e assert das observações
    explain.pl   % impressão da trilha das regras

Como executar (Windows + SWI-Prolog):

1. Abra um terminal PowerShell e entre na pasta do projeto (onde está este README):

   cd C:\Users\<seu_usuario>\Documents\estacionamento-rotativo-prolog

2. Inicie o SWI-Prolog e carregue o arquivo main:

   swipl
   ?- ['src/main.pl'].
   ?- start.

Observacoes:
- Ao inserir tempos, informe dia como inteiro (ex: 0, 1) e hora no formato HH:MM.
  Ex.: entrada dia 0 hora 08:30 -> dia=0, hora=08:30.
- Mensalistas e PNE sao isentos do pagamento por uso.
- Regras aplicadas sao listadas ao final para explicar o raciocinio.

Regras implementadas (exemplos):
- Arredondamento por blocos de 15 minutos.
- Cálculo por faixas de tarifa (3 faixas implementadas).
- Teto diário aplicado por cada dia completo.
- Adicional noturno quando houver sobreposição com 20:00-06:00.
- Taxa de pernoite quando a saída ocorre em dia posterior.
- Isenção para mensalistas e PNE.
- Breakdown por faixa e total por veículo.
- Estimativa simplificada de ocupação média.

Limitações e próximos passos:
- Parsing de datas/horários pode ser melhorado usando bibliotecas de datas.
- Cálculo de ocupação é simplificado; poderia usar janela de analise e capacidade para estimativa mais precisa.
- Validacoes de entrada são básicas; melhorar robustez de leitura.

Desenvolvido por: Caio Krieger e Felipe Macedo
