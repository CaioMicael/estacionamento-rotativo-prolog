Caio Micael (@CaioMicael)
Felipe Macedo (@FelipeMacedoK)

Sistema Especialista - Estacionamento Rotativo

Estrutura:

estacionamento-rotativo-prolog/
  src/
    main.pl      % Menu principal e orquestração do fluxo
    kb.pl        % Base de conhecimento (tarifas, regras, constantes)
    rules.pl     % Motor de inferência e cálculos
    ui.pl        % Interface com usuário e coleta de dados
    explain.pl   % Geração de relatórios e explicações
```

## Pré-requisitos

- SWI-Prolog instalado (https://www.swi-prolog.org/download/stable)
- Windows PowerShell ou Terminal

## Como Executar

1. Clone este repositório:
   ```
   git clone https://github.com/CaioMicael/estacionamento-rotativo-prolog.git
   ```

2. Navegue até a pasta do projeto:
   ```powershell
   cd C:\Users\<seu_usuario>\Documents\estacionamento-rotativo-prolog
   ```

3. Inicie o SWI-Prolog e carregue o sistema:
   ```prolog
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
