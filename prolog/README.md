# Prolog – Enunciados inferidos a partir das soluções

Observação: os enunciados abaixo foram inferidos lendo os predicados implementados em cada arquivo.

- `a251primo.pl` — Definir um predicado que verifica se um número N é primo, procurando o primeiro divisor a partir de 2 e sendo verdadeiro apenas quando o primeiro divisor encontrado é o próprio N.
- `a251print.pl` — Definir `imprimir/1` que, dado N (1 ≤ N < 10), imprime N, N-1, …, 1 e encerra a execução ao chegar a 0.
- `a251situacao.pl` — Dadas três notas N1, N2 e N3, calcular a média e classificar o aluno como "aprovado" (média ≥ 7), "reprovado" (média < 4) ou "final" (4 ≤ média < 7).
- `a251soma.pl` — Calcular recursivamente a soma dos inteiros de 1 até N (para N ≥ 0), com caso base soma(0)=0.
- `abs.pl` — Implementar `abs/2` (valor absoluto) e `converte/2` que transforma uma lista de inteiros na lista de seus valores absolutos.
- `binario.pl` — Gerar e imprimir todas as cadeias binárias de 3 dígitos (000 a 111).
- `concat.pl` — Implementar a concatenação de duas listas.
- `familia.pl` — Com base em fatos de parentesco, definir `padrasto/2`: Y é padrasto de X se Y é cônjuge da mãe de X e Y é diferente do pai biológico de X.
- `mapa.pl` — Colorir um mapa com 5 regiões (A–E) usando as cores {azul, verde, amarelo, vermelho} obedecendo restrições de vizinhança, e listar todas as colorações válidas.
- `perfeito.pl` — Verificar se um número N é perfeito somando seus divisores próprios e checando se a soma é igual a N.
- `pertence.pl` — Verificar se um elemento pertence a uma lista.
- `q1.pl` — Verificar se uma lista possui elementos repetidos (duas variantes: `repetidos/1` e `repetidos2/1`).
- `remove.pl` — Remover a primeira ocorrência de um elemento em uma lista.
- `reverte.pl` — Inverter uma lista usando recursão e `append/3`.
- `soma.pl` — Calcular a soma dos inteiros de 1 até N (retorna 0 para N ≤ 0).
- `sublista.pl` — Verificar se X é sublista de Y usando os predicados auxiliares de prefixo e sufixo.
- `tamanho.pl` — Calcular o comprimento de uma lista (número de elementos).
- `tbinario.pl` — Gerar todas as cadeias binárias de 3 dígitos (sem imprimir automaticamente).
- `tdividelista.pl` — Dado um número N e uma lista, dividir a lista em duas: elementos ≤ N e elementos > N, preservando a ordem relativa.
- `teste.pl` — Exemplos de termos com listas aninhadas para testes de unificação.
- `tperfeito.pl` — Verificar se um número é perfeito (duas versões de predicado: `perfeito/1` e `perfect/1`).
- `tpertence.pl` — Verificar se um elemento pertence a uma lista.
- `tremoveall.pl` — Remover todas as ocorrências de um elemento em uma lista (`limpa/3`).
- `treverse.pl` — Inverter uma lista (variação com caso base na lista vazia).
- `tsoma.pl` — Calcular a soma dos inteiros de 1 até X (retorna 0 para X ≤ 0).
- `tstring.pl` — Arquivo reservado/vazio para futuros exercícios de strings.
- `ultimo.pl` — Obter o último elemento de uma lista.
