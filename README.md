# Terminal Hero 🎵🎮

**Terminal Hero** é um jogo de ritmo simples feito em Haskell, inspirado em jogos como Guitar Hero, rodando diretamente no terminal com interface gráfica via `Gloss`.

---

## 📦 Requisitos do Sistema

Certifique-se de estar em um sistema baseado em Unix (como Ubuntu) e ter permissões para instalar pacotes e executar scripts.

---

## 🚀 Instalação

Siga os passos abaixo para configurar o ambiente Haskell e executar o projeto.

### 1. Instalar o GHCup (gerenciador de ferramentas Haskell)

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Siga as instruções no terminal para concluir a instalação.

> **Dica**: Reinicie o terminal após a instalação ou execute `source ~/.ghcup/env` para carregar as variáveis de ambiente.

---

### 2. Instalar ferramentas Haskell essenciais

```bash
ghcup install ghc
ghcup install cabal
ghcup install hls
```

Essas ferramentas incluem:

- `GHC` (compilador Haskell)
- `Cabal` (sistema de build e dependências)
- `HLS` (Haskell Language Server, útil para editores)

---

### 3. Instalar dependências do sistema

Algumas bibliotecas nativas são necessárias para compilar gráficos com o `gloss`.

```bash
sudo apt update
sudo apt install libgmp-dev
sudo apt install libglu1-mesa-dev freeglut3-dev mesa-common-dev
```

---

## 🔧 Compilação do Projeto

Dentro da pasta do projeto `terminal-hero`, execute os comandos abaixo:

```bash
cabal clean     # limpa builds anteriores
cabal build     # compila o projeto
cabal run       # executa o jogo
```

---

## 🕹️ Controles (Exemplo)

> (Dependente de como o jogo foi implementado)

- Teclas: `A`, `S`, `D`, `F` para acertar as notas.
- Pontuação é mostrada no canto da tela.
- Notas caem de cima e devem ser pressionadas no tempo certo.

---

## 🛠️ Problemas Comuns

- **Erro com OpenGL**: Certifique-se de que as bibliotecas `mesa` e `freeglut` estão instaladas corretamente.
- **Comando `ghcup` não encontrado**: Adicione `source ~/.ghcup/env` no seu `.bashrc` ou `.zshrc`.

---

## 📚 Referências

- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal](https://www.haskell.org/cabal/)
- [Gloss](https://hackage.haskell.org/package/gloss)

---

Feito com ❤️ e Haskell.