# PureAgent

> Agent framework powered by Monads, driven by logic

*The project is at the initial phase, and is actively developed. Any contribution is welcomed.*

## Overview

PureAgent is a Haskell-based AI agent framework designed for building
intelligent, reliable agents with clean functional architecture.

## Features

- Functional AI agents with **stateful reasoning**
- Modular architecture for **easy extension**
- Simple integration with **external tools and APIs**
- Clean, Haskell-native **monadic design**

## Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) >= 9.2
- [Cabal](https://www.haskell.org/cabal/) >= 3.6

### Installation

`Main.hs` implements an ReAct agent with a dummy `getWeather` registered as a tool
(the function always return "it's always sunny"). To run the agent, you need to export
your `OPENAI_API_KEY` to the enviroment.
```bash
git clone https://github.com/yourusername/PureAgent.git
cd PureAgent
cabal build
export OPENAI_API_KEY=<YOUR_KEY>
cabal run
