
# 🐦 Galápagos: Multi-Agent Prisoner's Dilemma Simulation

This repository contains my solution for the **Advanced Programming 2025** exam — *Galápagos*.  
It implements a **multi-agent, multi-round Prisoner’s Dilemma** simulation using **cellular automatons** and an **extended APL interpreter**.

---

## 📘 Overview

The Galápagos simulation models finches on a grid, each following a behavioural strategy defined in an extended version of **APL**.  
Finches interact, reproduce, age, and die based on both deterministic rules and randomised events, showcasing emergent cooperative or selfish behaviour.

The world evolves according to **block-cellular automata** with the **Margolus neighbourhood scheme**, allowing local interactions and potential concurrency.

---

## 🧩 Tasks Implemented

| Task | Module | Description |
|------|---------|-------------|
| **A** | `APL/Eval.hs` | Implemented set data type and operations: `union`, `without`, `elem`, `{}` notation. |
| **B** | `APL/Parser.hs` | Extended the APL parser for set syntax, removed left recursion, and applied operator precedence rules. |
| **C** | `BlockAutomaton/Server.hs` | Implemented a concurrent block-cellular automaton server with process-based cells and message passing. |
| **D** | `Galapagos/Rules.hs` | Defined finch–finch interaction (`meet`) based on strategy effects (`meet`, `ignore`, `groom`). |
| **E** | `Galapagos/Parser.hs` | Built parser for `.rules` configuration files including parameters and strategies. |
| **F** | `Galapagos/Rules.hs` | Combined all modules into a functional Galápagos simulation (`initial`, `reproduce`, `groom`, `age`, `kill`). |

---

---

## 🧠 How to Build and Run

### 🔹 Run Tests
```bash
cabal test
````

### 🔹 Run Visualisation

```bash
cabal run -- viewer example.rules
```

This visualises the simulation of finch interactions on the grid.

---

## 🧪 Example Strategies

Example strategies are defined in APL-style syntax inside `.rules` files, e.g.:

```apl
strategy samaritan #ff0000 {
  loop x = 0 for i < 100 do
  let unused = meet in groom
}
```

Other example strategies include `cheater`, `student`, `flipflop`, and `grudger`.

---

## 🧾 Report

A short report accompanies this code submission, following the structure specified in the exam:

* Discussion of ambiguities and design decisions
* Final grammar
* Concurrency, message-passing, and race-condition analysis
* Testing methodology and evaluation of functionality

