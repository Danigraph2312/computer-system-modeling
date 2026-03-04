# Computer Systems Modelling — R & JMT

![R](https://img.shields.io/badge/R-statistical_analysis-276DC3?style=flat&logo=r&logoColor=white)
![JMT](https://img.shields.io/badge/JMT-queueing_simulation-orange?style=flat)
![Grade](https://img.shields.io/badge/Grade-29%2F30-brightgreen?style=flat)
![University](https://img.shields.io/badge/Università_Vanvitelli-BSc_Data_Analytics-darkred?style=flat)

**Course:** Computer Systems Modelling and Semantic Web  
**Institution:** Università degli Studi della Campania 'Luigi Vanvitelli'  
**Degree:** BSc in Data Analytics · 2024–2025  
**Grade:** 29/30

---

## Overview

A full performance modelling study of an **ARM big.LITTLE heterogeneous processor architecture**, combining statistical workload characterisation from real execution traces with closed queueing network simulation. The project is split into two parts: trace analysis in **R** and system simulation in **JMT (Java Modelling Tools)**.

---

## Part 1 — Workload Analysis & Distribution Fitting (R)

Four execution trace files (`n = 50,000` samples each) record task completion times on two core types under two workload classes in the ARM big.LITTLE architecture:

| Trace | Core type | Workload class |
|---|---|---|
| `traceB-HH.txt` | High Performance | Heavy computation |
| `traceB-HE.txt` | Energy Efficient | Heavy computation |
| `traceB-LH.txt` | High Performance | Low computation |
| `traceB-LE.txt` | Energy Efficient | Low computation |

### Statistical Summary

| Trace | Mean (s) | Std Dev | CV | Skewness | Kurtosis |
|---|---|---|---|---|---|
| traceB-HH | 24.13 | 35.20 | 1.459 | 4.011 | 23.78 |
| traceB-HE | 96.56 | 141.78 | 1.468 | 4.068 | 25.14 |
| traceB-LH | 1.50 | 0.433 | 0.288 | 0.593 | 0.533 |
| traceB-LE | 4.80 | 1.383 | 0.288 | 0.569 | 0.439 |

The coefficient of variation (CV) is used as the primary criterion for distribution family selection:

- **CV > 1** (traceB-HH, traceB-HE): high variance relative to mean, leptokurtic shape → candidate distributions: **Hyper-Exponential**, Hyper-Erlang, Gamma
- **CV < 1** (traceB-LH, traceB-LE): low variance, near-symmetric shape → candidate distributions: **Hypo-Exponential**, Erlang, Gamma, Normal, Weibull

### Distribution Fitting Methodology

Parameter estimation via **Maximum Likelihood Estimation (MLE)** and **Method of Moments**, solved numerically where closed-form solutions are unavailable (e.g. Hyper-Exponential requires numerical optimisation of the likelihood gradient).

**Fitted parameters:**

| Trace | Best-fit distribution | Key parameters |
|---|---|---|
| traceB-HH | Hyper-Exponential | λ₁=0.01656, λ₂=0.06634, p=0.1997 |
| traceB-HE | Hyper-Exponential | λ₁=0.00409, λ₂=0.01662, p=0.1974 |
| traceB-LH | Gamma / Erlang | shape=12.02, scale=0.125 / k=12, rate=7.998 |
| traceB-LE | Gamma / Erlang | shape=12.26, scale=0.395 / k=12, rate=2.512 |

### Visualisation Pipeline

Three-stage goodness-of-fit assessment for each trace × distribution combination:

1. **PDF overlay** — empirical kernel density vs theoretical PDF for all candidate distributions on a single plot (colour-coded)
2. **CDF overlay** — empirical ECDF (step function) vs theoretical CDF
3. **Q-Q plots** — all candidate distributions overlaid on a single Q-Q plot for direct visual comparison; identity line as reference

All plots produced with `ggplot2` and assembled into composite grids with `patchwork`.

### R Dependencies

```r
library(psych)          # descriptive statistics (describe())
library(ggplot2)        # visualisation
library(patchwork)      # plot composition
library(distributions3) # Erlang distribution (dErlang, pErlang)
library(Distributacalcul) # extended distribution support
library(tidyr)          # pivot_longer for multi-distribution overlays
```

---

## Part 2 — Closed Queueing Network Simulation (JMT)

The ARM big.LITTLE system is modelled as a **closed multiclass queueing network** simulated in JMT (Java Modelling Tools). The model captures two competing workload classes sharing five service stations.

### Workload Classes

| Class | Type | Population | Reference station |
|---|---|---|---|
| Heavy computation tasks | Closed | 20 customers | Storage |
| Low computation tasks | Closed | 32 customers | Storage |

### Network Topology

Five stations, each modelled as a queue + server + probabilistic router:

| Station | Server type | Notes |
|---|---|---|
| **Storage** | PS (Processor Sharing) | Reference source for both classes |
| **I/O** | PS (Processor Sharing) | Shared I/O subsystem |
| **Network** | PS (Processor Sharing) | Interconnect/bus |
| **High Performance Cores** | FCFS | big cores — lower service time for heavy tasks |
| **Energy Efficient Cores** | FCFS | LITTLE cores — fitted trace distributions as service times |

Service time distributions at the CPU stations are parameterised using the fitted distributions from Part 1 (Hyper-Exponential for heavy tasks, Gamma/Erlang for light tasks). I/O and Network stations use Exponential service times.

### Performance Metrics Collected

Per station × per class: **Throughput**, **Utilization**, **Number of Customers**, **Residence Time**, **Response Time**.  
System-level: **System Throughput** and **System Response Time** for each class.

Simulation configured with `maxSamples=1,000,000` and confidence interval parameters `α=0.01`, `precision=0.03`.

### Running the Simulation

Open `PROJECT.jsimg` in **JMT** (Java Modelling Tools, free download at [jmt.sourceforge.net](http://jmt.sourceforge.net)) and run the simulation directly. No installation of additional dependencies required beyond JMT itself.

---

## Repository Structure

```
computer-system-modeling/
├── TRACES_analysis.R       # Full R analysis: stats, distribution fitting, PDF/CDF/QQ plots
├── PROJECT.jsimg           # JMT closed queueing network model (XML)
├── PROJECT.pdf             # Full written report with derivations and results
└── README.md
```

---

## Stack

| Tool | Purpose |
|---|---|
| R | Statistical analysis, distribution fitting, visualisation |
| JMT (Java Modelling Tools) | Closed queueing network simulation |
| ggplot2 / patchwork | PDF, CDF, Q-Q plot generation |
