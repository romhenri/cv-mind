# <p align="center"> **CV Mind: A Prolog Resume Recommender 🧠** </p>

<a id="overview"></a>

##⭐ Overview

**CV Mind** was designed to help recruiters filter and recommend the most qualified candidates for job vacancies, based on Course, CRA, Area, Skills and Exp Time.

---

<a id="functionality"></a>

## 📋 How To Use

To use **CV Mind**, you need a Prolog interpreter like **SWI-Prolog** installed. Steps:

```bash
swipl
```

```bash
[main].
```

```bash
menu.
```

CLI Options:

- Add a new job opportunity
- List available job openings
- List all registered candidates
- Filter candidates who meet the requirements for a specific job
- Recommend the top candidates based on a customized scoring system
- Exit the system

> Filter excludes who does not have minimum requirements.

> Scoring system is based on the number of skills and experience time.

---

<a id="files"></a>

## 🗂 Files

- `main.pl`: the main entry point. Handles the menu and general system flow.
- `data/candidates.pl`: contains mock candidates.
- `data/jobs.pl`: contains job offers.
- `services/filter.pl`: defines the logic for filtering qualified candidates.
- `services/score.pl`: implements the scoring system for ranking candidates.
- `util/io.pl`: handle user input and output formatting.
- `util/util.pl`: utility functions used across modules.

---

<a id="tech-used"></a>

## 🌐 Stack

<a href="https://www.swi-prolog.org/"><img src="https://img.shields.io/badge/PROLOG-9e193d?style=for-the-badge" alt="PROLOG" /></a>
