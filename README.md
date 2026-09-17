# Worldcup-Predictor
A fully reproducible, Elo‑based prediction engine for international football. This project builds dynamic team ratings from historical match data and uses them to forecast World Cup matches, group outcomes, and knockout brackets. It is designed to be transparent, modifiable, and academically clean — ideal for research, portfolio use, or tournament simulations.


# Project Structure  
This repository contains the following components:

```
.vscode/              # VS Code debugger configuration (R-Debugger)
Elo.R                 # Core Elo rating update logic
former_names.csv      # Mapping of historical team names to modern names
main.R                # Entry point for running predictions
model.R               # Probability model + simulation functions
results.csv           # Full historical match dataset
shootouts.csv         # Penalty shootout outcomes for knockout matches
README.md             # Project documentation
```

# How the Model Works

1. Data Cleaning & Name Normalization  
International football has decades of geopolitical changes, renamings, and federation splits.  
`former_names.csv` ensures that historical team names (e.g., “Czechoslovakia”, “Yugoslavia”) map cleanly to modern equivalents so ratings remain consistent across eras.

2. Elo Rating Engine  
Implemented in `Elo.R`, the model processes matches chronologically and updates ratings using:

- Custom K‑factors for friendlies, qualifiers, continental tournaments, and World Cups  
- Home‑field advantage adjustments  
- Penalty‑shootout logic using `shootouts.csv`  
- Expected score calculations  
- Rating deltas based on match importance  

This produces a continuously updated rating table for all national teams.

3. Prediction Model  
`model.R` uses Elo differences to compute match‑level probabilities:

- Win probability   
- Loss probability  

These probabilities can be used for single‑match predictions or full tournament simulations.

4. Main Script  
`main.R` ties everything together:

- Loads datasets  
- Builds Elo ratings  
- Runs prediction functions  
- Outputs results or prints them to the console  

This is the recommended entry point for running the model.

# Requirements  
- R (4.0+)  
- tidyverse  
- dplyr  
- readr  

# Running Predictions

1. Clone the repository  
2. Open it in VS Code  
3. Use the debugger presets or run `main.R` manually  
4. View predictions in the console


---

If you want, I can also generate a **LICENSE**, a **GitHub project description**, or a **banner/logo** for your repo.
