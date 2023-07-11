# 🎨 Confint Visualisations

**A collection of data visualisation ideas implemented in R — free to use and adapt.**

During my career as a performance engineer I had to visualise the data in a way that helps to analyse and present it. And I must admit sometimes this turned to be quite tricky.

This project is more ofa a **set of concepts** rather than a code for visualising performance and statistical data in intuitive ways. Nevertheless, I provide code written in **R** language so you can reuse the ideas in your project.

Each **R** script represents a unique visualisation idea. And I will be adding more.

Note, that **R** is just an instrument — you can reproduce the same ideas in Python, JavaScript, or any other tool you'd like.

---

## 🧭 Overview

Each file in this repository corresponds to one visualisation concept:

| Script                                                                                                     | Purpose                                                    | Example                              |
|------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|--------------------------------------|
| [and_candles_for_all.R](https://github.com/yourusername/confint-visualisations/wiki/and_candles_for_all.R) | Percentile candles reflecting outliers and pass/fail ratio | ![](images/clipboard-3874127004.png) |
| [distributions.R](https://github.com/yourusername/confint-visualisations/wiki/distributions.R)             | Enhanced distribution diagrams with SLA comparison         | ![](images/distrib_single.png)       |

👉 Explore the full documentation in the **[Wiki](https://github.com/yourusername/confint-visualisations/wiki)**.

---

## 🚀 Quick Start

1. Clone this repository  
   ```bash
   git clone https://github.com/yourusername/confint-visualisations.git
   ```

2. Open R and source any script you want to explore:  
   ```r
   source("and_candles_for_all.R")
   generate_candles_chart(your_data, "Login")
   ```

3. Or use built-in data generators provided in each script.

---

## 🧠 Concept

These visualisations aim to:
- Make statistical characteristics **visually intuitive**
- Enhance classic plots (boxplots, histograms) with **context and comparability**
- Support **performance testing and SLA analysis**

---

## 🤝 Contributing

Ideas, improvements, and additional visualisation types are welcome!  
Feel free to fork and submit a pull request, open an issue with your proposal or ask the question.

---

## 📄 License

This project is released under the MIT License — free for any use.