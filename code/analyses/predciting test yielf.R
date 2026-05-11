# ============================================================
#  HIV — Predict Positive Tests: Poisson vs Negative Binomial
#  Model: test_pos ~ test_total + year  (per country)
#  Data: 15 countries, 2016–2024
#  Prediction year: 2026
# ============================================================

library(MASS)       # glm.nb — load BEFORE tidyverse to avoid masking dplyr::select
library(tidyverse)
library(broom)
library(patchwork)  # combine plots

# ── 1. DATA ──────────────────────────────────────────────────
df <- read_csv("/Users/adenooy/Downloads/tests_totals.csv") %>%
  dplyr::select(Country, year, test_total, test_pos)

predict_year <- 2026

# Planned 2026 test volumes — edit these to match programme targets.
# Defaults below use each country's 2024 value as a placeholder.
planned_tests <- df %>%
  filter(year == max(year)) %>%
  dplyr::select(Country, planned_2026 = test_total)

# ── 2. FIT BOTH MODELS PER COUNTRY ───────────────────────────
models <- df %>%
  group_by(Country) %>%
  nest() %>%
  mutate(
    fit_pois = map(data, ~ glm(
      test_pos ~ test_total + year,
      family = poisson(link = "log"), data = .x
    )),
    fit_nb = map(data, ~ glm.nb(
      test_pos ~ test_total + year, data = .x
    ))
  )

# ── 3. OVERDISPERSION CHECK ───────────────────────────────────
# Dispersion ratio > 1 means overdispersion → NB preferred
overdisp <- models %>%
  mutate(
    dispersion = map_dbl(fit_pois, ~ {
      sum(residuals(.x, type = "pearson")^2) / .x$df.residual
    })
  ) %>%
  dplyr::select(Country, dispersion) %>%
  ungroup()

cat("\n========== OVERDISPERSION (Poisson) ==========\n")
cat("Ratio >> 1 → data overdispersed → prefer Negative Binomial\n\n")
print(as.data.frame(overdisp), digits = 3)

# ── 4. MODEL COMPARISON: AIC ─────────────────────────────────
model_compare <- models %>%
  mutate(
    aic_pois = map_dbl(fit_pois, AIC),
    aic_nb   = map_dbl(fit_nb,   AIC),
    preferred = if_else(aic_nb < aic_pois, "Neg. Binomial", "Poisson")
  ) %>%
  dplyr::select(Country, aic_pois, aic_nb, preferred) %>%
  ungroup()

cat("\n========== AIC COMPARISON ==========\n")
cat("Lower AIC = better fit. Preferred model shown.\n\n")
print(as.data.frame(model_compare), digits = 4)

# ── 5. PREDICTIONS FOR 2026 ───────────────────────────────────
predictions <- models %>%
  left_join(planned_tests, by = "Country") %>%
  left_join(dplyr::select(model_compare, Country, preferred), by = "Country") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pred_pois = round(predict(
      fit_pois,
      newdata = data.frame(test_total = planned_2026, year = predict_year),
      type = "response"
    )),
    pred_nb = round(predict(
      fit_nb,
      newdata = data.frame(test_total = planned_2026, year = predict_year),
      type = "response"
    )),
    pred_final = if_else(preferred == "Neg. Binomial", pred_nb, pred_pois)
  ) %>%
  ungroup() %>%
  dplyr::select(Country, planned_2026, pred_pois, pred_nb, preferred, pred_final)

cat("\n========== PREDICTIONS FOR", predict_year, "==========\n\n")
print(as.data.frame(predictions), digits = 0)

# ── 6. PLOTS ──────────────────────────────────────────────────

# --- 6a. Overdispersion chart ---
p_overdisp <- overdisp %>%
  mutate(Country = fct_reorder(Country, dispersion)) %>%
  ggplot(aes(x = Country, y = dispersion, fill = dispersion > 10)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#e05c5c", "FALSE" = "#5ca8e0"),
                    labels = c("TRUE" = "Overdispersed", "FALSE" = "OK"),
                    name = NULL) +
  annotate("text", x = 0.7, y = 1.3, label = "Threshold = 1",
           size = 3, colour = "grey40", hjust = 0) +
  labs(
    title    = "Overdispersion Check (Poisson Model)",
    subtitle = "Values >> 1 indicate overdispersion — NB model preferred",
    x = NULL, y = "Pearson dispersion ratio"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# --- 6b. AIC comparison ---
p_aic <- model_compare %>%
  pivot_longer(c(aic_pois, aic_nb), names_to = "model", values_to = "AIC") %>%
  mutate(
    model   = recode(model, aic_pois = "Poisson", aic_nb = "Neg. Binomial"),
    Country = fct_reorder(Country, AIC)
  ) %>%
  ggplot(aes(x = Country, y = AIC, colour = model, group = model)) +
  geom_line(colour = "grey80", aes(group = Country)) +
  geom_point(size = 3) +
  coord_flip() +
  scale_colour_manual(values = c("Poisson" = "#5ca8e0", "Neg. Binomial" = "#e05c5c")) +
  labs(
    title    = "Model Fit: AIC by Country",
    subtitle = "Lower AIC = better fit",
    x = NULL, y = "AIC", colour = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# --- 6c. Predicted positives (preferred model) ---
p_pred <- predictions %>%
  mutate(Country = fct_reorder(Country, pred_final)) %>%
  ggplot(aes(x = Country, y = pred_final, fill = preferred)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(pred_final)),
            hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("Poisson" = "#5ca8e0", "Neg. Binomial" = "#e05c5c")) +
  labs(
    title    = paste("Predicted HIV-Positive Results —", predict_year),
    subtitle = "Using preferred model (lower AIC) per country",
    x = NULL, y = "Predicted positives", fill = "Model used"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# --- 6d. Fitted vs actual per country ---
fitted_both <- models %>%
  ungroup() %>%
  mutate(
    fitted_pois = map2(data, fit_pois, ~ mutate(.x, fitted = fitted(.y), model = "Poisson")),
    fitted_nb   = map2(data, fit_nb,   ~ mutate(.x, fitted = fitted(.y), model = "Neg. Binomial"))
  ) %>%
  dplyr::select(Country, fitted_pois, fitted_nb) %>%
  pivot_longer(c(fitted_pois, fitted_nb), values_to = "data_frame") %>%
  unnest(data_frame)

p_fitted <- ggplot(fitted_both, aes(x = year)) +
  geom_point(aes(y = test_pos), colour = "grey30", size = 1.8) +
  geom_line(aes(y = fitted, colour = model), linewidth = 0.9) +
  facet_wrap(~ Country, scales = "free_y", ncol = 5) +
  scale_colour_manual(values = c("Poisson" = "#5ca8e0", "Neg. Binomial" = "#e05c5c")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Fitted vs Actual: Poisson and Negative Binomial",
    subtitle = "Points = observed · Lines = model fit",
    x = "Year", y = "HIV-positive results", colour = "Model"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 8))

# Print all plots
print(p_overdisp)
print(p_aic)
print(p_pred)
print(p_fitted)

