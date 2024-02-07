# calculate_effective_x_bins() works

    Code
      calculate_effective_x_bins(.data = s_curve_noise_umap, x = "UMAP1", cell_area = 0)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(.data = s_curve_noise_umap, x = "UMAP1", cell_area = Inf)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(.data = s_curve_noise_umap, x = "UMAP1", cell_area = -
        3)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(.data = s_curve_noise_umap, x = "UMAP1", cell_area = 1)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! NAs present

---

    Code
      calculate_effective_x_bins(.data = s_curve_noise_umap, x = "UMAP1", cell_area = 1)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Inf present

# calculate_effective_shape_value() works

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_na, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! NAs present

---

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_na, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! NAs present

---

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_na, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! NAs present

---

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_inf, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Inf present

---

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_inf, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Inf present

---

    Code
      calculate_effective_shape_value(.data = s_curve_noise_umap_1, x = "UMAP1", y = "UMAP2")
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Presence one observation only

