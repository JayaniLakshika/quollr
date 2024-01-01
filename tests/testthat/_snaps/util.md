# calculate_effective_x_bins() works

    Code
      calculate_effective_x_bins(data, x, cell_area = 0)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(data, x, cell_area = Inf)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(data, x, cell_area = -3)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Invalid cell area value

---

    Code
      calculate_effective_x_bins(data, x)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! NAs present

---

    Code
      calculate_effective_x_bins(data, x)
    Condition
      Error in `calculate_effective_x_bins()`:
      ! Inf present

# calculate_effective_shape_value() works

    Code
      calculate_effective_shape_value(data, x, y)
    Condition
      Error in `calculate_effective_shape_value()`:
      ! NAs present

---

    Code
      calculate_effective_shape_value(data, x, y)
    Condition
      Error in `calculate_effective_shape_value()`:
      ! NAs present

---

    Code
      calculate_effective_shape_value(data, x, y)
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Inf present

---

    Code
      calculate_effective_shape_value(data, x, y)
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Inf present

---

    Code
      calculate_effective_shape_value(data, x, y)
    Condition
      Error in `calculate_effective_shape_value()`:
      ! Presence one observation only

