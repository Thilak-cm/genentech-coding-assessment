# descriptiveStats

An R package providing essential descriptive statistics functions for calculating central tendency and dispersion measures.

## Overview

The `descriptiveStats` package implements a collection of functions for computing common descriptive statistics including measures of central tendency (mean, median, mode) and measures of dispersion (quartiles, interquartile range). All functions include robust error handling and support for missing values.

## Installation

To install this package from source, use:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install descriptiveStats
devtools::install("question_1_descriptive_stats/descriptiveStats")

# Load the package
library(descriptiveStats)
```

## Functions

### Central Tendency Measures

#### `calc_mean(x)`
Calculates the arithmetic mean of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing the mean

**Example:**
```r
calc_mean(c(1, 2, 3, 4, 5))  # Returns 3
calc_mean(c(1, NA, 3, 4, 5))  # Returns 3.25 (NA removed)
```

#### `calc_median(x)`
Calculates the median of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing the median

**Example:**
```r
calc_median(c(1, 2, 3, 4, 5))  # Returns 3
calc_median(c(1, 2, 3, 4))     # Returns 2.5
```

#### `calc_mode(x)`
Calculates the mode (most frequently occurring value) of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing the mode, or `NA` if no mode exists (all values are unique)

**Note:** In case of ties (multiple values with the same maximum frequency), returns the first mode encountered.

**Example:**
```r
calc_mode(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))  # Returns 5
calc_mode(c(1, 2, 3, 4, 5))                  # Returns NA (no mode)
calc_mode(c(1, 2, 2, 3, 3))                  # Returns 2 (first mode in tie)
```

### Dispersion Measures

#### `calc_q1(x)`
Calculates the first quartile (25th percentile) of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing Q1

**Example:**
```r
calc_q1(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))  # Returns 3.25
calc_q1(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))  # Returns 2.5
```

#### `calc_q3(x)`
Calculates the third quartile (75th percentile) of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing Q3

**Example:**
```r
calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))  # Returns 7.75
calc_q3(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))  # Returns 5.5
```

#### `calc_iqr(x)`
Calculates the Interquartile Range (IQR = Q3 - Q1) of a numeric vector.

**Parameters:**
- `x`: A numeric vector

**Returns:** A numeric scalar representing the IQR

**Example:**
```r
calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))  # Returns 4.5
calc_iqr(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))  # Returns 3
```

## Complete Example

```r
# Load the package
library(descriptiveStats)

# Example dataset
data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)

# Calculate all statistics
calc_mean(data)   # 3.3
calc_median(data) # 4.5
calc_mode(data)   # 5
calc_q1(data)     # 2.5
calc_q3(data)     # 5.5
calc_iqr(data)    # 3
```

## Error Handling

All functions include input validation:

- **Non-numeric input:** Functions will stop with an error message: `"Input must be a numeric vector."`
- **Empty vectors:** Functions will stop with an error message: `"Input vector must not be empty."`
- **Missing values:** All functions handle `NA` values by removing them before calculation (`na.rm = TRUE`)

## Package Structure

```
descriptiveStats/
├── DESCRIPTION      # Package metadata
├── NAMESPACE        # Exported functions
├── R/               # Source code
│   ├── calc_mean.R
│   ├── calc_median.R
│   ├── calc_mode.R
│   ├── calc_q1.R
│   ├── calc_q3.R
│   └── calc_iqr.R
├── man/             # Documentation (generated from Roxygen2)
└── README.md        # This file
```

## Documentation

Full function documentation is available using R's help system:

```r
?calc_mean
?calc_median
?calc_mode
# etc.
```

## Author

Thilak Mohan

## License

See DESCRIPTION file for license information.
