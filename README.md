# Tabula

A powerful, cross-platform data manipulation and analysis library for Scala 3. Tabula provides a pandas-like API for working with tabular data, supporting everything from basic data loading to advanced statistical operations.

## Features

- **Cross-platform**: Runs on JVM, JavaScript (Node.js), and Scala Native
- **Multiple data sources**: CSV files, tab-separated files, PostgreSQL databases, and in-memory data
- **Type-safe operations**: Automatic type inference with support for integers, floats, strings, booleans, and timestamps
- **Statistical analysis**: Built-in descriptive statistics, z-scores, percentiles, and more
- **Data manipulation**: Filtering, sorting, sampling, splitting, and reshaping operations
- **Performance optimized**: Efficient operations on large datasets

## Installation

Add tabula to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "tabula" % "0.0.1"
```

For cross-platform projects, tabula supports:
- **JVM**: Full feature set including PostgreSQL connectivity
- **Scala.js**: Core data operations for browser/Node.js environments
- **Scala Native**: High-performance compiled operations

## Quick Start

### Creating Datasets

```scala
import io.github.edadma.tabula.*

// From CSV file
val iris = Dataset.fromCSVFile("iris.csv")

// From in-memory data
val sales = Dataset(
  Seq("product", "revenue", "units"),
  Seq(
    Seq("Widget A", 1250.50, 45),
    Seq("Widget B", 890.25, 32),
    Seq("Widget C", 2100.75, 67)
  )
)

// From a Map
val temperatures = Dataset(Map(
  "city" -> Seq("New York", "London", "Tokyo"),
  "temp_c" -> Seq(22.5, 18.2, 25.1),
  "humidity" -> Seq(65, 78, 82)
))
```

### Basic Operations

```scala
// Display dataset info
iris.info()
// <class io.github.edadma.tabula.Dataset>
// 150 rows; 5 columns
// #  Column        Non-Null Count  Datatype
// 0  sepal_length  150             float
// 1  sepal_width   150             float
// 2  petal_length  150             float
// 3  petal_width   150             float
// 4  species       150             string

// View first/last rows
iris.head()     // First 5 rows
iris.tail(10)   // Last 10 rows

// Get descriptive statistics
iris.describe.print()
//       count  mean   std    min    25%    50%    75%    max
// sepal_length  150.0  5.8433  0.8281  4.3000  5.1000  5.8000  6.4000  7.9000
// sepal_width   150.0  3.0573  0.4359  2.0000  2.8000  3.0000  3.3000  4.4000
// ...

// Access columns
val sepalLength = iris("sepal_length")
val species = iris.species  // Dynamic column access
```

### Data Filtering and Selection

```scala
// Filter by conditions
val largeSepals = iris(iris("sepal_length") > 6.0)
val setosa = iris(iris("species") == "setosa")

// Complex filtering
val filtered = iris(
  (iris("sepal_length") > 5.0) && 
  (iris("petal_width") < 2.0)
)

// Select specific rows by index
val firstTen = iris.iloc(0 to 9)
val sample = iris.sample(20)  // Random sample of 20 rows
```

### Statistical Operations

```scala
// Column statistics
iris("sepal_length").mean    // Mean value
iris("sepal_length").std     // Standard deviation
iris("sepal_length").min     // Minimum value
iris("sepal_length").max     // Maximum value

// Z-score normalization
val normalized = iris.zcode

// Remove outliers (keep values within 3 standard deviations)
val cleaned = iris((iris.zcode.abs < 3).all)
```

### Data Manipulation

```scala
// Sort by column
val sorted = iris.sort("sepal_length")

// Add new columns
val enhanced = iris.append(
  (iris("sepal_length") * iris("sepal_width")).rename("sepal_area")
)

// Drop columns
val reduced = iris.drop("sepal_width", "petal_width")

// Sample and split data
val shuffled = iris.shuffle
val Array(train, test) = iris.split(80, 20)  // 80% train, 20% test
```

### Working with Time Series

```scala
// Load timestamp data
val timeseries = Dataset(
  Seq("timestamp", "value"),
  Seq(
    Seq("2023-01-01 09:00:00", 100.5),
    Seq("2023-01-01 10:00:00", 102.3),
    Seq("2023-01-01 11:00:00", 98.7)
  )
)

// Time-based operations
val recent = timeseries(timeseries("timestamp") > Instant.parse("2023-01-01T09:30:00Z"))
```

## Advanced Features

### PostgreSQL Integration (JVM only)

```scala
import io.github.edadma.tabula.PG

val connection = PG.connect("localhost", "mydb", "user", "password", 5432)
val results = PG.query(
  "SELECT * FROM sales WHERE revenue > 1000", 
  connection
)
```

### Statistical Analysis

```scala
// Correlation and regression metrics
val predictions = model.predict(testData)
val error = testData.error("predictions", "actual")
val r2Score = testData.r2("predictions", "actual")
val adjustedR2 = testData.adjustedR2("predictions", "actual")

// Value counts and frequency analysis
val speciesCounts = iris.counts("species")
val normalized = iris.countsNormalize("species")
```

### Custom Data Types

Tabula automatically infers column types but also supports explicit type specification:

```scala
import io.github.edadma.tabula.Type.*

val dataset = Dataset(
  Seq("id", "score", "active", "created"),
  data,
  types = Seq(IntType, FloatType, BoolType, TimestampType)
)
```

## Performance Tips

- Use `sample()` for working with large datasets during development
- Leverage `iloc()` for efficient row selection by index
- Consider splitting large datasets with `split()` for parallel processing
- Use `shuffle()` before splitting for better data distribution

## API Reference

### Dataset Creation
- `Dataset.fromCSVFile(path)` - Load from CSV file
- `Dataset.fromTabFile(table, path)` - Load from tab-separated file
- `Dataset(columns, data)` - Create from sequences
- `Dataset(map)` - Create from Map of column name to values

### Data Access
- `dataset(columnName)` - Select column
- `dataset.iloc(indices)` - Select rows by position
- `dataset.loc(labels)` - Select rows by label
- `dataset(booleanMask)` - Filter rows by boolean condition

### Statistical Operations
- `dataset.describe` - Descriptive statistics
- `dataset.mean`, `dataset.std`, `dataset.min`, `dataset.max`
- `dataset.zcode` - Z-score normalization
- `dataset.count()` - Count non-null values

### Data Manipulation
- `dataset.sort(column)` - Sort by column
- `dataset.drop(columns...)` - Remove columns
- `dataset.append(other)` - Add columns from another dataset
- `dataset.sample(n)` - Random sample
- `dataset.split(percentages...)` - Split into multiple datasets

## License

This project is licensed under the ISC License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Changelog

### 0.0.1
- Initial release
- Cross-platform support (JVM, JS, Native)
- Core data manipulation operations
- Statistical analysis functions
- CSV and PostgreSQL data sources