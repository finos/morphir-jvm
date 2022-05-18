# Morphir JVM binding

This repo contains tools to work with the Morphir IR on the JVM. It will include data models for the IR, JSON serialization and developer tooling.

# For Linux
## How to build and test

Morphir-jvm use [mill]("http://www.lihaoyi.com/mill/) as its build tool.

### IntelliJ Setup

If you are using IntelliJ IDEA to edit morphir-jvm's Scala code, you can create the
IntelliJ project files via:

```bash
./mill mill.scalalib.GenIdea/idea
```

### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```bash
./mill mill.contrib.Bloop/install
```

### Run Tests

```bash
./mill __.test
```

or in watch mode:

```bash
./mill -w __.test
```

### Formatting Code

Code needs to be formatted according to `scalafmt` rules. To run `scalafmt` on all the source code using:

```bash
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

or in watch mode to reformat changed files:

```bash
./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```


# For Windows
## How to build and test

Morphir-jvm use [mill]("http://www.lihaoyi.com/mill/) as its build tool.

### IntelliJ Setup

If you are using IntelliJ IDEA to edit morphir-jvm's Scala code, you can create the
IntelliJ project files via:

```bash
.\mill mill.scalalib.GenIdea/idea
```

### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```bash
.\mill mill.contrib.Bloop/install
```

### Run Tests

```bash
.\mill __.test
```

or in watch mode:

```bash
.\mill -w __.test
```

### Formatting Code

Code needs to be formatted according to `scalafmt` rules. To run `scalafmt` on all the source code using:

```bash
.\mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

or in watch mode to reformat changed files:

```bash
.\mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```
