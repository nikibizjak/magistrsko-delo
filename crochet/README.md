# Interpreter `crochet`

## Usage

```
Crochet - an interpreter for the STG language

Usage: crochet [--debug-interpreter] [--debug-directory DIRECTORY] INPUT

  Run the STG interpreter.

Available options:
  --debug-interpreter      If set, output each evaluation step to the screen.
  --debug-directory DIRECTORY
                           If set, output each evaluation step to a file
                           'step{index}.html' inside of the DIRECTORY folder.
  INPUT                    STG program input file that will be executed.
  -h,--help                Show this help text
```

### Usage example

```bash
cabal run crochet -- ./examples/map/map.wool --debug-directory ./examples/map/map_steps/
```

### Examples

You can find the examples in the [`examples`](examples) directory.