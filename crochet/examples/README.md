# Primeri

Programe lahko s pomočjo simulatorja izvedemo s spodnjim ukazom.

```bash
cabal run crochet -- <INPUT_FILE> --debug-directory <DEBUG_DIRECTORY>

# Izvedemo program 'arithmetic_operations.wool' in korake simulacije shranimo v mapo 'arithmetic_operations_steps'
cabal run crochet -- ./examples/arithmetic_operations/arithmetic_operations.wool --debug-directory ./examples/arithmetic_operations/arithmetic_operations_steps/
```

## Kazalo

| Primer | Kratek opis |
| ------ | ----------- |
| [`arithmetic_operations/arithmetic_operations.wool`](./arithmetic_operations/arithmetic_operations.wool) | Izračuna vrednost izraza `(6 * 2) + (8 / 2)` |
