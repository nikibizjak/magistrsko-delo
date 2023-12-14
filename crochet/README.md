# Prevajalnik

```bash
cabal run crochet < examples/example.fj
```

## TODO:

* [x] Dodaj parsanje tipov `data bool = | True | False ;;`
* [ ] Dodaj razčlenjevanje spremenljivk
* [ ] Dodaj preverjanje tipov
* [ ] Dodaj importe
* [ ] Vgradi `bool`, `pair` in `'a list` tipe  v jezik
* [ ] Napiši standardno knjižnico za:
    * [ ] `pair`: `fst`, `snd`
    * [ ] `bool`: `and`, `or`, `not`, `xor`, ...
    * [ ] `'a list`: `head`, `tail`
* [ ] Prevedi `if b then e1 else e2` --> `case b of | True -> e1 | False -> e2`