# Prevajalnik `crochet`

## Uporaba

```bash
cabal run crochet examples/map.wool
```

## Primeri

### Funkcija map

Implementacija funkcije `map` vzeta neposredno iz originalnega članka z implementacijo STG stroja[[1]](#1).

```stg
nil = CON(Nil)

map = FUN(f xs ->
    case xs of
    | Nil -> nil
    | Cons y ys ->
        let h = THUNK(f y)
            t = THUNK(map f ys)
            r = CON(Cons h t)
        in
            r
)
```

## Literatura

<a id="1">[1]</a> S. Marlow, S. P. Jones, **Making a fast curry: push/enter vs. eval/apply for higher-order languages**, ACM SIGPLAN Notices 39 (9) (2004) 4–15.
