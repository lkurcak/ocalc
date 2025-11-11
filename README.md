# ocalc

Basic mathematical operations in Ocaml.

## Building (for development purposes)

Build:
```sh
dune build
```

Build and watch:
```sh
dune build -w
```

## Running

Run:
```sh
dune exec ocalc
```

Run and watch:
```sh
dune exec ocalc -w
```

## Testing

Run only tests whose dependencies have changed:
```sh
dune test
```

Run all tests:
```sh
dune test -f
```
