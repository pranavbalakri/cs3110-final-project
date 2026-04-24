# INSTALL

Minimum steps for graders (macOS/Linux with OPAM already installed):

```bash
# from the project root
opam update
opam switch create cs3110-final-project 5.1.1 -y
eval $(opam env)
opam install dune raylib ounit2 -y

dune build
dune runtest
```

If the switch already exists, use:

```bash
opam switch cs3110-final-project
eval $(opam env)
```
