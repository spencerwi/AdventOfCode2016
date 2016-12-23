all: day1 day2 day3 day4 day5 day6 day7

clean:
	@$(RM) *.byte *.native
	@$(RM) -r _build
	@$(RM) *.cmo *.cmo *.cmi *.cma
	@$(RM) tags

deps:
	@opam install batteries angstrom cryptohash

stdlib: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" AdventStdLib.cma

day%: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" $@.native
	@./$@.native

