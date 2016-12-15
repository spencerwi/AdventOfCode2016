all: day1 day2 day3 day4 day5

clean:
	@$(RM) *.byte *.native
	@$(RM) -r _build
	@$(RM) *.cmo *.cmo *.cmi *.cma

deps:
	@opam install batteries

day1: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day1.native
	@./day1.native

day2: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day2.native
	@./day2.native

day3: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day3.native
	@./day3.native

day4: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day4.native
	@./day4.native

day5: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day5.native 
	@./day5.native

day6: clean deps
	@ocamlbuild -quiet -use-ocamlfind -cflags "-w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60" day6.native 
	@./day6.native

