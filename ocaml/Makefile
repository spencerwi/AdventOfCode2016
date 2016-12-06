all: day1 day2 day3

clean:
	@$(RM) *.byte *.native
	@$(RM) -r _build
	@$(RM) *.cmo *.cmo *.cmi

day1: clean
	@ocamlc str.cma AdventStdLib.ml day1.ml -o day1.native
	@./day1.native

day2: clean
	@ocamlc str.cma AdventStdLib.ml day2.ml -o day2.native
	@./day2.native

day3: clean
	@ocamlc str.cma AdventStdLib.ml day3.ml -o day3.native
	@./day3.native


