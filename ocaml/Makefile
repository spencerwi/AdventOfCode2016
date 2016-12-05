all: day1

clean:
	@$(RM) *.byte *.native
	@$(RM) -r _build
	@$(RM) *.cmo *.cmo *.cmi

day1: clean
	@ocamlc str.cma AdventStdLib.ml day1.ml -o day1.native
	@./day1.native

