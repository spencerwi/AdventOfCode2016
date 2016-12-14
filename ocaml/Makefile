all: day1 day2 day3 day4

clean:
	@$(RM) *.byte *.native
	@$(RM) -r _build
	@$(RM) *.cmo *.cmo *.cmi

stdlib: clean
	@ocamlc -a str.cma AdventStdLib.ml -o AdventStdLib.cma -w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60

day1: clean stdlib
	@ocamlc AdventStdLib.cma day1.ml -o day1.native -w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60
	@./day1.native

day2: clean stdlib
	@ocamlc AdventStdLib.cma day2.ml -o day2.native -w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60
	@./day2.native

day3: clean stdlib
	@ocamlc AdventStdLib.cma day3.ml -o day3.native -w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60
	@./day3.native

day4: clean stdlib
	@ocamlc AdventStdLib.cma day4.ml -o day4.native -w +a-4-6-7-8-9-16-27-29..39-41..42-44-45-48-50-60
	@./day4.native


