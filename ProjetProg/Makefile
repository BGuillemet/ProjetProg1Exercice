all: aritha rapport.pdf

aritha: asyntax.ml parser.ml lexer.ml x86_64.ml functions.ml typing.ml generator.ml aritha.ml
	ocamlc -g asyntax.ml parser.mli parser.ml lexer.ml x86_64.ml functions.ml typing.ml generator.ml aritha.ml -o aritha

parser.ml: parser.mly
	ocamlyacc $^

lexer.ml: lexer.mll
	ocamllex $^

clean:
	rm -rf *.cmi *.cmo *.cmx *.mli lexer.ml parser.ml aritha expression.s expression *.log *.aux _minted-rapport

test: aritha
	@cd tests; \
	for f in *.exp; do \
		f2=$${f%.exp}; \
		echo -n "$$f2: "; \
		../aritha $$f; \
		gcc -no-pie expression.s -o expression; \
		if cmp -s <( ./expression ) $$f2.rep; then \
			echo OK; \
		else \
			echo FAIL; \
		fi \
	done; \
	rm -f expression.*

rapport.pdf: rapport.tex
	pdflatex -shell-escape $^
