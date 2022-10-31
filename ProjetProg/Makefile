all: aritha rapport.pdf

aritha: asyntax.ml parser.ml lexer.ml x86_64.ml functions.ml typing.ml generator.ml aritha.ml
	ocamlc -g asyntax.ml parser.mli parser.ml lexer.ml x86_64.ml functions.ml typing.ml generator.ml aritha.ml -o aritha

parser.ml: parser.mly
	ocamlyacc $^

lexer.ml: lexer.mll
	ocamllex $^

clean:
	rm -rf *.cmi *.cmo *.cmx *.mli lexer.ml parser.ml aritha *.log *.aux _minted-rapport

test: aritha
	@cd tests; \
	for f in *.exp; do \
		f2=$${f%.exp}; \
		echo -n "$$f2: "; \
		../aritha $$f; \
		gcc -no-pie $$f2.s -o $$f2; \
		if cmp -s <( ./$$f2 ) $$f2.rep; then \
			echo OK; \
		else \
			echo FAIL; \
		fi; \
		rm -f $$f2.s $$f2; \
	done; \

rapport.pdf: rapport.tex
	pdflatex -shell-escape $^
