all: test_my_list rapport.pdf

test_my_list: my_list.ml test_my_list.ml
	ocamlopt -o $@ $^

rapport.pdf: rapport.tex
	pdflatex -shell-escape $<

clean:
	rm -rf test_my_list rapport.pdf *.cmi *.cmx *~ *.log *.aux *.o _minted-rapport
