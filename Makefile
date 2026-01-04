entrypoint = ft_turing.ml
output = ./ft_turing

all: compile

compile:
	mkdir -p ./build
	ocamlfind ocamlopt -package yojson -linkpkg -o $(output) $(entrypoint)
	mv *.cm* ./build/
	mv *.o ./build/

run: compile
	$(output)

clean:
	rm -f build/*.cm* && rm -f build/*.o

fclean: clean
	rm -f $(output)

re: fclean run

.PHONY: compile run clean fclean re