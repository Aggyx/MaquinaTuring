# Descripción
Simulando una máquina de Turing en lenguaje Ocaml.
La experiencia de la bobina de las máquinas de Turing se puede definir como un proceso secuencial de lectura, escritura y desplazamiento de una cinta infinita según reglas específicas, permitiendo la simulación de cualquier cálculo mecánico o algoritmo.

# Instalación
Instalar Opam (Página Oficial)[https://ocaml.org/docs/installing-ocaml]
```sh Linux
sudo apt-get install opam

opam init -y
eval $(opam env)
opam install utop yojson
```
Para compilar uso Ocamlopt
También se puede usar ocamlc
Uso una librería externa Yojson

# Uso
```sh
make run     (compila y ejecuta)
make         (compila)
make compile (compila)
make clean   (borra sources)
make fclean  (borra sources+ejecutable)
make re      (make fclean + make run)
``

# TODO
Determinar logica y estructura de la cinta
Parser Json a mano


