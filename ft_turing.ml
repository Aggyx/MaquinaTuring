(* =================================== funciones precompiladas  =================================== *)

let read_text_from_file filename =
	try
		let ic = open_in filename in
		let n = in_channel_length ic in
		let s = really_input_string ic n in
		close_in ic;
		s
	with Sys_error msg ->
		print_endline ("Error: " ^ msg);
		exit 1

(* =================================== Tipo de dato para representar la configuracion  =================================== *)

type paso = Izq | Der | Paro

type transicion = {
  por_leer: string;
  por_escribir: string;
  movimiento: paso;
  prox_estado: string
}

type turing_machine = {
  cinta: string list ref; (* Referencia a la cinta, array *)
  cabeza: int ref; (*indexo*)
  estado: string ref;
  transiciones: (string * string, transicion) Hashtbl.t;  (* HashMap de transiciones (state, symbol): transicion *)
}

type core = {
  name              : string;
  alphabet          : string list;
  blank             : string;
  states            : string list;
  initial           : string;
  finals            : string list;
  transiciones      : turing_machine;
}

(* Función para rellenar la estructura 'core' desde un Json (Usando Yojson) *)

let setup_core json =
	let core = {	
		name = Yojson.Safe.Util.(json |> member "name" |> to_string);
		alphabet = Yojson.Safe.Util.(json |> member "alphabet" |> to_list |> filter_string);
		blank = Yojson.Safe.Util.(json |> member "blank" |> to_string);
		states = Yojson.Safe.Util.(json |> member "states" |> to_list |> filter_string);
		initial = Yojson.Safe.Util.(json |> member "initial" |> to_string);
		finals = Yojson.Safe.Util.(json |> member "finals" |> to_list |> filter_string);
		transiciones = {
			cinta = ref [];
			cabeza = ref 0;
			estado = ref Yojson.Safe.Util.(json |> member "initial"	|> to_string);
			transiciones = Hashtbl.create 10;
		};
	} in
	core

	
(* =================================== Tipo de dato para representar la cinta =================================== *)

let crear_cinta size = Array.make size '_'  (* Inicializar con _ *)

(* =================================== MAIN =================================== *)
let usage_msg = "./ft_turing [-v] file1 alphabet" 
let verbose = ref false
let argv = ref []  (* list *)

let alphabet = ref ""

let anon_fun filename = argv:= filename::!argv  (* Append param to the list *)

let optionlist = [("-v", Arg.Set verbose, "Enable verbose mode")]


let () = 
	Arg.parse optionlist anon_fun usage_msg; (* Modulo standard Arg.parse para parsear argumentos de línea de comandos *)

	if List.length !argv <> 2 then (
		print_endline "Usage: ./ft_turing <file:string> <alphabet:string>";
		exit 1
	);
	let filename = List.nth !argv 1 in
	let alphabet_str = List.nth !argv 0 in
	print_endline "<####################################################>\n\t\tTURING MACHINE\n<####################################################>";
	print_endline ("Archivo: " ^ filename);
	print_endline ("Cinta de entrada: " ^ alphabet_str);

	let content = read_text_from_file filename in
	let json = Yojson.Safe.from_string content in
	(*Format.printf "Estructura Json:\n%a\n" Yojson.Safe.pp json;*)

	let turing_machine_config = setup_core json in
	(*	
	let turing_machine_config = {
		name = Yojson.Safe.Util.(json |> member "name" |> to_string);
		alphabet = Yojson.Safe.Util.(json |> member "alphabet" |> to_list |> filter_string);
		blank = Yojson.Safe.Util.(json |> member "blank" |> to_string);
		states = Yojson.Safe.Util.(json |> member "states" |> to_list |> filter_string);
		initial = Yojson.Safe.Util.(json |> member "initial" |> to_string);
		finals = Yojson.Safe.Util.(json |> member "finals" |> to_list |> filter_string);
		transiciones = {
			cinta = ref [];
			cabeza = ref 0;
			estado = ref Yojson.Safe.Util.(json |> member "initial" |> to_string);
			transiciones = Hashtbl.create 10;
		};
	} in*)

	print_endline ("Name of the loaded Machine: " ^ turing_machine_config.name);
	print_endline ("Loaded alphabet: " ^ String.concat ", " turing_machine_config.alphabet);
	print_endline ("Blank symbol: " ^ turing_machine_config.blank);
	print_endline ("States: " ^ String.concat ", " turing_machine_config.states);
	print_endline ("Initial state: " ^ turing_machine_config.initial);
	print_endline ("Final states: " ^ String.concat ", " turing_machine_config.finals);
	print_endline ("\n<####################################################>\n\t\tINITIALIZING TURING MACHINE\n<####################################################>");

	(*Guardar la entrada por interpretar en la cinta *)
	(*Saber como gestionar la cinta*)
	(*Simular la máquina de Turing paso a paso y medir complejidad si queremos booonus*)