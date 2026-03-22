(* =================================== Tipo de dato para representar la configuracion  =================================== *)

type paso = Izq | Der | STOP

type transicion = {
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

let setup_core json num_transiciones =
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
			transiciones = Hashtbl.create num_transiciones;
		};
	} in
	core

let transiciones_json turing_machine_config json =
	let open Yojson.Safe.Util in
	let transiciones_por_estado = json |> member "transitions" |> to_assoc in
	List.iter (fun (estado, transiciones_json) ->
		List.iter (fun transicion_json ->
			let simbolo_leido = transicion_json |> member "read" |> to_string in
			let simbolo_escrito = transicion_json |> member "write" |> to_string in
			let movimiento = transicion_json |> member "action" |> to_string in
			let siguiente_estado = transicion_json |> member "to_state" |> to_string in
			Hashtbl.add turing_machine_config.transiciones.transiciones (estado, simbolo_leido) {
				por_escribir = simbolo_escrito;
				movimiento = (match movimiento with
					| "LEFT" -> Izq
					| "RIGHT" -> Der
					| "STOP" -> STOP
					| _ -> invalid_arg "transiciones_json: movimiento inválido");
				prox_estado = siguiente_estado
			}
		) (transiciones_json |> to_list)
	) transiciones_por_estado

(* =================================== Tipo de dato para representar la cinta  =================================== *)
type cinta = { mutable data : char array; mutable index: int }

let crear_cinta size =
  if size <= 0 then invalid_arg "create: size > 0"
     else { data = Array.make size '_'; index = 0}

let leer cinta = cinta.data.(cinta.index)

let escribir cinta (c:string) = cinta.data.(cinta.index) <- String.get c 0

let rellenar_cinta cinta (input:string) =
  let len = String.length input in
  if len > Array.length cinta.data then
    invalid_arg "rellenar_cinta: Array allocado es menor que tamaño input"
  else
    for i = 0 to len - 1 do
      cinta.data.(i) <- input.[i]
    done

let mover_derecha cinta =
  if Array.length cinta.data = 0 then
    invalid_arg "mover_derecha: cinta vacia"
  if Array.length cinta.data > cinta.index + 1 then
    cinta.index <- cinta.index + 1
  else cinta.index <- 0

let mover_izquierda cinta =
  if Array.length cinta.data = 0 then
    invalid_arg "mover_izquierda: cinta vacia"
  else if cinta.index <= 0 then
    cinta.index <- Array.length cinta.data - 1
  else 
    cinta.index <- cinta.index - 1

(* =================================== Utilidades =================================== *)

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

let print_cinta cinta =
  let content = Array.fold_left (fun acc c -> acc ^ (String.make 1 c) ^ " ") "" cinta.data in
  print_endline ("Cinta: " ^ content);
  print_endline ("Cabeza en posición: " ^ string_of_int cinta.index)

let print_transiciones turing_machine_config =
  (* Recoger todas las entradas en una lista *)
  let transiciones_list =
    Hashtbl.fold (fun (estado, simbolo) transicion acc ->
      (((estado, simbolo), transicion) :: acc)
    ) turing_machine_config.transiciones.transiciones []
  in
  (* Ordenar por estado y luego por símbolo *)
  let ordenadas =
    List.sort (fun ((e1, s1), _) ((e2, s2), _) ->
      let c = String.compare e1 e2 in
      if c <> 0 then c else String.compare s1 s2
    ) transiciones_list
  in
  (* Imprimir en orden *)
  List.iter (fun ((estado, simbolo), transicion) ->
    print_endline ("(" ^ estado ^ ", " ^ simbolo ^ ") -> (" ^
      transicion.por_escribir ^ ", " ^
      (match transicion.movimiento with Izq -> "LEFT" | Der -> "RIGHT" | STOP -> "STOP") ^
      ", " ^ transicion.prox_estado ^ ")")
  ) ordenadas;

  print_endline("\n\n########################################################################################################n")

(* =================================== MAIN =================================== *)
let usage_msg = "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n  jsonfile\t\tjson description of the machine\n  input\t\tinput of the machine\n\noptional arguments:\n  -h, --help\tshow this help message and exit"
let argv = ref []

let parse_argv ()=
	let rec parse_args () =
        if Array.length Sys.argv > 1 then
            argv := List.tl (Array.to_list Sys.argv)
        else ()
        in
	parse_args ()

let checkforhelp ()= 
	let print_help () =
	    print_endline usage_msg;
	    exit 0 in

	if (List.exists (fun arg -> arg = "-h" || arg = "--help") !argv) then
		print_help ();

	if (List.length !argv <> 2) then
		print_help ()

let () =
    parse_argv ();
    checkforhelp ();
	let filename = List.nth !argv 0 in
	let entrada_por_interpretar = List.nth !argv 1 in
	print_endline "<####################################################>\n\t\tTURING MACHINE\n<####################################################>";
	print_endline ("Archivo: " ^ filename);
	print_endline ("Cinta de entrada: " ^ entrada_por_interpretar);

	let content = read_text_from_file filename in
	let json = Yojson.Safe.from_string content in
	(*Format.printf "Estructura Json:\n%a\n" Yojson.Safe.pp json;*)

	let turing_machine_config = setup_core json (String.length entrada_por_interpretar) in

	print_endline ("Configuración: " ^ turing_machine_config.name);
	print_endline ("Alphabet: " ^ String.concat ", " turing_machine_config.alphabet);
	print_endline ("Blank symbol: " ^ turing_machine_config.blank);
	print_endline ("States: " ^ String.concat ", " turing_machine_config.states);
	print_endline ("Initial : " ^ turing_machine_config.initial);
	print_endline ("Finals: " ^ String.concat ", " turing_machine_config.finals);
	let cinta = crear_cinta (String.length entrada_por_interpretar) in
	rellenar_cinta cinta entrada_por_interpretar;

	turing_machine_config.transiciones.cinta := List.init (Array.length cinta.data) (fun i -> String.make 1 (cinta.data.(i)));

	transiciones_json turing_machine_config json;

	print_endline ("Transiciones cargadas: " ^ string_of_int (Hashtbl.length turing_machine_config.transiciones.transiciones));
	print_transiciones turing_machine_config;
	print_endline ("\n");
	(*
	while estado actual != HALT:
		leer símbolo en la cinta
		buscar transición correspondiente
		escribir símbolo en la cinta
		mover cabeza
		actualizar estado actual
	*)
	while !(turing_machine_config.transiciones.estado) <> "HALT" do
		(* Leer símbolo en la cinta *)
		let simbolo_leido = List.nth !(turing_machine_config.transiciones.cinta) !(turing_machine_config.transiciones.cabeza) in
		(* Buscar transición correspondiente *)
		(try
			let transicion = Hashtbl.find turing_machine_config.transiciones.transiciones (!(turing_machine_config.transiciones.estado), simbolo_leido) in
			(* Escribir símbolo *)
			escribir cinta transicion.por_escribir;
			(* Sincronizar escritura en la lista *)
			turing_machine_config.transiciones.cinta := List.mapi (fun i c -> if i = cinta.index then String.make 1 cinta.data.(i) else c) !(turing_machine_config.transiciones.cinta);
			(*Mover cinta*)
			(match transicion.movimiento with
			| Izq -> mover_izquierda cinta
			| Der -> mover_derecha cinta
			| STOP -> ()); (* No hacemos nada *)
			(* Sincronizar movimiento *)
			turing_machine_config.transiciones.cabeza := cinta.index;
			(* Actualizar estado *)
			turing_machine_config.transiciones.estado := transicion.prox_estado;
			print_endline ("(" ^ !(turing_machine_config.transiciones.estado) ^ ", " ^ simbolo_leido ^ ") -> (" ^ transicion.por_escribir ^ ", " ^ (match transicion.movimiento with Izq -> "LEFT" | Der -> "RIGHT" | STOP -> "STOP") ^ ", " ^ transicion.prox_estado ^ ")");
			print_cinta cinta;
		with Not_found ->
			print_endline ("No existe la siguiente transición:\n(" ^ !(turing_machine_config.transiciones.estado) ^ ", " ^ simbolo_leido ^ ")");
			turing_machine_config.transiciones.estado := "HALT"
		)
	done;
