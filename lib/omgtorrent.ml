(* http://www.bittorrent.org/beps/bep_0003.html *)
let string_list strings =
  (* construct a list of Strings *)
  Bencode.List
    (List.map (fun str -> Bencode.String str) strings)

open Rresult

module R =
struct
    include R
    let rec fold f acc = function
      | [] -> Ok acc
      | hd::tl ->
        f hd >>= fun acc ->
        fold f acc tl
end

type 'error res = (unit, [> R.msg] as 'error) result

type 'error writer = Bencode_token.t list -> 'error res

let writer_of_out_channel out_channel : [> R.msg] writer =
  fun token_list ->
    Ok (Bencode_token.Easy.output out_channel token_list)


let hash_and_write ctx (writer:[>] writer) =
  fun token_list ->
    let serialized = Bencode_token.Easy.to_string token_list in
    let () = Digestif.SHA1.Bytes.feed_bytes ctx (Bytes.of_string serialized) in
    writer token_list

    (* let buf = Buffer.create 2 in
   let tokbuf = Bencode_token.Encode.to_buf buf in
   let () = Bencode_streaming.Encode.to_seq Bencode.(Integer 4L)
            |>(Bencode_token.Encode.put_many tokbuf) in
   Buffer.contents buf ;; *)


(*
(*TODO: enforce lexicographical order on keys inside dicts:
  BEP-003: Keys must be strings and appear in sorted order
           (sorted as raw strings, not alphanumerics).
 *)
module type X = sig
  type root
  type list
  type key
  type dict

  type (_,_) state =
    (*  | Root : [`root] state
        | Dict : ([< `root | `key of _ | `list of _ ] state as 'p
               ) -> [`dict of 'p] state
        | List : (([< `root | `key of _ | `list of _ ] as 't )state as 'p
               ) -> [`list of 't] state
        | Key : ( ([> ] as 'key) (* store the key as a polymorphic variant*)
                * ([`dict of _ state] state as 'p)
              ) -> [`key of 'key * 'p] state
        (*constraint 'key = 'key*)*)
    | Root : (root,root) state
    | DictRoot : (root,root) state -> (root,dict) state
    | ListRoot : (root,root) state -> (root,list) state
    | DictList : (_,list) state -> (list,dict) state
    | DictKey  : (_,key) state -> (key,dict) state

    | ListList : (_,list) state -> (list,list) state
    | ListKey  : (_,key) state -> (key,list) state
    | KeyDict  : (_,dict) state -> (dict,key) state
end

type xxx = [`xxx]
type xyz = [`xyz] type zzz
type _ wa =
  | A : int -> xxx wa
  | B : int -> xyz wa
  | C : int -> zzz wa
type _ proof =
    AB : [xxx | xyz] proof

let ab : type a.a wa -> int  =
  function A x -> x+1
         | B x -> x*2

module S = struct
  type root = [`root]
  type dict = [`dict]
  type list = [`list]
  type key = [`key]
    type (_,_) state =
    | Root : (root,root) state
    | DictRoot : (root,root) state -> (root,dict) state
    | ListRoot : (root,root) state -> (root,list) state
    | DictList : (_,list) state -> (list,dict) state
    | DictKey  : (_,key) state -> (key,dict) state

    | ListList : (_,list) state -> (list,list) state
    | ListKey  : (_,key) state -> (key,list) state
    | KeyDict  : (_,dict) state -> (dict,key) state
    type ('x,'a) value = ('x,'a) state constraint 'a = [< key | list]
    type ('x,'a) list_valid =
      ('x,'a) state constraint 'a = [< key | list | root]
    type (_,'b) proof=
        A : (_,[< list | root | key] as 'x) state -> ('x,'x) proof
    type _ s =
      | SRoot : root s
      | SDict : dict s
      | SKey  : key s
      | SList : list s
    type (_,_) valid =
      | A : root s * dict s -> (root,dict) valid
      | B : root s * list s -> (root,list) valid
      | C : dict s * key s -> (dict,key) valid
      | D : key s * dict s -> (key,dict) valid
      | G : key s * list s -> (key,list) valid
      | E : list s * list s -> (list,list) valid
      | F : list s * dict s -> (list,dict) valid
end

let w_list (type x) (type y) (type z) writer
    (proof : (x S.s,y S.s) S.valid)
    (state: (x, y) S.state)
  : (y, S.list) S.state =
  match proof, state with
  | S.B (SRoot,SList), S.ListRoot -> S.ListList state
  | S.B (_,_), S.ListRoot _ -> S.ListList state
  | _, S.ListList _ -> S.ListList state
  | _, S.KeyDict _ -> S.ListKey state
  | _, S.ListKey _ -> S.ListList state
(*  | _ -> R.error_msg "yo"*)

let x =
  let open S in
  let proof : (root,list) S.valid =
    S.B (SRoot,SList)
  in
  let state = ListRoot S.Root in
  w_list 1 proof state

let n = Refl Root

let empty_state = Root

let enter_dict state = Dict state

let enter_key state typ =
  Key (typ, state)

let end_key (Key (_typ, state)) = state

let end_dict (Dict state) = state

let enter_list state = List state

let end_list (List state) = state

let a = empty_state
let b = enter_dict a
let c = enter_key b `hi
let d = enter_list a

let e = enter_list c
let f = end_list e
let g = end_key f
let h = end_dict g

let n = Key (`a, Dict Root)
let x = List (Key (`ab, Dict (List Root)))
let y = Key (`a, Dict Root)

let cool
    (y : [`key of 'xxx * [`dict of [`root] state] state] state) =
  match y with
  | Key (`a, Dict Root) -> 1
  | Key (`b, Dict Root) -> 3

let cool (type x)
    (y : [`key of [`a|`b] * x] state) =
  match y with
  | Key (`a, Dict Root) -> 1
  | Key (`b, Dict Root) -> 3

(* why does this fail???*)
(*
let cool (type x)
    (y : [`key of x * [`dict of [`root] state] state] state) =
  match y with
  | Key (`a, Dict Root) -> 1
  | Key (`b, Dict Root) -> 3
*)

let what (y : [`key of [`a] * [`dict of [`root] state] state] state) =
  match y with
  | Key (`a, Dict Root) -> 3

let whatis = what n

let write_dict (writer:'error writer) state =
  writer `BeginDict >>| fun () ->
  Dict state

let write_key (writer:'error writer) (name,name_poly) state =
  writer (`S name) >>| fun () ->
  Key (name_poly, state)

let write_list (writer:'error writer) state =
  writer `BeginList >>| fun () ->
  List state

let write_string_in_list (writer:'error writer) ~(value:string) (List x) =
  writer (`S value) >>| fun () ->
  List x

let write_string_in_key (writer:'error writer) value (Key (x,dict)) =
  writer (`S value) >>| fun () ->
  dict

let write_string (type x) (type y) writer
  (*    (state:[< `list of [<`root | `list of [`root]] | `root] state)*)
    (state: x state)
    : (int, [> R.msg]) result
    =
  match state with
  (*| List n -> write_string_in_list writer ~value:"a" n*)
  (*| Key (_,_) -> Ok state (*write_string_in_key writer "a" state*)*)
  | List List Root -> Ok 1
  | List Root -> Ok 2
  | Dict Root -> Ok 4
  | List _ -> Ok 5
  | Dict _ -> Ok 6
  | Root -> Ok 7
  | Key _ -> Ok 8


let write_string
    (type xxx)
    (type aaa)
    (type a)
    (writer:'error writer) string
    (state : [> `list of xxx state] state
    ) : [< `list of xxx state] state
  =
  (*writer (`S string) >>| fun () ->*)
  match state with
    | List x -> List x
  | Root -> Root
  | Root -> List Root
  | Dict _ -> List Root
  | Key _ -> Root
(*  | Dict _ -> Dict Root
  | Key xyz -> Root
    | Key (x,y) -> Root*)

let () = what y |> Printf.printf "rooo: %d\n"
*)

let write_announce_list_entry (write:[>] writer) urls =
  (* under root dict *)
  write @@ [ `S "announce" ; (* compat with older clients *)
               `S (List.hd urls);
             (* http://www.bittorrent.org/beps/bep_0012.html *)
             `S "announce-list";
             `BeginList]
           @ (List.map (fun url -> `S url) urls)
           @ [`End]

type file_attribute = (* see BEP-0047 *)
  | Unknown_file_attribute of char
  | Symlink
  | Executable
  | Hidden
  | Padding

let char_of_file_attribute = function
  | Unknown_file_attribute c -> c
  | Symlink -> 's'
  | Executable -> 'x'
  | Hidden -> 'h'
  | Padding -> 'p'

let string_of_file_attributes attrs =
  String.init (List.length attrs)
    (fun i -> List.nth attrs i |> char_of_file_attribute)

let padding_path n = string_list [".pad"; string_of_int n]

type file = { length : int64 ;
              path   : string list ;
              attrs  : file_attribute list ;
              hash   : Digestif.SHA1.Bytes.t option ;
            }

let pp_file fmt file =
  Fmt.pf fmt "@[<v>{ @[<v>path: %a ;@ length: %Ld ; \
              attrs: %a; @ hash: %a@] }@]"
    Fmt.(list ~sep:(unit "/") string) file.path
    file.length
    Fmt.string (string_of_file_attributes file.attrs) (* TODO pp*)
    Fmt.(option (fun fmt v -> Fmt.pf fmt "%s" (Digestif.SHA1.Bytes.to_hex v
                                              |> Bytes.to_string ))
        ) file.hash

type file_ctx =
  { hash_ctx  : Digestif.SHA1.Bytes.ctx ;
    piece_ctx : Digestif.SHA1.Bytes.ctx ;
    remaining : int64 ;
  }

type torrent_ctx =
  { hash_ctx : Digestif.SHA1.Bytes.ctx ;
    piece_length : int64 ;
    name : string ;
    (*bencoding : Bencode_streaming.t ;*)
  }

type torrent =
  { info_hash : string;
  }

let create_torrent ~piece_length ~name =
  { hash_ctx = Digestif.SHA1.Bytes.init () ;
    piece_length ;
    name ;
  }

let add_new_file = ()

let dict_entry (write:[>] writer) key value =
  write [`S key ; value]

let write_dict_entry (write:[>] writer) key f =
  write [`S key] >>= fun () ->
  f write

let write_string_list (write:[>] writer) (list:string list) =
  write @@ `BeginList
           ::List.map (fun (str:string) -> `S str) list
           @ [ `End ]

let write_file write file =
  (* under "info"->"files" list *)
  write [`BeginDict] >>= fun () ->
  (
    (* BEP-0047: *)
    dict_entry write "attr" (`S (string_of_file_attributes file.attrs)
                            ) >>= fun () ->

    dict_entry write "length" (`I file.length) >>= fun () ->
    write [`S "path"] >>= fun () ->
    write_string_list write file.path >>= fun () ->

    (* http://www.bittorrent.org/beps/bep_0047.html below: *)
    begin match file.hash with
      | None -> Ok ()
      | Some hash -> dict_entry write "sha1" (`S (Bytes.to_string hash))
    end

    (*; ("symlink path" , string_list path_lst)*)

  ) >>= fun () ->
  write [`End] (* end dict*)

let write_begin_info_files (writer: 'error writer) =
  (* under root dict *)
  (* IMPORTANT:
     "All keys must be byte strings and MUST appear in lexicographical order"*)
  (* IMPORTANT: this function leaves us inside a dict,
     and missing the "pieces" string and the "files" dict
  *)
  (* everything below is part of the infohash: *)
  writer [ `BeginDict ] >>= fun () ->
  writer [`S "files" ; `BeginList]

let write_info_between_files_and_pieces
    (writer: 'error writer)
    ~torrent_name
    ~piece_length  =
  (* suggested name for the download: *)
  writer [`End] >>= fun () -> (* end "files" list *)
  dict_entry writer "name" (`S torrent_name) >>= fun () ->
  (* piece length, in bytes: *)
  dict_entry writer "piece length" (`I (Int64.of_int piece_length)
                                   ) >>= fun () ->
  writer [ `S "pieces" ] (* exit inside pieces entry *)

let write_info_after_pieces ~is_private
    (writer:'error writer)
  =
  (* BEP-0027: *)
  dict_entry writer "private" (`I (if is_private then 1_L else 0_L))


let write_header ?comment ?creation_date write ~announce_urls =
  write [`BeginDict] >>= fun () -> (* begin root dict *)
  write_announce_list_entry write announce_urls >>= fun () ->

  begin match comment with
    | None -> Ok ()
    | Some comment -> write [`S "comment" ; `S comment]
  end >>= fun () ->

  begin match creation_date with
    | None -> Ok ()
    | Some date -> write [`S "creation date" ; `I date]
  end >>= fun () ->

  (* the key itself is not hashed, only the value node: *)
  write [ `S "info" ]

let write_magnet_info (write:[>]writer) ~display_name ~info_hash =
  write [ `S "magnet-info" ; (*enter key under root dict*)
          `BeginDict ;
          `S "display-name"; `S display_name ; (* must be url-encoded*)
          `S "info_hash"; `S info_hash ;
          `End ]

let metainfo =
  (* "metainfo" aka a .torrent file *)
  (* must have "announce" and "info" *)
  (* TODO "collections": http://www.bittorrent.org/beps/bep_0038.html *)
  ()

(* root ->
   ( "created by", "omktorrent" );
   ( "comment", <string:len 61> );
   ( "creation date", 1489680000 );
   ( "encoding", "UTF-8" );

*)
