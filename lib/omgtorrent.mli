open Rresult

type 'error res = (unit, [> R.msg] as 'error) result

type 'error writer = Bencode_token.t list -> 'error res

val writer_of_out_channel : out_channel -> [> R.msg] writer
(** [writer_of_out_channel chan] is a buffered section writer backed by [chan].
    It does not fsync, so you must take care of that yourself after writing.*)

type file_attribute = (* see BEP-0047 *)
  | Unknown_file_attribute of char
  | Symlink
  | Executable
  | Hidden
  | Padding

type file =
  { length : int64 ;
    path   : string list ;
    attrs  : file_attribute list ;
    hash   : Digestif.SHA1.Bytes.t option ;
  }

val pp_file : Format.formatter -> file -> unit

val write_file : 'error writer -> file -> 'error res
(* emit a file to the writer *)

val write_begin_info_files : 'error writer -> 'error res
(** writes the beginning of "info", and leaves you inside
*   {"info": {"files": ...
*)

val write_info_between_files_and_pieces :
  'error writer ->
  torrent_name:string ->
  piece_length:int ->
  'error res

val write_header : ?comment:string -> ?creation_date:int64 ->
  'error writer -> announce_urls:string list -> 'error res
(** this is [write_begin_info_files] with the root dict
    and "announce-list" prepended. *)

val write_magnet_info : 'error writer -> display_name:string ->
  info_hash:string -> 'error res
