open Rresult

let handle_file ~unaligned ~piece_length ~pieces_chan
    ~files_chan rel_path input  =
  let open Omgtorrent in
  let write_piece_hash hash =
    output_bytes pieces_chan hash
  in
  let files_writer = Omgtorrent.writer_of_out_channel files_chan in
  let { Unix.LargeFile.st_size = file_size; _ } = Unix.LargeFile.fstat input in
  let mmap_piece_count   = Int64.div file_size (piece_length |> Int64.of_int)
                         |> Int64.to_int in
  let manual_piece_length = Int64.rem file_size (piece_length |> Int64.of_int)
                         |> Int64.to_int in
  let padding_size =
    if unaligned (* then don't insert padding *)
    then 0
    else (piece_length - manual_piece_length) mod piece_length in
  assert(mmap_piece_count >= 0);
  assert(padding_size <= piece_length);
  assert(piece_length > 0);
  assert(padding_size >= 0);
  assert(manual_piece_length <= piece_length);

  let whole_file_sha1_ctx = Digestif.SHA1.Bytes.init () in

  (* hash pieces from the mmap: *)
  ( let ba =
      Bigarray.Array2.map_file input Bigarray.Char Bigarray.C_layout
        true  (*="shared". see comment in the other map_file below *)
        (mmap_piece_count) piece_length
    in
    for idx = 0 to mmap_piece_count-1 do
      Logs.debug (fun m -> m "hashing piece idx %d" idx);
      Digestif.SHA1.Bytes.feed_bigstring whole_file_sha1_ctx
      @@ Bigarray.Array2.slice_left ba idx ;
      let piece_ctx = Digestif.SHA1.Bytes.init () in
      Digestif.SHA1.Bytes.feed_bigstring piece_ctx
      @@ Bigarray.Array2.slice_left ba idx ;
      write_piece_hash (Digestif.SHA1.Bytes.get piece_ctx)
    done ) ;

  (* hash remainder of the file *)
  ( let ba =
      Bigarray.Array1.map_file input Bigarray.Char Bigarray.C_layout
        ~pos:(Int64.sub file_size @@ Int64.of_int manual_piece_length)
        (* ^-- TODO: since piece_length is a multiple of 16K,
           this is always page-aligned ??? *)
        true  (*="shared". no actual reason for this except ocaml insisting on
                 allocating mapped files with PROT_WRITE because no one thought
                that an immutable vector type could ever come in handy.
                allocating with PROT_WRITE and MAP_PRIVATE will result in an OOM
                error on large files because the kernel interprets it as a
                request to allocate the full size of the file to the process.
              *)
        manual_piece_length
    in
    let piece_ctx = Digestif.SHA1.Bytes.init () in
    Digestif.SHA1.Bytes.feed_bigstring whole_file_sha1_ctx ba ;
    Digestif.SHA1.Bytes.feed_bigstring piece_ctx ba ;
    (* TODO hash {padding_size} nulls *)
    write_piece_hash (Digestif.SHA1.Bytes.get piece_ctx) ;
  ) ;

  (* write file record *)
  ( let file = { length = file_size ;
                   (* fill in relative path, split by '/': *)
                 path   = Fpath.segs rel_path ;
                 attrs  = [] ;
                 hash   = Some (Digestif.SHA1.Bytes.get whole_file_sha1_ctx) ;
               } in
    Logs.info (fun m -> m "adding file: %a" Omgtorrent.pp_file file) ;
    write_file files_writer file
  ) >>= fun () ->

  (* insert padding file record, if applicable *)
  if padding_size > 0 then
    let padding_file =
      { length = Int64.of_int padding_size ;
        path =  [ ".pad" ; string_of_int padding_size ] ;
        attrs = [ Padding ; Hidden ] ;
        hash = None; } in
    write_file files_writer padding_file >>| fun () ->
    mmap_piece_count + 1, file_size
  else
    Ok (mmap_piece_count, file_size)

let fold_files ~root acc f input
  : ('acc, [> R.msg]) result =
  Bos.OS.Path.fold
    (* TODO consider ~dotfiles arg*)
    ~dotfiles:false
    (fun fpath acc ->
       acc >>= fun ((files, pieces, bytes) as acc : int * int * int64) ->
       let path = Fpath.to_string fpath in
       (* TODO fail if it's a symlink *)
       if Sys.is_directory path then
         Ok acc
       else
         match Fpath.relativize ~root fpath with
         | Some rel_path ->
           (* let's open it in read-WRITE mode. we don't need to write,
              obviously, but the OCaml way(tm) is to live life dangerously!
              FML...
           *)
           Logs.debug (fun m -> m "Adding file %s to torrent [%d/%d/%Ld]" path
                          files pieces bytes ) ;
           ( let fd = Unix.openfile path Unix.[O_RDWR] 0 in
             let res = f rel_path fd in
             Unix.close fd ;
             res ) >>= fun (new_pieces, added_size) ->
           Ok (files + 1, pieces+new_pieces, Int64.add bytes added_size)
         | None ->
           Logs.err (fun m -> m "failed to relative for %s" path);
           Ok acc
    )
    (Ok acc)
    [input] |> R.join

let file_insertion_points output_path section_count =
  let one_terabyte = Int64.(mul 1024L (mul 1024L (mul 1024L 1024L))) in
  (* preallocate 1TB of sparse space per worker in output file: *)
  (*let count_64 = Int64.of_int section_count in*)
  (*let () = Unix.LargeFile.ftruncate parent_fd
      Int64.(mul one_terabyte count_64)
    in*)
  (* prepare an fd for each section: *)
  Array.init section_count
    (fun idx ->
       let my_fd =
           Unix.openfile output_path
             Unix.[ O_RDWR ] 0o600  in
       let my_offset = Int64.(mul one_terabyte @@ of_int idx) in
       assert ( my_offset =
                Unix.LargeFile.lseek my_fd my_offset Unix.SEEK_SET ) ;
       Unix.out_channel_of_descr my_fd
    )

let collapse_insertion_points
    ~(output_fd:Unix.file_descr)
    ~(fds: ( [`Hash | `Don't_hash]
           * [`String | `Raw]
           * Unix.file_descr array) list)
  =
  (* NOTE: remember to flush output buffers before calling this function *)

  let infohash_ctx = Digestif.SHA1.Bytes.init () in

  let one_terabyte = Int64.(mul 1024L (mul 1024L (mul 1024L 1024L))) in
  let output = Unix.out_channel_of_descr output_fd in
  let get_section_len fd =
    let end_pos = Unix.LargeFile.lseek fd 0L Unix.SEEK_CUR in
    end_pos, Int64.rem end_pos one_terabyte
  in
  let get_total_sections_len fds = Array.fold_left (fun acc fd ->
      Int64.add acc (get_section_len fd |> snd)) 0L fds in

  (* buffer for keeping chunks of the sections to be rewritten: *)
  let bufsize = 32768 in
  let bufsize_L = Int64.of_int bufsize in
  let buf = Bytes.make bufsize '\000' in

  let rewrite_section output_f fd =
    let end_pos, section_len = get_section_len fd in
    let _ = Unix.LargeFile.lseek fd (Int64.sub end_pos section_len)
        Unix.SEEK_SET in
    let rec write_next = function
      | 0L -> ()
      | bytes_left ->
      let rlen = Int64.to_int @@ min bufsize_L bytes_left in
      let actual_read = Unix.read fd buf 0 rlen in
      let string_buf = Bytes.sub_string buf 0 actual_read in
      output_f string_buf ;
      write_next Int64.(sub bytes_left @@ of_int actual_read)
    in write_next section_len ;
  in

  let output_and_hash str =
    Digestif.SHA1.Bytes.feed_bytes infohash_ctx (Bytes.of_string str) ;
    output_string output str in
  let output_func = function
    | `Hash -> output_and_hash
    | `Don't_hash -> output_string output
  in

  let rec write_sections acc = function
    | [] -> acc
    | (do_hash, `String, string_fds)::tl ->
      let outputf = output_func do_hash in
      outputf (Int64.to_string @@ get_total_sections_len string_fds) ;
      outputf ":" ;
      Array.iter (rewrite_section outputf) string_fds ;
      write_sections () tl
    | (do_hash, `Raw, raw_fds)::tl ->
      let outputf = output_func do_hash in
      Array.iter (rewrite_section outputf) raw_fds ;
      write_sections () tl
  in write_sections () fds ;

  flush output ;
  let new_end = Unix.LargeFile.lseek output_fd 0L Unix.SEEK_CUR in
  Unix.LargeFile.ftruncate output_fd new_end ;
  infohash_ctx

let do_make () (*<-- unit: setup_log *)
    (unaligned:bool)
    (piece_length:int)
    (output_path:string)
    (display_name:string option)
    (input_path:string) = (* TODO Fpath.t ???*)
  let output_fd = (* create output file: *)
    Unix.openfile output_path
      Unix.[ O_CREAT  ; O_EXCL ; (* make sure we don't overwrite existing file*)
             O_RDWR ] 0o600 in
  let output_chan = Unix.out_channel_of_descr output_fd in
  let workers = 1 in
  let section_count = 1 (* "announce-list" and enter root->infox *)
                      + ( 1 (* first_info_fds *)
                          + workers (* "files"; 1 per worker thread *)
                          + 1 (* between; "name" and "piece length" *)
                          + workers (* "pieces"; 1 per worker_thread *)
                          + 1) (* last_info; "private" ; end "info" *)
                      + 1 (* root -> magnet_info *)
  in
  let insertion_points = file_insertion_points output_path section_count in
  Logs.info (fun m -> m "got %d insertion points from section_count:%d/workers:%d"
                (Array.length insertion_points)
                section_count
                workers
            );
  let header_fds = Array.sub insertion_points 0 1 in (* "announce-list" *)
  let first_info_fds = Array.sub insertion_points (Array.length header_fds) 1 in
  let files_fds = Array.sub insertion_points
      Array.(length header_fds + length first_info_fds) workers in
  let succeeds a_lst = ( List.fold_left (+) 0
                             (List.map Array.length a_lst) ) in
  let between_files_and_pieces_fds = Array.sub insertion_points
      ( succeeds [header_fds ; first_info_fds ; files_fds])  1
  in
  let pieces_fds = Array.sub insertion_points
      ( succeeds [header_fds ; first_info_fds ; files_fds ;
                  between_files_and_pieces_fds]) workers
  in
  let last_info_fds = Array.sub insertion_points
      ( succeeds [header_fds ; first_info_fds ; files_fds ;
                 between_files_and_pieces_fds ; pieces_fds ]) 1 in
  let footer_fds = Array.sub insertion_points
      ( succeeds [header_fds ; first_info_fds ; files_fds ;
                 between_files_and_pieces_fds ; pieces_fds ; last_info_fds]) 1 in

  assert ( succeeds [header_fds ; first_info_fds ; files_fds ;
                     between_files_and_pieces_fds ; pieces_fds;
                     last_info_fds ; footer_fds]
           = section_count) ;

  let fds = Array.map Unix.descr_of_out_channel in
  let writers = Array.map Omgtorrent.writer_of_out_channel in

  Fpath.of_string input_path >>= fun input_fpath ->
  let display_name =
    match display_name with
    | None -> Fpath.basename input_fpath
    | Some dn -> dn in

  (**** handle things before "files" *)
  let announce_urls = ["udp://tracker.leechers-paradise.org:6969" ;
                       "udp://zer0day.ch:1337" ;
                       "udp://open.demonii.com:1337" ;
                       "udp://tracker.coppersurfer.tk:6969" ;
                       "udp://exodus.desync.com:6969" ] in
  Omgtorrent.write_header (writers header_fds).(0) ~announce_urls >>= fun () ->
  Logs.debug (fun m -> m "headers written: %a"
                 Fmt.(list ~sep:(unit " ") string) announce_urls);

  Omgtorrent.write_begin_info_files (writers first_info_fds).(0) >>= fun () ->

  (**** handle "files": *)
  let root =
    if Sys.is_directory input_path
    then input_fpath
    else Fpath.parent input_fpath in
  Logs.debug (fun m -> m "got FDs");
  fold_files ~root (0,0,0L)
    (handle_file ~unaligned ~piece_length
       ~pieces_chan:(pieces_fds.(0)) (* TODO threads *)
       ~files_chan:(files_fds.(0))   (* TODO threads*)
    ) input_fpath >>= fun (filecount, piececount, totalbytes) ->

  Logs.debug (fun m -> m "about to write between files and pieces: %d"
                 (Array.length between_files_and_pieces_fds)
             );
  Omgtorrent.write_info_between_files_and_pieces
    (writers between_files_and_pieces_fds).(0)
    ~torrent_name:display_name ~piece_length
  >>= fun () ->
  (writers last_info_fds).(0) [`End] >>= fun () -> (*end "info" dict*)

  Array.iter flush insertion_points ; (* fsync everything *)

  let infohash_ctx = collapse_insertion_points ~output_fd
    ~fds:[ `Don't_hash , `Raw    , fds header_fds ;
           `Hash       , `Raw    , fds first_info_fds ;
           `Hash       , `Raw    , fds files_fds ;
           `Hash       , `Raw    , fds between_files_and_pieces_fds ;
           `Hash       , `String , fds pieces_fds ;
           `Hash       , `Raw    , fds last_info_fds ;
           `Don't_hash , `Raw    , fds footer_fds ;
         ] in

  let info_hash, info_hash_hex =
    let h = Digestif.SHA1.Bytes.get infohash_ctx in
    Bytes.to_string h , Digestif.SHA1.Bytes.to_hex h |> Bytes.to_string in

  let last_wr = Omgtorrent.writer_of_out_channel output_chan in
  Logs.debug (fun m -> m "writing magnet info");
  Omgtorrent.write_magnet_info last_wr ~display_name ~info_hash >>= fun () ->
  Logs.debug (fun m -> m "wrote magnet");

  last_wr [`End] >>| fun () -> (* end root dict *)
  flush output_chan ;

  Logs.info (fun m -> m "Wrote file-%saligned torrent consisting of %d files, \
                         %d non-null pieces of %d bytes each, \
                         for a total of %Ld bytes"
                (if unaligned then "UN" else "")
                filecount piececount piece_length totalbytes );

  let magnet_uri =
    let urlencode (unenc:string) =
      let alphanum = "0123456789abcdef" in
      let buf : string list ref = ref [] in
      let _ = String.iter (function
          | 'a'..'z' | 'A'..'Z' | '0'..'9' as ch ->
            buf := (String.make 1 ch)::!buf ; ()
          | ch -> buf := ("%" ^(String.init 2 (fun shift ->
              let shift = shift * 4 in (* 0 or 4 *)
              alphanum.[ (Char.code ch land (0xf0 lsr shift))
                         lsr (4-shift) ] )))::!buf ; ()
        ) unenc in String.concat "" @@ List.rev !buf
    in
    String.concat "&" ["magnet?xt=urn:btih:" ^ info_hash_hex ;
                       "dn=" ^ urlencode display_name ;
                       "xl=" ^ Int64.to_string totalbytes ;
                       "tr=" ^ ( String.concat "&tr=" @@
                                 List.map urlencode announce_urls) ]
  in
  Logs.app (fun m -> m "%s" magnet_uri)



let do_pp () (* unit: setup_log*) torrent_path = (* TODO make streaming *)
  let benc = Bencode.decode (`File_path torrent_path) in
  Logs.app (fun m -> m "%a" Bencode.pp benc);
  Ok ()

open Cmdliner

let arg_torrent =
  Arg.(required & pos 0 (some string) None
       & info [] ~docv:"TORRENT")

let arg_display_name =
  (*Arg.(non_empty & pos_right 0 file [] & info [] ~docv:"FILES")*)
  Arg.(value & (opt (some string)) None
         & info ["name"] ~docv:"DISPLAY-NAME")

let arg_input_path =
  (*Arg.(non_empty & pos_right 0 file [] & info [] ~docv:"FILES")*)
  Arg.(required & pos 1 (some string) None
         & info [] ~docv:"INPUT")

let arg_piece_length : int Cmdliner.Term.t =
  let default_length = "2M" in
  let doc =
    {|Manually supply a piece length for the torrent to be created.
      The default length is |} ^ default_length ^
    {| and care should be taken when changing it:
      If you are looking to take advantage of piece deduplication across
      torrents, make sure they have the same piece length in addition to
       using file-aligned padding.|}
  in
  let length_parser : int Cmdliner.Arg.parser =
    fun length ->
      Scanf.sscanf length "%d%s"
        (fun num ->
           function
           |  "" -> R.ok num
           | ("m"|"M"|"MB"|"mb")      -> R.ok @@ num * 1024 * 1024
           | ("K"|"k"|"KB"|"kB"|"kb") -> R.ok @@ num * 1024
           | appendix -> R.error_msgf "Unknown appendix: %S" appendix
        )
      >>= ( function
          | num when num > 0 && num mod 16384 = 0 -> R.ok num
          | num -> R.error_msgf "%d must be a multiple of 16K." num )
      |> R.reword_error (fun (`Msg str) -> str)
      |> R.to_presult
  in
  Arg.(value & opt (length_parser, (fun fmt _ -> Format.fprintf fmt "LENGTH"))
         (match length_parser default_length with
          | `Ok default -> default
          | `Error err -> failwith @@ "default length is invalid: " ^ err)
       & info ["piece-length"] ~doc)


let arg_unaligned =
  let doc =
    {|By default $(mname) inserts padding files after files that
      do not fit \(align\) perfectly with the piece length.
      This enables deduplication within v1 torrents, and enables DHT peers that
      possess identical files from overlapping torrents to seed the files in
      your newly produced torrent, but does result in a slightly larger torrent
      file \(roughly TODO ~}
      ^ string_of_int ( 50 (* "4:attr" + "1:p" *) )^
    {|50 bytes per file\).
      Clients implementing BEP-0047 ignore these padding files, and they only
      incur minor metadata overhead \(no extra bandwidth is spent transferring
      their null content\).
      Passing this flag to $(mname) makes it refrain from inserting this
      padding, and is useful if your dataset contains millions of unique files
      that are not individually useful and thus unlikely to be shared in another
      torrent.
    |} in
  Arg.(value & flag & info ["unaligned"] ~doc)

let setup_log =
  let _setup_log (style_renderer:Fmt.style_renderer option) level : unit =
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level level ;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Term.(const _setup_log
        $ Fmt_cli.style_renderer ~docs:Manpage.s_common_options ()
        $ Logs_cli.level ~docs:Manpage.s_common_options ())


let cmdliner_info =
  Term.info ~version:(Manpage.escape "%%VERSION_NUM%%")
    ~sdocs:Manpage.s_common_options (* put --help and --version under that *)

let make_cmd =
  let doc = {| $(mname) is a commandline interface to the omgtorrent
               library. |} in
  let man = [] in
  Term.(term_result (const do_make $ setup_log
                     $ arg_unaligned
                     $ arg_piece_length
                     $ arg_torrent
                     $ arg_display_name
                     $ arg_input_path)),
  cmdliner_info "make" ~man ~doc

let pp_cmd =
  let doc = {| pretty-prints a torrent file|} in
  let man = [] in
  Term.(term_result (const do_pp $ setup_log
                     $ arg_torrent)),
  cmdliner_info "pp" ~man ~doc

let add_tracker_cmd =
  (* add "announce-list" entry if it doesn't already exist*)
  ()

let help_cmd =
  let doc = {| $(mname)HELP is a commandline interface to the omgtorrent
               library. |} in
  let man =
[
  `S "DESCRIPTION" ;
  `P {|$(mname) implements BEP-003 (BitTorrent v1) for the purpose of creating
       torrent files by indexing a set of directories.|} ;
  `S "USAGE" ;
  `P {|Note that you only have to type out a unique prefix for the subcommands.
       That means that $(mname) $(b,l) is an alias for
       $(mname) $(b,list-packets) ;
       That $(mname) $(b,v) is an alias for $(mname) $(b,verify) and so forth.|}
 ;`P {|The same is the case for options,
       so $(b,--rng) is an alias for $(b,--rng-seed) ;|} ;
  `Noblank ;
  `P {|$(mname) $(b,v) $(b,--sig) $(i,file.asc) is equivalent to
       $(mname) $(b,verify) $(b,--signature) $(i,file.asc) |} ;
  `S "EXAMPLES" ;
  `P "# $(mname) $(b,genkey --uid) 'Abbot Hoffman' $(b,>) abbie.priv" ;
  `P "# $(mname) $(b,sign --sk) abbie.priv MKULTRA.DOC $(b,>) MKULTRA.DOC.asc" ;
  `P "# $(mname) $(b,convert) abbie.priv $(b,>) abbie.pub" ;
  `P {|# $(mname) $(b,verify --sig) MKULTRA.DOC.asc $(b,--pk) abbie.pub
                   MKULTRA.DOC |} ; `Noblank ;
  `Pre {|opgp: [ERROR] Failed decoding ASCII armor ASCII public key block,
              parsing as raw instead|} ; `Noblank ;
  `P "Good signature!" ;
  `P {|# $(b,echo \$?) |}; `Noblank ;
  `P "0" ;
  `S Manpage.s_bugs;
  `P ( "Please report bugs on the issue tracker at "
     ^ "<https://github.com/cfcs/omgtorrent/issues>") ]
  in
  let help _ = `Help (`Pager, None) in
  Term.(ret (const help $ setup_log)),
  cmdliner_info "help" ~man ~doc


let cmds = [ help_cmd ; make_cmd ; pp_cmd ]

let () =
  Term.(exit @@ eval_choice help_cmd cmds)
