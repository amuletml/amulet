open import "./../amulet/base.ml"
open import "./../amulet/option.ml"
open import "./../amulet/exception.ml"

type file_handle

instance show file_handle begin
  let show _ = "<file handle>"
end

external private val io_open :
  string -> string -> (file_handle -> 'a) -> (string -> 'a) -> 'a =
    "function(path, mode, cont, fail) \
       local h, err = io.open(path, mode) \
       if h then \
         return cont(h) \
       else \
         return fail(err) \
       end \
     end"

external private val io_read : file_handle -> 'a -> string =
  "function(h, s) return h:read(s) end"

external private val io_write : file_handle -> string -> file_handle =
  "function(h, s) return h:write(s) end"

external private val io_close : file_handle -> unit =
  "function(h) return h:close() end"

external private val io_flush : file_handle -> bool =
  "function(h) return h:close() end"

external private val wrap_nil : option 'a -> (string -> option 'a) -> ('a -> option 'a) -> (() -> 'a) -> option 'a =
  "function(none, left, some, k) \
     local ok, err = pcall(k) \
     if not ok then \
       return left(err) \
     elseif err == nil then \
       return none \
     else \
       return some(err) \
     end \
   end"

external private val io_stdout : file_handle = "io.stdout"
external private val io_stderr : file_handle = "io.stderr"
external private val io_stdin  : file_handle = "io.stdin"

type io_error = IoError of string
deriving instance typeable io_error

instance exception io_error begin
  let describe_exception (IoError x) = "Input/output error: " ^ x
end

type mode =
  | Read_m
  | Write_m
  | Append_m
  | ReadWrite_m
  | ReadAppend_m

instance show mode begin
  let show = function
    | Read_m       -> "r"
    | Write_m      -> "w"
    | Append_m     -> "a"
    | ReadWrite_m  -> "r+"
    | ReadAppend_m -> "a+"
end

let open_file path (mode : mode) =
  let m_str = show mode
  io_open path m_str
    (fun x -> x)
    (fun x -> throw (IoError x))

let open_for_reading path = open_file path Read_m

let maybe_open_file path mode =
  try_ (fun () -> open_file path mode)

let read_bytes file (n : int) =
  wrap_nil None (fun x -> throw (IoError x)) Some @@ fun _ -> io_read file n

let read_line file =
  wrap_nil None (fun x -> throw (IoError x)) Some @@ fun _ -> io_read file "*l"

let read_all file =
  match wrap_nil None (fun x -> throw (IoError x)) Some (fun _ -> io_read file "*a") with
  | Some "" -> None
  | x -> x

let write_bytes file str =
  let _ = wrap_nil None (fun x -> throw (IoError x)) Some @@ fun _ -> io_write file str
  ()

let close_file file =
  let _ = wrap_nil None (fun _ -> None) Some @@ fun _ -> io_close file
  ()

let flush_file file =
  let _ = wrap_nil None (fun _ -> None) Some @@ fun _ -> io_flush file
  ()

type handle = private Handle of ref file_handle

let standard_out = Handle (ref io_stdout)
let standard_err = Handle (ref io_stderr)
let standard_in  = Handle (ref io_stdin)

let file_of (Handle r) = !r

let reopen_file (Handle r) path mode =
  let old_h = !r
  r := open_file path mode
  old_h

let put_line str =
  let Handle r = standard_out
  write_bytes !r (str ^ "\n")

let put_bytes str =
  let Handle r = standard_out
  write_bytes !r str

let print x = put_line (show x)
