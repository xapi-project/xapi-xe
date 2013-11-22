open Pervasiveext

let try_read_string ?limit fd =
        let buf = Buffer.create 0 in
        let chunk = match limit with None -> 4096 | Some x -> x in
        let cache = String.make chunk '\000' in
        let finished = ref false in
        while not !finished do
                let to_read = match limit with
                        | Some x -> min (x - (Buffer.length buf)) chunk
                        | None -> chunk in
                let read_bytes = Unix.read fd cache 0 to_read in
                Buffer.add_substring buf cache 0 read_bytes;
                if read_bytes = 0 then finished := true
        done;
        Buffer.contents buf

let really_write fd string off n =
        let written = ref 0 in
        while !written < n
        do
                let wr = Unix.write fd string (off + !written) (n - !written) in
                written := wr + !written
        done

let string_of_signal x =
        let table = [
                Sys.sigabrt, "SIGABRT";
                Sys.sigalrm, "SIGALRM";
                Sys.sigfpe, "SIGFPE";
                Sys.sighup, "SIGHUP";
                Sys.sigill, "SIGILL";
                Sys.sigint, "SIGINT";
                Sys.sigkill, "SIGKILL";
                Sys.sigpipe, "SIGPIPE";
                Sys.sigquit, "SIGQUIT";
                Sys.sigsegv, "SIGSEGV";
                Sys.sigterm, "SIGTERM";
                Sys.sigusr1, "SIGUSR1";
                Sys.sigusr2, "SIGUSR2";
                Sys.sigchld, "SIGCHLD";
                Sys.sigcont, "SIGCONT";
                Sys.sigstop, "SIGSTOP";
                Sys.sigttin, "SIGTTIN";
                Sys.sigttou, "SIGTTOU";
                Sys.sigvtalrm, "SIGVTALRM";
                Sys.sigprof, "SIGPROF";
        ] in
        if List.mem_assoc x table
        then List.assoc x table
        else (Printf.sprintf "(ocaml signal %d with an unknown name)" x)

(** open a file, and make sure the close is always done *)
let with_file file mode perms f =
        let fd = Unix.openfile file mode perms in
        let r =
                try f fd
                with exn -> Unix.close fd; raise exn
                in
        Unix.close fd;
        r

(** [fd_blocks_fold block_size f start fd] folds [f] over blocks (strings)
    from the fd [fd] with initial value [start] *)
let fd_blocks_fold block_size f start fd = 
        let block = String.create block_size in
        let rec fold acc = 
                let n = Unix.read fd block 0 block_size in
                (* Consider making the interface explicitly use Substrings *)
                let s = if n = block_size then block else String.sub block 0 n in
                if n = 0 then acc else fold (f acc s) in
        fold start

let buffer_of_fd fd = 
        fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

let string_of_file file_path = Buffer.contents (buffer_of_file file_path)

let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x
let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x

exception Break

let lines_fold f start input =
        let accumulator = ref start in
        let running = ref true in
        while !running do
                let line =
                        try Some (input_line input)
                        with End_of_file -> None
                in
                match line with
                | Some line ->
                        begin
                                try accumulator := (f !accumulator line)
                                with Break -> running := false
                        end
                | None ->
                                running := false
        done;
        !accumulator

let lines_iter f = lines_fold (fun () line -> ignore(f line)) ()

(** open a file, and make sure the close is always done *)
let with_input_channel file f =
        let input = open_in file in
        do_finally
                (fun () -> f input)
                (fun () -> close_in input)

(** open a file, and make sure the close is always done *)
let with_file file mode perms f =
        let fd = Unix.openfile file mode perms in
        let r =
                try f fd
                with exn -> Unix.close fd; raise exn
                in
        Unix.close fd;
        r

let file_lines_fold f start file_path = with_input_channel file_path (lines_fold f start)


let file_lines_iter f = file_lines_fold (fun () line -> ignore(f line)) ()

let readfile_line = file_lines_iter

