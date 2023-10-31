open Sys
open Unix

let force = ref false
let host = ref "127.0.0.1"
let port = ref 80
let benchtime = ref 30
let clients = ref 1
let failed = ref 2
let speed = ref 0
let bytes = ref 0
let display_version = ref false
let version = "1.0"
let url = ref ""
let path = ref "/"
let bufsize = 1024

let (fd_in, fd_out) = Unix.pipe()
let req = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

let timer_expired = ref false

let timeout_handle _ =
 timer_expired := true;;

let build_socket host port = 
  let inaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect sock inaddr; 
  Unix.setsockopt sock Unix.TCP_NODELAY false;
  sock;;

let rec get_bytes ic buf bufsize bytes_read =
  if !timer_expired 
    then bytes_read
  else  
    let n = input ic buf 0 bufsize in
    if n < bufsize then bytes_read + n
    else get_bytes ic buf bufsize (bytes_read + n)

let rec benchcore host port req =
  if !timer_expired then
  begin
    let oc = Unix.out_channel_of_descr fd_out in
    Printf.fprintf oc "%i %i %i\n" !speed !failed !bytes;
    flush oc;
    Unix.close fd_out;
  end
  else
  let success_retry = fun () ->
    speed := !speed + 1; 
    benchcore host port req;
  in
  let failed_retry = fun () ->
    failed := !failed + 1;
    benchcore host port req;
  in
  try
    let sock = build_socket host port in
    let ic = Unix.in_channel_of_descr sock in
    let oc = Unix.out_channel_of_descr sock in
    output_string oc req;
    flush oc;
    if !force 
      then (Unix.close sock; success_retry ())
    else
      let buf = Bytes.create bufsize in
      let n = get_bytes ic buf bufsize 0 in
      bytes := !bytes + n;
      Unix.close sock;
      success_retry ();
  with Unix_error _ ->
    failed_retry ();;
    

let bench_start host port req =
  set_signal sigalrm (Signal_handle timeout_handle);
  ignore (alarm !benchtime);
  Unix.close fd_in;
  benchcore host port req;;

let display_bench ()=
  let ic = Unix.in_channel_of_descr fd_in in
  let in_c = Scanf.Scanning.from_channel ic in  
  let sc s f b =
    speed := !speed + s;
    failed := !failed + f;
    bytes := !bytes + b;
  in
  for _ = 1 to !clients do
    Scanf.bscanf in_c "%d %d %d\n" sc;
  done;
  Unix.close fd_in;
  Unix.close fd_out;
  let pages = (!speed + !failed) / !benchtime in
  Printf.printf "Speed=%d pages/sec, %d bytes/sec.\nRequests: %d succeed, %d failed.\n" pages (!bytes / !benchtime) !speed !failed;;

let bench host port req =
  try 
    let s = build_socket host port in
    Unix.close s;
    let rec fork_client client =
      if client = 0 then display_bench ()
      else match Unix.fork () with
      | 0 -> bench_start host port req
      | _ -> fork_client (client - 1) in 
    fork_client !clients
  with Unix_error (e, _, _) ->
    print_endline (Unix.error_message e);;

let usage () = print_endline "webbench [option]... URL"


let build_request url =
  if not (String.starts_with ~prefix: "http://" url) then
  begin
    Printf.eprintf "\n:%s: is not a valid URL.\n" url;
    exit 2;
  end
  else 
  begin
    let s = String.length "http://" in
    if not (String.contains_from url s '/') then
    begin
      Printf.eprintf "\nInvalid URL syntax - hostname don't ends with '/'.\n";
      ignore (exit 2);
    end
    else
    begin  
      let host_end = String.index_from url s '/' in
      let host_port = String.sub url s (host_end - s) in
      if String.contains_from host_port 0 ':' then
        begin
          let end_index = String.index_from host_port 0 ':' in 
          host := String.sub host_port 0 end_index;
          let port_length = String.length host_port - end_index - 1 in
          let port_str = String.sub host_port (end_index + 1) port_length in
          port := Int32.(to_int (of_string port_str))
        end
      else
        host := host_port;
      let path_str = String.sub url host_end (String.length url - host_end) in
        path := path_str;
      
     let request = "GET " ^ path_str ^ " HTTP/1.1\r\nHost: " 
        ^ !host ^"\r\n"
        ^ "User-Agent: WebBench " ^ version ^ "\r\n\r\n"in
        print_endline request;
    end  
  end
   


let () =
  Arg.parse
    (Arg.align
      [
      "-f", Arg.Set force, " Don't wait for repyl from server.";
      "-t", Arg.Set_int benchtime, " Run benchmark for <sec> seconds. Default 30.";
      "-c", Arg.Set_int clients, " Run <n> HTTP clients at once. Default one.";
      "-p", Arg.Set_int port, " port to connect to";
      "-V", Arg.Set display_version, " Display program version.";
        ( "--alarm",
          Arg.Int (fun i -> Unix.alarm i |> ignore),
          " set alarm (in seconds)" );
      ])
    (fun s -> url := s)
    "webbench [option]... URL";
  if !display_version then
    begin
    print_endline version;
    ignore (exit 0);
    end;
  
  if (String.length !url = 0) then
    begin
    print_endline "webbench: Missing URL!";
    usage ();
    ignore (exit 2);
    end;

  Printf.printf "Webbench - Simple Web Benchmark %s\n" version;
  build_request !url;
  let msg = "Benchmarking: GET " 
    ^ !host ^ ":" ^ (Int.to_string !port) ^ "\n"
    ^ (Int.to_string !clients) ^ " client, running " ^(Int.to_string !benchtime) ^ " sec." in
  print_endline msg;
  try 
    bench !host !port req;
  with Sys_error e ->
    Printf.eprintf "err: %s\n" e;
