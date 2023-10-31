
## Overview Webbench_ml
webbench_ml is the OCaml implementation of [webbench](http://home.tiscali.cz/~cz210552/webbench.html)

### Web Bench
Web Bench is very simple tool for benchmarking WWW or proxy servers. Uses fork() for simulating multiple clients and can use HTTP/0.9-HTTP/1.1 requests. This benchmark is not very realistic, but it can test if your HTTPD can realy handle that many clients at once (try to run some CGIs) without taking your machine down. Displays pages/min and bytes/sec. Can be used in more aggressive mode with -f switch.

## Build
```bash
$ eval `opam env`
$ dune build
$ dune exec -- webbench -t 10 -c 20  http://127.0.0.1:8000/
```
