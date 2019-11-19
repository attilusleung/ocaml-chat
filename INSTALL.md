## Installation Instructions

### Installing OPAM packages
Run the command `opam install oUnit ounit-lwt ANSITerminal lwt lwt_ppx` in the command line. The following OPAM packages will be installed:
- oUnit
- ounit-lwt
- ANSITerminal
- lwt
- lwt_ppx

### Running the chatroom
NOTE: There is currently a known issue with running the client on WSL (because
WSL doesn't handle unix sockets properly). While the code will work, attempting
to run the code without an active server will cause the program to crash.
Therefore, the server should be run before the client on WSL systems. This is
not an issue on linux, and the client will continuously try and connect to the
server until it can connect successfully. Honestly, I should probably dockerize
this.

#### Running the server
Before running the server, please make sure the port 9000 on the system is not
currently in use.
Run `make serve`.

#### Running the client
Run `make term`.
