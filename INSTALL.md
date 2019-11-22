## Installation Instructions

### Installing OPAM packages
You will need to install the following opam packages:
- oUnit
- ounit-lwt
- Unix
- ANSITerminal
- lwt
- lwt.unix
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
Run `make term`. This creates a chat client that connects to any server run on
the local computer.
Alternatively, a server is already hosted online. To connect to this server, run
`make remote`.
